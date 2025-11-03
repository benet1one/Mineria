##############################################################
#  MARKET BASKET ANALYSIS (MBA) – BASE DE DATOS DE CANCIONES
##############################################################

#-------------------------------------------------------------
# 1. Cargar librerías necesarias
#-------------------------------------------------------------

# install.packages("arules")
# install.packages("arulesViz")
# install.packages("tidyverse")
# install.packages("FactoMineR")
library(arules)
library(arulesViz)
library(tidyverse)
library(FactoMineR)

#-------------------------------------------------------------
# 2. Lectura de la base de datos
#-------------------------------------------------------------
songs <- readRDS("data/songs_outlied.rds")

# Exploración inicial
dim(songs)
str(songs)
summary(songs)
head(songs)

#-------------------------------------------------------------
# 3. Preprocesamiento de datos
#-------------------------------------------------------------
# Las reglas de asociación requieren variables categóricas.
# Discretizamos las variables numéricas en categorías (bajo, medio, alto)

numeric_cols <- c("liveness", "loudness", "danceability", "song_duration_ms",
                  "audio_valence", "energy", "tempo", "acousticness",
                  "speechiness", "instrumentalness", "song_popularity")

songs_disc <- songs

for (col in numeric_cols) {
    songs_disc[[col]] <- discretize(songs[[col]],
                                    method = "frequency",
                                    categories = 3,
                                    labels = c("Low", "Medium", "High"))
}

# Convertimos todo en factor
songs_disc <- songs_disc %>%
    mutate(across(everything(), as.factor))

str(songs_disc)
summary(songs_disc)

#-------------------------------------------------------------
# 4. Transformación a base transaccional
#-------------------------------------------------------------
ttr2 <- as(songs_disc, "transactions")

# Resumen de la base transaccional
summary(ttr2)
inspect(ttr2[1:5])

#-------------------------------------------------------------
# 5. Análisis descriptivo de ítems
#-------------------------------------------------------------
# Frecuencia de aparición de atributos
itemFrequencyPlot(ttr2, topN = 20, type = "absolute", cex.names = 0.7)
itemFrequencyPlot(ttr2, support = 0.05, cex.names = 0.7)

#-------------------------------------------------------------
# 6. Generación de reglas de asociación con Apriori
#-------------------------------------------------------------
# Seleccionamos parámetros balanceados
# min_support: frecuencia mínima (3%)
# min_confidence: fuerza mínima (70%)
# maxlen: longitud máxima de reglas

rules <- apriori(ttr2,
                 parameter = list(support = 0.03,
                                  confidence = 0.7,
                                  minlen = 2,
                                  maxlen = 5))

summary(rules)
inspect(head(sort(rules, by = "lift"), 10))

#-------------------------------------------------------------
# 7. Eliminación de reglas redundantes
#-------------------------------------------------------------
rules_non_redundant <- rules[!is.redundant(rules)]
summary(rules_non_redundant)
inspect(head(sort(rules_non_redundant, by = "lift"), 10))

#-------------------------------------------------------------
# 8. Visualización general de reglas
#-------------------------------------------------------------
plot(rules_non_redundant, measure = c("support", "lift"), shading = "confidence")
plot(rules_non_redundant, method = "grouped")
plot(rules_non_redundant, method = "graph", control = list(type = "items"))

#-------------------------------------------------------------
# 9. Exportar reglas a CSV
#-------------------------------------------------------------
write(rules_non_redundant, file = "rules_songs_general.csv", sep = ",", col.names = NA)

##############################################################
#         SEGUNDA PARTE: ANÁLISIS FOCALIZADO EN POPULARIDAD
##############################################################

#-------------------------------------------------------------
# 10. Filtrar reglas con RHS = song_popularity=High
#-------------------------------------------------------------
rules_popular <- subset(rules_non_redundant, subset = rhs %in% "song_popularity=High")

summary(rules_popular)
inspect(head(sort(rules_popular, by = "lift"), 10))

#-------------------------------------------------------------
# 11. Eliminar redundancias dentro del subconjunto de popularidad
#-------------------------------------------------------------
rules_popular_nr <- rules_popular[!is.redundant(rules_popular)]
summary(rules_popular_nr)

#-------------------------------------------------------------
# 12. Visualización específica de reglas de popularidad
#-------------------------------------------------------------
plot(rules_popular_nr, measure = c("support", "lift"), shading = "confidence")
plot(rules_popular_nr, method = "grouped")
plot(rules_popular_nr, method = "graph", control = list(type = "items"))

#-------------------------------------------------------------
# 13. Exportar reglas de popularidad a CSV
#-------------------------------------------------------------
write(rules_popular_nr, file = "rules_song_popularity_high.csv", sep = ",", col.names = NA)

#-------------------------------------------------------------
# 14. Interpretación de resultados
#-------------------------------------------------------------
# Ejemplo de interpretación:
# Si encontramos reglas como:
#   energy=High, danceability=High  => song_popularity=High
# con lift = 1.7, significa que las canciones con alta energía y bailabilidad
# tienen 1.7 veces más probabilidad de ser populares de lo esperado al azar.

#-------------------------------------------------------------
# 15. Ajuste de parámetros para exploración
#-------------------------------------------------------------
# Si se desean más reglas, se puede reducir el soporte mínimo:
# rules_alt <- apriori(ttr2, parameter = list(support = 0.01, confidence = 0.6, maxlen = 5))
# Si se quieren solo reglas muy fuertes, aumentar el confidence:
# rules_strict <- apriori(ttr2, parameter = list(support = 0.05, confidence = 0.8, maxlen = 5))
# Luego volver a aplicar el filtro de popularidad y visualización.

##############################################################
#                   FIN DEL SCRIPT
##############################################################
