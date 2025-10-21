# Instalar paquetes si no están instalados
#install.packages(c("tidyverse", "caret", "randomForest", "Metrics"))

library(tidyverse)
library(caret)
library(randomForest)
library(Metrics)

# ==========================
# 1. CARGA DE DATOS
# ==========================
# Supongamos que tienes dos archivos: train.csv y test.csv
train <- read.csv("C:/Users/pol.tobella/Downloads/Mineria-master/Mineria-master/data/train.csv", stringsAsFactors = FALSE)
test  <- read.csv("C:/Users/pol.tobella/Downloads/Mineria-master/Mineria-master/data/test.csv", stringsAsFactors = FALSE)

# ==========================
# 2. INSPECCIÓN Y PREPROCESAMIENTO
# ==========================
# Revisar estructura
str(train)

# Eliminar columnas no necesarias (por ejemplo, ID)
train <- train %>% select(-ID)
test <- test %>% select(-ID)

# Convertir variables categóricas en factor
train$audio_mode <- as.factor(train$audio_mode)
train$key <- as.factor(train$key)
train$time_signature <- as.factor(train$time_signature)

test$audio_mode <- as.factor(test$audio_mode)
test$key <- as.factor(test$key)
test$time_signature <- as.factor(test$time_signature)

# ==========================
# 3. DIVIDIR TRAIN PARA VALIDACIÓN
# ==========================
set.seed(123)
train_index <- createDataPartition(train$song_popularity, p = 0.8, list = FALSE)
train_data <- train[train_index, ]
val_data   <- train[-train_index, ]

# ==========================
# 4. ENTRENAR MODELO RANDOM FOREST
# ==========================
# Definir fórmula (todas las variables excepto song_popularity como predictoras)
features <- setdiff(names(train_data), "song_popularity")
formula <- as.formula(paste("song_popularity ~", paste(features, collapse = " + ")))

set.seed(123)
rf_model <- randomForest(formula, data = train_data, ntree = 500, mtry = 5, importance = TRUE)

print(rf_model)
varImpPlot(rf_model)

# ==========================
# 5. VALIDACIÓN DEL MODELO
# ==========================
val_pred <- predict(rf_model, val_data)

# Calcular métricas
rmse_value <- rmse(val_data$song_popularity, val_pred)
mae_value <- mae(val_data$song_popularity, val_pred)

cat("📊 RMSE:", rmse_value, "\n")
cat("📊 MAE:", mae_value, "\n")

# ==========================
# 6. PREDICCIÓN SOBRE TEST
# ==========================
test_pred <- predict(rf_model, test)

# Agregar columna de predicciones a test
test$song_popularity <- test_pred

# ==========================
# 7. EXPORTAR RESULTADOS
# ==========================
write.csv(test, "test_predicciones.csv", row.names = FALSE)

cat("✅ Predicciones guardadas en 'test_predicciones.csv'\n")
