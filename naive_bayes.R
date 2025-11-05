# --- Librerie necessarie ---
library(dplyr)
library(e1071)
library(caret)
library(ggplot2)

# --- Importazione e pulizia ---
source("reading.R")
songs <- songs[complete.cases(songs), ]

# --- Trasformazione del target (da numerico a categoria) ---
# Usiamo i terzili per definire 3 classi di popolarità
songs$popularity_class <- cut(
  songs$song_popularity,
  breaks = quantile(songs$song_popularity, probs = c(0, 0.33, 0.66, 1)),
  labels = c("Bassa", "Media", "Alta"),
  include.lowest = TRUE
)

# --- Divisione train/test ---
set.seed(123)
idx <- sample(nrow(songs), 0.7 * nrow(songs))
train <- songs[idx, ]
test  <- songs[-idx, ]

# --- Selezione predittori (escludiamo ID e variabili target) ---
X_train <- train %>% select(-ID, -song_popularity, -popularity_class)
X_test  <- test  %>% select(-ID, -song_popularity, -popularity_class)
y_train <- train$popularity_class
y_test  <- test$popularity_class

# --- Conversione in numerico (necessario per Naive Bayes continuo) ---
X_train <- data.frame(lapply(X_train, function(x) as.numeric(as.character(x))))
X_test  <- data.frame(lapply(X_test,  function(x) as.numeric(as.character(x))))

# --- Costruzione del modello Naive Bayes ---
model_nb <- naiveBayes(x = X_train, y = y_train, laplace = 1)

# --- Predizioni ---
pred_nb <- predict(model_nb, X_test)

# --- Valutazione del modello ---
conf_mat <- confusionMatrix(pred_nb, y_test)
print(conf_mat)

# --- KPI principali ---
cat("\nAccuracy:", conf_mat$overall["Accuracy"], "\n")
cat("Kappa:", conf_mat$overall["Kappa"], "\n")

# --- Visualizzazione: matrice di confusione ---
ggplot(as.data.frame(conf_mat$table), aes(Reference, Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Matrice di confusione - Naive Bayes",
       x = "Reale", y = "Predetto") +
  theme_minimal()
