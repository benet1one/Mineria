library(dplyr)
library(e1071)
library(caret)
library(ggplot2)
setwd("D:/Iker/VSC-Python/Mineria")
source("KPI.R")

# 1. CARGA DE DATOS

songs <- readRDS("data/songs_outlied.RDS")

set.seed(1234)
train <- songs |> slice_sample(prop = 0.7)
test  <- songs |> filter(!is.element(ID, train$ID))

# 2. SELECCIÓN DE PREDICTORES Y NORMALIZACIÓN
formula <- (
  song_popularity ~ 
    liveness + loudness + danceability + song_duration_ms + tempo +
    time_signature + audio_valence + energy +
    acousticness + instrumentalness + speechiness
)

# Normalización min-max (SVM lo necesita)

predictoras <- c(
  "liveness", "loudness", "danceability", "song_duration_ms",
  "tempo", "time_signature", "audio_valence", "energy",
  "acousticness", "instrumentalness", "speechiness"
)

normalizer <- preProcess(train[, predictoras], method = c("range"))

train_norm <- predict(normalizer, train[, predictoras])
test_norm  <- predict(normalizer, test[, predictoras])
songs_norm <- predict(normalizer, songs[, predictoras])
test_kaggle <- predict(normalizer, songs_test[, predictoras])

train_norm$song_popularity <- train$song_popularity
train_norm$outlier_weight   <- train$outlier_weight

train_clean <- train_norm %>% filter(train$outlier_weight == 1)

# 3. ENTRENAMIENTO SVM BASE (sin tuning)

svm_base <- svm(
  formula,
  data  = train_clean,
  kernel = "radial",
  cost   = 1,
  gamma  = 0.1
)

# Predicciones iniciales
pred_train_base <- predict(svm_base, train_norm)
pred_test_base  <- predict(svm_base,  test_norm)

# Métricas base
rmse_train_base <- rmse(train$song_popularity, pred_train_base)
rmse_test_base  <- rmse(test$song_popularity,  pred_test_base)
mape_train_base <- mape(train$song_popularity, pred_train_base)
mape_test_base  <- mape(test$song_popularity,  pred_test_base)

print("Resultados modelo base:")
print(c(rmse_train_base, rmse_test_base, mape_train_base, mape_test_base))
# Resultados esperables cuando no hay tuning.

# 4. TUNING CON VALIDACIÓN CRUZADA

set.seed(1234)

grid_cost  <- c(0.3, 1, 3)
grid_gamma <- c(0.01, 0.05, 0.1)

svm_tuned <- tune(
  svm,
  formula,
  data = train_clean,
  kernel = "radial",
  ranges = list(
    cost  = grid_cost,
    gamma = grid_gamma
  ),
  tunecontrol = tune.control(sampling = "cross", cross = 5)
)

summary(svm_tuned)
best_svm <- svm_tuned$best.model
best_svm

# 5. ENTRENAMIENTO FINAL DEL MODELO

svm_final <- svm(
  formula,
  data = train_norm,
  kernel = "radial",
  cost   = best_svm$cost,
  gamma  = best_svm$gamma
)

# 6. METRICAS INTERNAS (TRAIN / TEST)

pred_train <- predict(svm_final, train_norm)
pred_test  <- predict(svm_final, test_norm)

rmse_train <- rmse(train$song_popularity, pred_train)
rmse_test  <- rmse(test$song_popularity,  pred_test)
mape_train <- mape(train$song_popularity, pred_train)
mape_test  <- mape(test$song_popularity,  pred_test)

metricas <- tibble(
  dataset = c("train","test"),
  RMSE = c(rmse_train, rmse_test),
  MAPE = c(mape_train, mape_test)
)

print(metricas)

# 7. GRÁFICO DIAGNÓSTICO

ggplot(
  tibble(real = test$song_popularity, pred = pred_test),
  aes(x = real, y = pred)
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal() +
  labs(
    title = "SVM – Real vs Predicho (Test)",
    x = "Real",
    y = "Predicho"
  )

# 8. ENTRENAMIENTO FINAL PARA SUBMISSION (TRAIN + TEST ORIGINAL)

songs_test <- readRDS("data/songs_test_imputed.RDS")

# Normalizar ambos datasets con el mismo normalizer
songs_norm  <- predict(normalizer, songs)
test_kaggle <- predict(normalizer, songs_test)

# Entrenar modelo definitivo
svm_submission <- svm(
  formula,
  data   = songs_norm,
  kernel = "radial",
  cost   = best_svm$cost,
  gamma  = best_svm$gamma
)

# Predicción sobre test Kaggle
pred_kaggle <- predict(svm_submission, test_kaggle)

submission <- tibble(
  ID = songs_test$ID,
  song_popularity = round(pred_kaggle)
)

write.csv(
  submission,
  "predictions/svm.csv",
  row.names = FALSE
)

