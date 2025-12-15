library(dplyr)
library(randomForest)

# =============================
# Utility
# =============================
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

set.seed(123)

# =============================
# Load data
# =============================
songs <- readRDS("data/songs_outlied.RDS")

songs <- songs |>
  select(
    ID,
    liveness, danceability, audio_valence,
    energy, acousticness, speechiness,
    song_popularity
  ) |>
  mutate(
    fold = factor(rep_len(1:3, n()))
  )

# =============================
# Storage
# =============================
rmse_rf <- numeric(3)
rmse_baseline <- numeric(3)

# =============================
# Cross-validation
# =============================
for (i in seq_along(levels(songs$fold))) {
  
  f <- levels(songs$fold)[i]
  cat("\n========================")
  cat("\nFold", f)
  cat("\n========================\n")
  
  # ----- Split -----
  train <- songs |> filter(fold != f)
  test  <- songs |> filter(fold == f)
  
  # =============================
  # Log-transform target
  # =============================
  train$y_log <- log1p(train$song_popularity)
  test$y_log  <- log1p(test$song_popularity)
  
  # =============================
  # Random Forest (train on y_log)
  # =============================
  model_rf <- randomForest(
    y_log ~ liveness + danceability + audio_valence +
      energy + acousticness + speechiness,
    data = train,
    ntree = 500,
    mtry = 3,
    importance = TRUE
  )
  
  # =============================
  # Prediction
  # =============================
  pred_log <- predict(model_rf, test)
  pred <- expm1(pred_log)
  
  # =============================
  # Baseline
  # =============================
  pred_base <- mean(train$song_popularity)
  
  # =============================
  # Diagnostics
  # =============================
  cat("Var(pred RF):", var(pred), "\n")
  cat("RMSE Baseline:", rmse(test$song_popularity, pred_base), "\n")
  
  # =============================
  # Metrics
  # =============================
  rmse_rf[i] <- rmse(test$song_popularity, pred)
  rmse_baseline[i] <- rmse(test$song_popularity, pred_base)
  
  cat("RMSE RF:", rmse_rf[i], "\n")
}

# =============================
# Summary
# =============================
cat("\n========================")
cat("\nSUMMARY (AVG)")
cat("\n========================\n")

cat("Baseline RMSE:", mean(rmse_baseline), "\n")
cat("RF RMSE:", mean(rmse_rf), "\n")
cat("Improvement:", mean(rmse_baseline) - mean(rmse_rf), "\n")

# =============================
# Train final RF on all data
# =============================
songs$y_log <- log1p(songs$song_popularity)

final_rf <- randomForest(
  y_log ~ liveness + danceability + audio_valence +
    energy + acousticness + speechiness,
  data = songs,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

cat("\n========================")
cat("\nFINAL MODEL IMPORTANCE")
cat("\n========================\n")

print(importance(final_rf))
varImpPlot(final_rf)
