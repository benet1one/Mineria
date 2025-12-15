library(dplyr)

# =============================
# Utility
# =============================
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# =============================
# Load data
# =============================
songs <- readRDS("data/songs_all_imputed.RDS")

songs <- songs |>
  select(
    ID,
    training,
    liveness, danceability, audio_valence,
    energy, acousticness, speechiness,
    song_popularity
  ) |>
  mutate(
    fold = factor(rep_len(1:3, n()))
  )

# =============================
# Results storage
# =============================
rmse_glm <- numeric(3)
rmse_baseline <- numeric(3)

set.seed(123)

# =============================
# Cross-validation loop
# =============================
for (i in seq_along(levels(songs$fold))) {
  
  f <- levels(songs$fold)[i]
  cat("\n========================")
  cat("\nFold", f)
  cat("\n========================\n")
  
  # ----- Split (NO training filter here) -----
  train <- songs |> filter(fold != f)
  test  <- songs |> filter(fold == f)
  
  # ----- Scaling ONLY on train -----
  vars <- c(
    "liveness","danceability","audio_valence",
    "energy","acousticness","speechiness"
  )
  
  for (v in vars) {
    mu <- mean(train[[v]])
    sdv <- sd(train[[v]])
    
    train[[v]] <- (train[[v]] - mu) / sdv
    test[[v]]  <- (test[[v]]  - mu) / sdv
  }
  
  # =============================
  # GLM Gaussian
  # =============================
  model_glm <- glm(
    song_popularity ~ liveness + danceability + audio_valence +
      energy + acousticness + speechiness,
    data = train,
    family = gaussian()
  )
  
  pred_glm <- predict(model_glm, newdata = test)
  
  # =============================
  # Baseline (mean predictor)
  # =============================
  pred_base <- mean(train$song_popularity)
  
  # =============================
  # Diagnostics (CRITICAL)
  # =============================
  cat("Var(pred GLM):", var(pred_glm), "\n")
  cat("Var(Y test):", var(test$song_popularity), "\n")
  
  # =============================
  # Metrics
  # =============================
  rmse_glm[i] <- rmse(test$song_popularity, pred_glm)
  rmse_baseline[i] <- rmse(test$song_popularity, pred_base)
  
  cat("RMSE GLM:", rmse_glm[i], "\n")
  cat("RMSE Baseline:", rmse_baseline[i], "\n")
}

# =============================
# Summary
# =============================
cat("\n========================")
cat("\nSUMMARY")
cat("\n========================\n")

cat("Avg RMSE GLM:", mean(rmse_glm), "\n")
cat("Avg RMSE Baseline:", mean(rmse_baseline), "\n")
cat("Improvement:", mean(rmse_baseline) - mean(rmse_glm), "\n")
