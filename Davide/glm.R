library(dplyr)
source("KPI.R")

# Load data
songs <- readRDS("data/songs_all_imputed.RDS")

# Add RMSE function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Prepare dataset with the same variables as your previous models
songs_glm <- songs |>
  select(ID, training, liveness, danceability, audio_valence, energy, 
         acousticness, speechiness, song_popularity) |>
  mutate(fold = rep_len(1:3, n()) |> factor())

# Initialize results storage
results <- list()

set.seed(123)

for (f in levels(songs_glm$fold)) {
  cat("\n========================================")
  cat("\nFold", f)
  cat("\n========================================\n")
  
  # Split data
  train <- songs_glm |>
    filter(fold != f) |>
    filter(training)
  
  test <- songs_glm |>
    filter(fold == f) |>
    filter(training)
  
  # ===== Model 1: GLM with Gaussian family (Linear Regression) =====
  cat("\n--- GLM Model 1: Gaussian (Linear) ---\n")
  
  model_gaussian <- glm(
    song_popularity ~ liveness + danceability + audio_valence + 
      energy + acousticness + speechiness,
    data = train,
    family = gaussian()
  )
  
  predicted_gaussian <- predict(model_gaussian, newdata = test)
  predicted_gaussian <- pmax(0, pmin(predicted_gaussian, 4))  # Clip to [0,4]
  
  # ===== Model 2: GLM with Gamma family (for positive continuous data) =====
  cat("\n--- GLM Model 2: Gamma ---\n")
  
  # Gamma requires positive values, so add small constant if needed
  train_gamma <- train |> mutate(song_popularity = song_popularity + 0.1)
  
  model_gamma <- glm(
    song_popularity ~ liveness + danceability + audio_valence + 
      energy + acousticness + speechiness,
    data = train_gamma,
    family = Gamma(link = "log")
  )
  
  predicted_gamma <- predict(model_gamma, newdata = test, type = "response") - 0.1
  predicted_gamma <- pmax(0, pmin(predicted_gamma, 4))
  
  # ===== Model 3: GLM with interaction terms =====
  cat("\n--- GLM Model 3: Gaussian with Interactions ---\n")
  
  model_interactions <- glm(
    song_popularity ~ (liveness + danceability + audio_valence + energy + 
                         acousticness + speechiness)^2,
    data = train,
    family = gaussian()
  )
  
  predicted_interactions <- predict(model_interactions, newdata = test)
  predicted_interactions <- pmax(0, pmin(predicted_interactions, 4))
  
  # ===== Calculate baseline (worst case) =====
  mape_mean <- minimize_weighted_mape(draws = train$song_popularity)
  worst_mape <- weighted_mape(actual = test$song_popularity, predicted = mape_mean)
  worst_rmse <- rmse(actual = test$song_popularity, predicted = mape_mean)
  
  # ===== Evaluate all models - MAPE =====
  mape_gaussian <- weighted_mape(actual = test$song_popularity, predicted = predicted_gaussian)
  mape_gamma <- weighted_mape(actual = test$song_popularity, predicted = predicted_gamma)
  mape_interactions <- weighted_mape(actual = test$song_popularity, predicted = predicted_interactions)
  
  # ===== Evaluate all models - RMSE =====
  rmse_gaussian <- rmse(actual = test$song_popularity, predicted = predicted_gaussian)
  rmse_gamma <- rmse(actual = test$song_popularity, predicted = predicted_gamma)
  rmse_interactions <- rmse(actual = test$song_popularity, predicted = predicted_interactions)
  
  # Print results
  cat("\n--- BASELINE (Constant Prediction) ---")
  cat("\nMAPE =", round(worst_mape, 4))
  cat("\nRMSE =", round(worst_rmse, 4))
  
  cat("\n\n--- GLM GAUSSIAN ---")
  cat("\nMAPE =", round(mape_gaussian, 4), "| Improvement =", round(worst_mape - mape_gaussian, 4))
  cat("\nRMSE =", round(rmse_gaussian, 4), "| Improvement =", round(worst_rmse - rmse_gaussian, 4))
  
  cat("\n\n--- GLM GAMMA ---")
  cat("\nMAPE =", round(mape_gamma, 4), "| Improvement =", round(worst_mape - mape_gamma, 4))
  cat("\nRMSE =", round(rmse_gamma, 4), "| Improvement =", round(worst_rmse - rmse_gamma, 4))
  
  cat("\n\n--- GLM INTERACTIONS ---")
  cat("\nMAPE =", round(mape_interactions, 4), "| Improvement =", round(worst_mape - mape_interactions, 4))
  cat("\nRMSE =", round(rmse_interactions, 4), "| Improvement =", round(worst_rmse - rmse_interactions, 4))
  cat("\n")
  
  # Store results
  results[[f]] <- list(
    fold = f,
    baseline = list(mape = worst_mape, rmse = worst_rmse),
    gaussian = list(
      mape = mape_gaussian, 
      rmse = rmse_gaussian,
      mape_imp = worst_mape - mape_gaussian,
      rmse_imp = worst_rmse - rmse_gaussian
    ),
    gamma = list(
      mape = mape_gamma, 
      rmse = rmse_gamma,
      mape_imp = worst_mape - mape_gamma,
      rmse_imp = worst_rmse - rmse_gamma
    ),
    interactions = list(
      mape = mape_interactions, 
      rmse = rmse_interactions,
      mape_imp = worst_mape - mape_interactions,
      rmse_imp = worst_rmse - rmse_interactions
    )
  )
}

# ===== Summary across all folds =====
cat("\n========================================")
cat("\n         SUMMARY ACROSS FOLDS")
cat("\n========================================\n")

# Calculate averages
avg_baseline_mape <- mean(sapply(results, function(r) r$baseline$mape))
avg_baseline_rmse <- mean(sapply(results, function(r) r$baseline$rmse))

avg_gaussian_mape <- mean(sapply(results, function(r) r$gaussian$mape))
avg_gaussian_rmse <- mean(sapply(results, function(r) r$gaussian$rmse))

avg_gamma_mape <- mean(sapply(results, function(r) r$gamma$mape))
avg_gamma_rmse <- mean(sapply(results, function(r) r$gamma$rmse))

avg_interactions_mape <- mean(sapply(results, function(r) r$interactions$mape))
avg_interactions_rmse <- mean(sapply(results, function(r) r$interactions$rmse))

cat("\n--- BASELINE ---")
cat("\nAverage MAPE:", round(avg_baseline_mape, 4))
cat("\nAverage RMSE:", round(avg_baseline_rmse, 4))

cat("\n\n--- GLM GAUSSIAN ---")
cat("\nAverage MAPE:", round(avg_gaussian_mape, 4), 
    "| Improvement:", round(avg_baseline_mape - avg_gaussian_mape, 4))
cat("\nAverage RMSE:", round(avg_gaussian_rmse, 4), 
    "| Improvement:", round(avg_baseline_rmse - avg_gaussian_rmse, 4))

cat("\n\n--- GLM GAMMA ---")
cat("\nAverage MAPE:", round(avg_gamma_mape, 4), 
    "| Improvement:", round(avg_baseline_mape - avg_gamma_mape, 4))
cat("\nAverage RMSE:", round(avg_gamma_rmse, 4), 
    "| Improvement:", round(avg_baseline_rmse - avg_gamma_rmse, 4))

cat("\n\n--- GLM INTERACTIONS ---")
cat("\nAverage MAPE:", round(avg_interactions_mape, 4), 
    "| Improvement:", round(avg_baseline_mape - avg_interactions_mape, 4))
cat("\nAverage RMSE:", round(avg_interactions_rmse, 4), 
    "| Improvement:", round(avg_baseline_rmse - avg_interactions_rmse, 4))

# ===== Comparison table =====
cat("\n\n--- COMPARISON TABLE ---\n")
comparison <- data.frame(
  Model = c("Baseline", "GLM Gaussian", "GLM Gamma", "GLM Interactions"),
  Avg_MAPE = c(avg_baseline_mape, avg_gaussian_mape, avg_gamma_mape, avg_interactions_mape),
  Avg_RMSE = c(avg_baseline_rmse, avg_gaussian_rmse, avg_gamma_rmse, avg_interactions_rmse)
)
comparison <- comparison |>
  mutate(
    MAPE_Improvement = Avg_MAPE[1] - Avg_MAPE,
    RMSE_Improvement = Avg_RMSE[1] - Avg_RMSE
  )
print(round(comparison, 4))

# ===== Train final model on all training data =====
cat("\n\n========================================")
cat("\n      FINAL MODEL (All Training Data)")
cat("\n========================================\n")

train_all <- songs_glm |> filter(training)

final_model <- glm(
  song_popularity ~ liveness + danceability + audio_valence + 
    energy + acousticness + speechiness,
  data = train_all,
  family = gaussian()
)

cat("\nModel Summary:\n")
print(summary(final_model))

# Variable importance (based on standardized coefficients)
cat("\n\nVariable Importance (Standardized Coefficients):\n")
train_std <- train_all |>
  mutate(across(c(liveness, danceability, audio_valence, energy, 
                  acousticness, speechiness), scale))

final_model_std <- glm(
  song_popularity ~ liveness + danceability + audio_valence + 
    energy + acousticness + speechiness,
  data = train_std,
  family = gaussian()
)

coef_importance <- abs(coef(final_model_std)[-1])  # Remove intercept
coef_importance <- sort(coef_importance, decreasing = TRUE)
print(round(coef_importance, 4))

cat("\n\nGLM analysis complete!")
cat("\nBest model based on MAPE:", comparison$Model[which.min(comparison$Avg_MAPE)])
cat("\nBest model based on RMSE:", comparison$Model[which.min(comparison$Avg_RMSE)])