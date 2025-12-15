library(dplyr)
library(xgboost)
library(Matrix)

set.seed(123)

# =============================
# Load data
# =============================
songs <- readRDS("data/songs_outlied.RDS")

# =============================
# Feature engineering
# =============================
songs <- songs %>%
  mutate(
    energy_dance = energy * danceability,
    mood = audio_valence * energy,
    acoustic_speech = acousticness * speechiness,
    rhythmic = danceability / (tempo + 1),
    loud_energy = loudness * energy,
    y_log = log1p(song_popularity)
  )

# =============================
# Train/test split (80/20)
# =============================
train_idx <- sample(seq_len(nrow(songs)), size = 0.8 * nrow(songs))
train <- songs[train_idx, ]
test  <- songs[-train_idx, ]

# =============================
# Prepare matrices for XGBoost
# =============================
feature_names <- c("liveness", "loudness", "danceability", "song_duration_ms", "tempo",
                   "time_signature", "audio_valence", "energy",
                   "acousticness", "instrumentalness", "speechiness",
                   "energy_dance", "mood", "acoustic_speech", "rhythmic", "loud_energy")

X_train <- train[, feature_names] %>% mutate(across(everything(), as.numeric)) %>% as.matrix()
y_train <- train$y_log

X_test <- test[, feature_names] %>% mutate(across(everything(), as.numeric)) %>% as.matrix()
y_test <- test$y_log

dtrain <- xgb.DMatrix(data = X_train, label = y_train, weight = train$outlier_weight)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test, weight = test$outlier_weight)

# =============================
# XGBoost parameters
# =============================
params <- list(
  objective = "reg:squarederror",
  eta = 0.03,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  gamma = 0
)

# =============================
# 5-fold Cross-validation
# =============================
cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = 50,
  metrics = "rmse",
  maximize = FALSE
)

cat("Best nrounds:", cv$best_iteration, "\n")

# =============================
# Train final model with best nrounds
# =============================
model_xgb <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = cv$best_iteration,
  watchlist = list(train = dtrain, eval = dtest),
  early_stopping_rounds = 50,
  print_every_n = 50
)

# =============================
# Predictions and RMSE on original scale
# =============================
pred_log <- predict(model_xgb, dtest)
pred <- expm1(pred_log)  # back to original scale

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
cat("XGBoost RMSE (test set):", rmse(test$song_popularity, pred), "\n")

# =============================
# Feature importance
# =============================
importance_matrix <- xgb.importance(feature_names = feature_names, model = model_xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix)

    