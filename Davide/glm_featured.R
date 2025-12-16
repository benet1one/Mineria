# =============================
# Libraries
# =============================
library(dplyr)

# =============================
# Set working directory & load data
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
    loud_energy = loudness * energy
  )

# =============================
# Train/test split
# =============================
set.seed(123)
train_idx <- sample(seq_len(nrow(songs)), size = 0.8 * nrow(songs))
train <- songs[train_idx, ]
test <- songs[-train_idx, ]

# =============================
# Optionally log-transform target
# =============================
train$y_log <- log1p(train$song_popularity)
test$y_log <- log1p(test$song_popularity)

# =============================
# Fit GLM
# =============================
glm_model <- glm(
  y_log ~ energy + danceability + audio_valence + acousticness + speechiness + loudness +
    energy_dance + mood + acoustic_speech + rhythmic + loud_energy,
  data = train,
  family = gaussian()
)

# =============================
# Predictions
# =============================
pred_log <- predict(glm_model, newdata = test)
pred <- expm1(pred_log)  # back to original scale

# =============================
# Evaluate RMSE
# =============================
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
rmse_score <- rmse(test$song_popularity, pred)
cat("GLM RMSE on test set:", rmse_score, "\n")

# =============================
# Model summary
# =============================
summary(glm_model)
