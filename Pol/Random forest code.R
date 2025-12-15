library(ranger)
library(caret)
library(dplyr)

# Random Forest Model

set.seed(123)
songs <- readRDS("data/songs_outlied.RDS")
songs_test <- readRDS("data/songs_test_imputed.RDS") |> 
    mutate(song_popularity = NA)
    
songs_all <- bind_rows(songs, songs_test) |> 
    mutate(
        # Reduce skew
        song_duration_ms = log1p(song_duration_ms),
        tempo = log1p(tempo)
    )

songs_train <- songs_all |> 
    filter(!is.na(song_popularity)) |> 
    filter(outlier_weight == 1)

formula <- (
    song_popularity ~ 0
    + liveness + loudness + danceability + song_duration_ms + tempo
    + time_signature + audio_valence + energy 
    + acousticness + instrumentalness + speechiness
)

rf_model <- ranger(
    formula = formula,
    data = songs_train,
    num.trees = 1500,
    mtry = 3,                
    min.node.size = 3,  
    splitrule = "variance",
    replace = FALSE,
    sample.fraction = 0.7,   
    importance = "permutation",
    respect.unordered.factors = "order",
)

# Variable importance
vi <- rf_model$variable.importance
barplot(sort(vi, TRUE), las=2, col="darkgreen",
        main="Random Forest Variable Importance")


# Save prediction
predictions <- tibble(
    id = songs_test$ID,
    song_popularity = predict(rf_model, songs_test) $ predictions |> round(4)
)

write.csv(predictions, file = "predictions/rf.csv", row.names = FALSE)


## Old Code

# Predictions
train$rf_pred <- predict(rf_model, train)$predictions
test$rf_pred  <- predict(rf_model, test)$predictions

# Metrics
rf_train <- data.frame(
    RMSE = RMSE(train$rf_pred, train$song_popularity),
    MAE  = MAE(train$rf_pred, train$song_popularity),
    R2   = R2(train$rf_pred,  train$song_popularity)
)

rf_test <- data.frame(
    RMSE = RMSE(test$rf_pred, test$song_popularity),
    MAE  = MAE(test$rf_pred, test$song_popularity),
    R2   = R2(test$rf_pred,   test$song_popularity)
)

cat("RANDOM FOREST TRAINING PERFORMANCE \n")
print(rf_train)
cat("RANDOM FOREST TEST PERFORMANCE \n")
print(rf_test)

