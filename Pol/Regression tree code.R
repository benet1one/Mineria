# Load packages
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

#Load data
source("KPI.R")
songs <- readRDS("data/songs_outlied.RDS")

set.seed(124)
train <- songs |> slice_sample(prop = 0.7)
test  <- songs |> filter(!is.element(ID, train$ID))

formula <- (
    song_popularity ~ 
        liveness + loudness + danceability + song_duration_ms + tempo +
        time_signature + audio_valence + energy +
        acousticness + instrumentalness + speechiness
)

#Fit tree

set.seed(123)

songs_tree <- rpart(
    formula,
    data = train,
    method = "anova",
    control = rpart.control(
        cp = 0.0001,      
        minsplit = 5,
        minbucket = 2,
        maxdepth = 30,
        xval = 20          
    )
)

# Show CP table
printcp(songs_tree)
plotcp(songs_tree)

# Best CP
best_cp <- songs_tree$cptable[which.min(songs_tree$cptable[,"xerror"]), "CP"]
cat("\nBest CP:", best_cp, "\n")


# Prune tree using optimal CP
songs_pruned <- prune(songs_tree, cp = best_cp)

# Visualize pruned tree
rpart.plot(
    songs_pruned,
    type = 2,
    extra = 101,
    under = TRUE,
    faclen = 0,
    varlen = 0,
    main = "Optimized Pruned Regression Tree"
)


# Predictions
train$pred <- predict(songs_pruned, newdata = train)
test$pred  <- predict(songs_pruned, newdata = test)


# Performance Metrics
train_metrics <- data.frame(
    RMSE = RMSE(train$pred, train$song_popularity),
    MAE  = MAE(train$pred, train$song_popularity),
    Rsq  = R2(train$pred, train$song_popularity)
)

test_metrics <- data.frame(
    RMSE = RMSE(test$pred, test$song_popularity),
    MAE  = MAE(test$pred, test$song_popularity),
    Rsq  = R2(test$pred, test$song_popularity)
)

cat("TRAINING PERFORMANCE \n")
print(train_metrics)
cat("TEST PERFORMANCE \n")
print(test_metrics)


# Variable Importance
importance <- songs_pruned$variable.importance |> sort(TRUE)
barplot(importance, las=2, col="steelblue",
        main="Variable Importance (Optimized Tree)")
