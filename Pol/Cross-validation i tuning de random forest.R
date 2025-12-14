    # Random Forest Regression with ranger

    library(dplyr)
    library(ranger)
    library(caret)
    songs <- readRDS("data/songs_outlied.RDS")
    
    set.seed(123)
    
    train <- songs |> slice_sample(prop = 0.7)
    test <- songs |> filter(!is.element(ID, train$ID))
    
    formula <- (
        song_popularity ~ 0
        + liveness
        + loudness
        + danceability
        + song_duration_ms
        + tempo
        + time_signature
        + audio_valence
        + energy
        + acousticness
        + instrumentalness
        + speechiness
    )
    
    # Hyperparameter Tuning (caret + ranger)
    # Cross-validation setup
    
    ctrl <- trainControl(
        method = "repeatedcv",
        number = 5,
        repeats = 2,
        verboseIter = TRUE
    )

    # Tuning grid
    grid <- expand.grid(
        mtry = c(3, 5, 7, 9),
        splitrule = c("variance", "extratrees"),
        min.node.size = c(3, 5, 10)
    )
    
    # Train tuned model
    set.seed(123)
    
    rf_tuned <- train(
        formula,
        data = train,
        method = "ranger",
        trControl = ctrl,
        tuneGrid = grid,
        num.trees = 1200,
        importance = "permutation",
        replace = FALSE,
        sample.fraction = 0.7,
        respect.unordered.factors = "order",
        metric = "RMSE"
    )
    
    print(rf_tuned$bestTune)
    
    # Final Model (Refit on Training Data)
    
    best <- rf_tuned$bestTune
    
    final_rf <- ranger(
        formula = formula,
        data = train,
        num.trees = 1500,
        mtry = best$mtry,
        min.node.size = best$min.node.size,
        splitrule = best$splitrule,
        replace = FALSE,
        sample.fraction = 0.7,
        importance = "permutation",
        respect.unordered.factors = "order",
        seed = 123
    )
    

    # Predictions
    pred <- predict(final_rf, data = test)$predictions
    
    # Metrics
    RMSE <- sqrt(mean((pred - test$song_popularity)^2))
    R2   <- cor(pred, test$song_popularity)^2
    
    cat("\nTest RMSE:", RMSE, "\n")
    cat("Test R²:", R2, "\n")
    songs_test <- readRDS("data/songs_test_imputed.RDS")
    
    full_train <- model.matrix(formula, data = songs)
    full_test <- model.matrix(formula, data = songs_test)
    
    full_fit <- ranger(
        formula = formula,
        data = train,
        num.trees = 1500,
        mtry = best$mtry,
        min.node.size = best$min.node.size,
        splitrule = best$splitrule,
        replace = FALSE,
        sample.fraction = 0.7,
        importance = "permutation",
        respect.unordered.factors = "order",
        seed = 123
    )
    
    full_prediction <- tibble(
        id = songs_test$ID,
        song_popularity = predict(full_fit, full_test) |> round() 
        #No compila pq el dataset songs_test te algunes variables diferents...
    )
    
    write.csv(
        full_prediction, file = "predictions/randomforest_pred.csv",
        row.names = FALSE
    )