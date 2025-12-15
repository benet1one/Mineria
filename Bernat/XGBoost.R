
library(dplyr)
library(ggplot2)
source("KPI.R")

songs <- readRDS("data/songs_outlied.RDS") |> 
    filter(outlier_weight > 0.9)

set.seed(124)
test <- songs |> slice_sample(prop = 0.3)
train <- songs |> filter(!is.element(ID, test$ID))

formula <- (
    song_popularity ~ 0
    + liveness + loudness + danceability + song_duration_ms + tempo
    + time_signature + audio_valence + energy 
    + round(acousticness) + round(instrumentalness) + round(speechiness)
)

x_train <- model.matrix(formula, data = train)
x_test <- model.matrix(formula, data = test)
w_train <- train$outlier_weight
w_test <- test$outlier_weight

fit <- xgboost::xgboost(
    data = model.matrix(formula, data = train),
    label = train$song_popularity,
    weight = w_train,
    nrounds = 20,
    verbose = 0,
    params = list(
        max_depth = 6,
        gamma = 500
    )
)

xgboost::xgb.model.dt.tree(model = fit)

rmse_mat <- tibble(tuned = FALSE, depth = 2:fit$niter) |> 
    rowwise() |> 
    mutate(rmse = {
        pred <- predict(fit, x_test, iterationrange = c(1, depth))
        rmse(test$song_popularity, pred)
    })

ggplot(rmse_mat, aes(x = depth, y = rmse)) +
    geom_line() +
    geom_point() +
    theme_minimal()


tuned_fit <- caret::train(
    form = formula,
    data = filter(train, outlier_weight > 0.9),
    method = "xgbTree",
    metric = "RMSE",
    verbose = 1,
    trControl = caret::trainControl(
        method = "cv",
        number = 15,
        returnResamp = "all"
    ),
    tuneGrid = expand.grid(
        eta = 2^seq(-4, +1),
        max_depth = 2:6,
        colsample_bytree = c(0.6, 0.8, 1.0),
        
        # Max depth controls depth, no need for huge gamma.
        gamma = 500,
        
        # Eta controls number of boosting rounds, leave this as generous.
        nrounds = 12,

        # Default xgboost parameters
        min_child_weight = 1,
        subsample = 1
    )
)

print(tuned_fit)
hyper <- names(tuned_fit$bestTune)
metrics <- tuned_fit$resample |> 
    as_tibble() |> 
    group_by(pick(all_of(hyper))) |> 
    summarise(
        mean_rmse = mean(RMSE),
        sd_rmse = sd(RMSE),
        max_rmse = max(RMSE),
        top80_rmse = quantile(RMSE, 0.80),
        .groups = "drop"
    )

# Min Max(RMSE)
metrics |> arrange(max_rmse) |> head(3)

# Min Expected(RMSE)
metrics |> arrange(mean_rmse) |> head(3)

# Midpoint between the above
metrics |> arrange(top80_rmse) |> head(3)

# We finally chose to keep the model with the least Expected(RMSE).
best_tune <- tuned_fit$bestTune
best_fit <- tuned_fit$finalModel

tuned_rmse_mat <- tibble(tuned = TRUE, depth = 2:best_fit$niter) |> 
    rowwise() |> 
    mutate(rmse = {
        pred <- predict(best_fit, x_test, iterationrange = c(1, depth))
        rmse(test$song_popularity, pred)
    })

rmse_mat <- bind_rows(rmse_mat, tuned_rmse_mat)

ggplot(rmse_mat, aes(x = depth, y = rmse, color = tuned)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d(end = 0.7) +
    scale_x_continuous(breaks = 2:12, minor_breaks = NULL, limits = c(2, 12)) +
    theme_minimal()


songs_test <- readRDS("data/songs_test_imputed.RDS")

full_train <- model.matrix(formula, data = songs)
full_test <- model.matrix(formula, data = songs_test)

full_fit <- xgboost::xgboost(
    data = full_train,
    label = songs$song_popularity,
    weight = songs$outlier_weight,
    nrounds = 12,
    verbose = 1,
    params = as.list(best_tune)
)

full_prediction <- tibble(
    id = songs_test$ID,
    song_popularity = predict(full_fit, full_test) |> round(4)
)

write.csv(
    full_prediction, file = "predictions/xgboost.csv",
    row.names = FALSE
)

hist(full_prediction$song_popularity)
