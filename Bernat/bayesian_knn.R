
library(dplyr)

songs <- readRDS("data/songs_all_imputed.RDS")

weighted_mape <- function(actual, predicted, weights = rep(1, length(actual))) {
    is_zero <- (actual == 0)
    weighted.mean(
        (abs(actual - predicted) + is_zero) / (actual + is_zero),
        w = weights
    )
}

minimize_weighted_mape <- function(draws, weights = rep(1, length(draws))) {
    candidates <- 0:100
    candidate_mape <- sapply(candidates, function(pred) 
        weighted_mape(actual = draws, predicted = pred, weights = weights)
    )
    candidates[which.min(candidate_mape)]
}

minimize_weighted_mape <- function(draws, weights = rep(1, length(draws))) {
    opt <- optimise(
        \(predicted) weighted_mape(actual = draws, predicted = predicted, weights = weights),
        interval = c(0, 100)
    )
    
    opt$minimum
}

bayesian_knn <- function(x_train, y_train, x_test, k = 50, weight_multiplier = 5.0) {
    nn <- FNN::knnx.index(
        data = x_train,
        query = x_test,
        k = k
    )

    apply(nn, 1, function(which_nn) {
        minimize_weighted_mape(y_train[which_nn])
    })
}


set.seed(123)
songs <- readRDS("data/songs_all_imputed.RDS")

songs_num <- songs |> 
    select(ID, training, liveness, danceability, audio_valence, energy, acousticness, speechiness, 
           key, audio_mode, song_popularity) |> 
    mutate(across(
        c(where(is.numeric), - ID, - song_popularity), 
        scale
    )) |> 
    fastDummies::dummy_columns() |> 
    mutate(fold = rep_len(1:3, n()) |> factor())


for (f in levels(songs_num$fold)) {
    cat("\nFold", f)
    
    train <- songs_num |> 
        filter(fold != f) |> 
        filter(training) |> 
        select(where(is.numeric), -fold)
        
    test <- songs_num |> 
        filter(fold == f) |> 
        filter(training) |> 
        select(where(is.numeric), -fold)
    
    predicted <- bayesian_knn(
        x_train = train |> select(-song_popularity), 
        y_train = train$song_popularity,
        x_test = test |> select(-song_popularity)
    )
    
    predicted <- pmin(predicted, 4)
    
    mape_mean <- minimize_weighted_mape(draws = train$song_popularity)
    worst_mape <- weighted_mape(actual = train$song_popularity, predicted = mape_mean)
    knn_mape <- weighted_mape(actual = train$song_popularity, predicted = predicted)
    
    cat("\nWorst MAPE =", worst_mape,
        "\nKnn MAPE =", knn_mape,
        "\nImprovement =", -(knn_mape - worst_mape))
}

