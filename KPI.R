
mape <- function(actual, predicted) {
    weighted_mape(actual = actual, predicted = predicted)
}

weighted_mape <- function(actual, predicted, weights = rep(1, length(actual))) {
    is_zero <- (actual == 0)
    weighted.mean(
        (abs(actual - predicted) + is_zero) / (actual + is_zero),
        w = weights
    )
}

minimize_weighted_mape <- function(draws, weights = rep(1, length(draws)), objective_value = FALSE) {
    opt <- optimise(
        f = function(predicted) 
            weighted_mape(actual = draws, predicted = predicted, weights = weights),
        interval = c(0, 100),
        tol = 0.04
    )
    
    solution <- round(opt$minimum)
    
    if (objective_value) {
        weighted_mape(actual = draws, predicted = solution, weights = weights)
    } else {
        solution
    }
}

rmse <- function(actual, predicted, weights = rep(1, length(actual))) {
    sqrt(weighted.mean(
        (actual - predicted)^2,
        w = weights
    ))
}
