
songs <- readRDS("data/songs_imputed.RDS")
test <- readRDS("data/songs_test_imputed.RDS")

mape <- function(actual, predicted) {
    is_zero <- (actual == 0)
    mean((abs(actual - predicted) + is_zero) / (actual + is_zero))
}

predict_min_mape <- function(draws) {
    candidates <- 0:100
    draw_mape <- sapply(candidates, \(pred) mape(actual = draws, predicted = pred))
    candidates[which.min(draw_mape)]
}

sophisticated_prediction <- tibble(
    id = test$ID,
    song_popularity = predict_min_mape(songs$song_popularity)
)

write.csv(
    sophisticated_prediction, 
    file = "predictions/highly_sophisticated_prediction.csv", 
    row.names = FALSE
)
