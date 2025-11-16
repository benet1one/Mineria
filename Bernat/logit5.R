
library(dplyr)
source("KPI.R")

predictor_df <- function(df) {
    df |> mutate(
        duration_qual = cut(
            song_duration_ms, 
            breaks = c(0, 3*60e3, 4*60e3, Inf),
            labels = c("Short", "Medium", "Long")
        ),
        tempo_qual = cut(
            tempo, 
            breaks = c(0, 100, 130, Inf), 
            labels = c("Slow", "Medium", "Fast")
        )
    )
}

songs <- readRDS("data/songs_outlied.RDS") |> 
    predictor_df()


train <- songs |> slice_sample(prop = 0.7)
test <- songs |> rows_delete(train, by = "ID") |> suppressMessages()

worst_mape <- weighted_mape(test$song_popularity, 4)
cat("\nWorst MAPE =", worst_mape)


for (threshold in 0:5) {
    train$popular <- (train$song_popularity > threshold)
    
    fit <- glm(
        popular ~ liveness + loudness + danceability + duration_qual + tempo_qual 
        + key + audio_mode + energy + audio_valence + acousticness + instrumentalness, 
        data = train,
        weights = train$outlier_weight, 
        family = quasibinomial("logit")
    )
    
    prob_popular <- predict(fit, test, type = "response")
    predicted <- sapply(prob_popular, \(p) {
        wt <- integer(nrow(train))
        wt[ train$popular] <- p / mean(train$popular)
        wt[!train$popular] <- (1 - p) / mean(!train$popular)

        minimize_weighted_mape(train$song_popularity, weights = wt)
    })
    
    th_mape <- mape(test$song_popularity, predicted = predicted)
    cat("\nThreshold", threshold, "MAPE =", th_mape)
}

