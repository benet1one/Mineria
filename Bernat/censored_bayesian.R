
library(dplyr)
source("KPI.R")

model <- thomas::cmdstan_model(file = "Bernat/censored_bayesian.stan")

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
    filter(outlier_weight > 0.9) |> 
    predictor_df()


fit_glm <- glm(
    (song_popularity + 1) ~ 
        liveness + loudness + danceability + duration_qual 
        + time_signature + key + prob_major
        + audio_valence + energy + tempo*tempo_qual
        + speechiness*instrumentalness + acousticness,
    data = songs,
    weights = songs$outlier_weight,
    family = Gamma(link = "log")
)

car::Anova(fit_glm)

# By running step(fit_glm) we get the following model:
# + loudness 
# + audio_valence 
# + energy 
# + danceability 
# + tempo_qual 
# + duration_qual 
# + key 
# + acousticness 
# + speechiness*instrumentalness


fit_data <- within(list(), {
    N = nrow(songs)
    popularity = songs$song_popularity
    
    index_pos = which(popularity > 0)
    index_zero = which(popularity == 0)
    
    N_pos = length(index_pos)
    N_zero = length(index_zero)
    
    x_numeric = scale(model.matrix(
        data = songs,
        ~ 0 + loudness + audio_valence + danceability 
        + acousticness
        # + speechiness*instrumentalness
    ))
    x_categorical = model.matrix(
        data = songs,
        ~ 0 + tempo_qual + duration_qual
        # + key
    )
    
    k_numeric = ncol(x_numeric)
    k_categorical = ncol(x_categorical)
})

fit <- thomas::run_cmdstan(
    model,
    data = fit_data,
    iter = 200,
    chains = 3,
    refresh = 10,
    parallel_chains = 3
)

thomas::traceplot(fit)
thomas::traceplot(fit, beta, .max_values = 50)
thomas::traceplot(fit, optimal_x, .max_values = 50)

draws <- thomas::get_draws(fit)
# colnames(draws$beta) <- colnames()
# draws

# pred <- sapply(1:nrow(draws), function(d) {
#     thomas::attach_draw(draws, d)
#     s0 <- c(s0)
#     s1 <- c(s1)
#     
#     mu <- design_mat %*% beta
#     sigma <- s0 + s1 * pmax(mu, 0)
#     rnorm(length(mu), mu, sigma) |> pmax(0) |> pmin(100)
# })

mean_pred <- rowMeans(pred)
sd_pred <- apply(pred, 1, sd)

hist(pred)
hist(mean_pred)
hist(sd_pred)

residuals <- (mean_pred - songs$song_popularity)
std_residuals <- residuals / sd_pred

hist(residuals)
plot(residuals ~ mean_pred)
plot(std_residuals ~ mean_pred)


mape_pred <- apply(pred, 1, minimize_weighted_mape)
mape(actual = songs$song_popularity, predicted = mape_pred)

mu <- design_mat %*% t(draws$beta)
mape(actual = songs$song_popularity, predicted = mu)

predict_min_mape(draws = songs$song_popularity)
mape(actual = songs$song_popularity, predicted = 4)
