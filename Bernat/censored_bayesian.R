
songs <- readRDS("data/songs_outlied.RDS")
model <- thomas::cmdstan_model(file = "Bernat/censored_bayesian.stan")

tempo_qual <- cut(
    songs$tempo,
    breaks = c(0, 100, 130, Inf),
    labels = c("Slow", "Medium", "Fast")
)
    
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
design_mat <- model.matrix(
    ~ loudness + audio_valence + energy + danceability + tempo_qual 
        + duration_qual + key + instrumentalness + acousticness,
    data = songs
)

index_pos <- which(songs$song_popularity > 0)
index_zero <- which(songs$song_popularity == 0)

fit <- thomas::run_cmdstan(
    model,
    data = list(
        N = nrow(songs),
        N_pos = length(index_pos),
        N_zero = length(index_zero),
        index_pos = index_pos,
        index_zero = index_zero,
        popularity = songs$song_popularity,
        
        k = ncol(design_mat),
        x = design_mat
    ),
    iter = 2000,
    chains = 3
)

thomas::traceplot(fit)
thomas::traceplot(fit, beta, .max_values = 50)

draws <- thomas::get_draws(fit)
colnames(draws$beta) <- colnames(design_mat)
draws

pred <- sapply(1:nrow(draws), function(d) {
    thomas::attach_draw(draws, d)
    s0 <- c(s0)
    s1 <- c(s1)
    
    mu <- design_mat %*% beta
    sigma <- s0 + s1 * pmax(mu, 0)
    rnorm(length(mu), mu, sigma) |> pmax(0) |> pmin(100)
})

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


mape <- function(actual, predicted) {
    is_zero <- (actual == 0)
    mean((abs(actual - predicted) + is_zero) / (actual + is_zero))
}

predict_min_mape <- function(draws) {
    candidates <- 0:100
    draw_mape <- sapply(candidates, \(pred) mape(actual = draws, predicted = pred))
    candidates[which.min(draw_mape)]
}



mape_pred <- apply(pred, 1, predict_min_mape)
mape(actual = songs$song_popularity, predicted = mape_pred)

mu <- design_mat %*% t(draws$beta)
mape(actual = songs$song_popularity, predicted = mu)

predict_min_mape(draws = songs$song_popularity)
mape(actual = songs$song_popularity, predicted = 4)
