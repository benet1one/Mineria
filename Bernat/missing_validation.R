
source("reading.R")
songs_imp <- readRDS("data/songs_imputed.RDS")
where_imp <- is.na(songs) |> as_tibble()

bivariate_imputation_plot <- function(x, y) {
    n_imputed <- where_imp |> 
        select({{x}}, {{y}}) |> 
        rowSums() |> 
        factor(levels = 0:2, labels = c("Original", "One Imputed", "Both Imputed"))
    
    ggplot(songs_imp, aes(x = {{x}}, y = {{y}}, color = n_imputed)) +
        geom_point(alpha = 0.4) +
        theme_minimal() +
        scale_color_manual(
            name = "Imputation", 
            values = c("gray20", "orange", "red4")
        )
}

bivariate_imputation_plot(loudness, energy)
bivariate_imputation_plot(acousticness, energy)
bivariate_imputation_plot(loudness, acousticness)
bivariate_imputation_plot(danceability, audio_valence)
bivariate_imputation_plot(instrumentalness, speechiness)
