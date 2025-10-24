
source("reading.R")
songs_imp <- readRDS("data/songs_imputed.RDS")
where_imp <- is.na(songs) |> as_tibble()

bivariate_imputation_plot <- function(x, y) {
    imp_cols <- where_imp |> select({{x}}, {{y}})
    r_sums <- rowSums(imp_cols)

    imp <- character(nrow(imp_cols))
    imp[r_sums == 0] <- "Original"
    imp[r_sums == 2] <- "Imputed both"
    imp[r_sums == 1] <- if_else(
        unlist(imp_cols[r_sums == 1, 1]),
        paste("Imputed", names(imp_cols)[1]),
        paste("Imputed", names(imp_cols)[2])
    )
    
    imp <- factor(imp, levels = c(
        "Original",
        paste("Imputed", names(imp_cols)[1]),
        paste("Imputed", names(imp_cols)[2]),
        "Imputed both"
    ))
    
    ggplot(songs_imp, aes(x = {{x}}, y = {{y}}, color = imp)) +
        geom_point(alpha = 0.4) +
        theme_minimal() +
        scale_color_manual(
            name = "Imputation", 
            values = c("gray20", "royalblue", "orange2", "red4")
        )
}

bivariate_imputation_plot(loudness, energy)
bivariate_imputation_plot(acousticness, energy)
bivariate_imputation_plot(loudness, acousticness)
bivariate_imputation_plot(danceability, audio_valence)
bivariate_imputation_plot(instrumentalness, speechiness)
