
source("reading.R")
songs_imp <- readRDS("data/songs_imputed.RDS")
songs <- songs |> filter(ID %in% songs_imp$ID)
where_imp <- is.na(songs) |> as_tibble()

imputation_scatterplot <- function(x, y) {
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

imputation_boxplot <- function(y, groups) {
    df <- bind_rows(
        songs     |> select({{y}}, {{groups}}) |> mutate(..imputation = "Original"),
        songs_imp |> select({{y}}, {{groups}}) |> mutate(..imputation = "Imputed"),
    ) |> mutate(..imputation = factor(..imputation, levels = c("Original", "Imputed")))

    if (!missing(groups)) {
        df <- filter(df, !is.na({{groups}}))
    }
    
    ggplot(df, aes(x = {{groups}}, y = {{y}}, color = ..imputation)) +
        geom_boxplot(fatten = 1, linewidth = 1) +
        theme_minimal() +
        scale_color_manual(name = "Imputation", values = c("gray20", "orange2"))
}

imputation_boxplot(tempo)
imputation_boxplot(loudness)
imputation_boxplot(song_duration_ms)

imputation_boxplot(acousticness, audio_mode)
imputation_boxplot(acousticness, time_signature)
imputation_boxplot(audio_valence, time_signature)
imputation_boxplot(danceability, time_signature)
imputation_boxplot(speechiness, time_signature)
imputation_boxplot(energy, time_signature)

imputation_scatterplot(loudness, energy)
imputation_scatterplot(loudness, acousticness)
imputation_scatterplot(energy, acousticness)
imputation_scatterplot(danceability, audio_valence)
imputation_scatterplot(instrumentalness, speechiness)

mean(songs$instrumentalness > 0, na.rm = TRUE)
mean(songs_imp$instrumentalness > 0)

table(songs$audio_mode) |> proportions()
table(songs_imp$audio_mode) |> proportions()
