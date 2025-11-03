
library(tidyverse)
library(visdat)
library(ggplot2)
source("reading.R")

# Exploration of missing values --------------------------------------------

# Analyse missing patron
vis_miss(songs_all)

naniar::gg_miss_upset(songs_all)
test_MCAR <- naniar::mcar_test(songs)
test_MCAR$p.value
# We do not reject the hypothesis that the missing values are MCAR.
# They have been generated completely at random.


# Identify which variables and observations have missing values.
## Variables
tibble(
    variable = names(songs),
    n_miss = colSums(is.na(songs)),
    prop_miss = n_miss / nrow(songs)
) |> 
    arrange(desc(prop_miss))

## Observations
missing_rows <- tibble(
    ID = songs$ID,
    n_miss = rowSums(is.na(songs)), 
    prop_miss = n_miss / ncol(songs)
)

missing_rows |>
    ggplot(aes(x = prop_miss)) +
    geom_bar() +
    labs(title = "Distribution of proportion of missings by instance",
         x = "Proportion of missing values",
         y = "Frequency")

# 1.7% of observations have missing values in most variables. 
# Not recommendable to impute from less than half of the variables. We remove them.
mean(missing_rows$prop_miss > 0.5)
songs <- songs[missing_rows$prop_miss <= 0.5, ]
songs_all <- bind_rows(songs, songs_test) |> 
    mutate(training = !is.na(song_popularity))


## Pre-analysis to locate rare things.
songs_all |>
    select(where(is.numeric)) |>
    pivot_longer(!ID, names_to = "variable") |>
    filter(!is.na(value)) |>
    group_by(variable) |>
    summarise(
        min = min(value),
        q1 = quantile(value, 0.25),
        median = median(value),
        q3 = quantile(value, 0.75),
        max = max(value),
        sd = sd(value)
    ) |>
    arrange(desc(max))




# Imputing mode from key ---------------------------------------------

mode_prop <- table(songs_all$audio_mode) |> proportions()
key_mode_prop <- table(songs_all$key, songs_all$audio_mode) |> proportions(margin = 1)
print(key_mode_prop)

songs_all <- songs_all |> mutate(
    prob_major = if_else(
        !is.na(audio_mode),
        as.numeric(audio_mode == "Major"),
        if_else(
            !is.na(key),
            key_mode_prop[key, "Major"],
            mode_prop["Major"]
        )
    ),
    .after = audio_mode,
    audio_mode = ifelse(
        !is.na(key),
        prob_major > 0.5,
        NA
    ) |> factor(labels = c("Minor", "Major"))
)

# Mode Proportion before
mode_prop
# Mode Proportion now
table(songs_all$audio_mode) |> proportions()


# Imputing instrumentalness from speechiness ----------------------

songs_all <- within(songs_all, {
    instrumentalness[is.na(instrumentalness) & speechiness > 0.5] <- 0
})


# Found Outliers --------------------------------------------------

# Loudness is suspicious, only 4 values above 0
songs_all |> arrange(-loudness) |> select(loudness, energy)
plot(energy ~ loudness, data = songs_all, col = ifelse(songs$loudness > 0, "red", "black"))

# With the previous plot it looks like 0 is a soft maximum. We correct the loudness of the
# red dot to 0, and impute the rest.
songs_all <- within(songs_all, {
    loudness[loudness > 0 & !is.na(energy)] <- 0
    loudness[loudness > 0] <- NA
})


# Mice ------------------------------------------------------------

# Transform energy and acousticness
logit <- \(x) log(x / (1 - x))
logit_inv <- \(x) exp(x) / (1 + exp(x))
songs_all$acousticness <- logit(songs_all$acousticness)

# Mice allows a predictor matrix with the following interpretation:
# - Rows: Imputed variable
# - Columns: Regressor variables
# A value of 1 means the row variable will be imputed using the column variable.
predictor_matrix <- matrix(1L, nrow = ncol(songs_all), ncol = ncol(songs_all))
rownames(predictor_matrix) <- colnames(predictor_matrix) <- names(songs_all)

# These cannot be used as a regressors.
predictor_matrix[, c("ID", "song_popularity", "training")] <- 0L
# Probabily bad to regress by key.
predictor_matrix[, "key"] <- 0L
# Probabily better to use audio_mode.
predictor_matrix[, "prob_major"] <- 0L

# Most variables are correctly imputed with predictive mean, matching, but some aren't.
methods <- character(ncol(songs_all))
names(methods) <- names(songs_all)
methods[] <- "pmm"
methods["loudness"] <- "lasso.norm"
methods["instrumentalness"] <- "rf"

# Acousticness is very annoying, I will try to make it depend only on energy, loudness and the
# categorical variables.
methods["acousticness"] <- "pmm"
predictor_matrix["acousticness", ] <- 0L
predictor_matrix["acousticness", c("energy", "loudness", "time_signature", "audio_mode")] <- 1L

# Sensitively imputed variables should not depend on other sensitively imputed variables
# unless their correlation is strong.
predictor_matrix["loudness", "speechiness"] <- 0L
predictor_matrix[c("loudness", "instrumentalness", "speechiness"), "acousticness"] <- 0L

songs_mice <- mice::mice(
    data = songs_all,
    m = 5,
    method = methods,
    predictorMatrix = predictor_matrix,
    seed = 5151,
)

songs_all_imputed <- mice::complete(songs_mice, action = songs_mice$m) |> 
    # Truncate loudness to less than zero
    mutate(loudness = pmin(loudness, 0)) |> 
    # Revert transformation on acousticness
    mutate(acousticness = logit_inv(acousticness)) |> 
    as_tibble()

songs_imputed <- songs_all_imputed |> filter(training) |> select(-training)
songs_test_imputed <- songs_all_imputed |> filter(!training) |> select(-training)

saveRDS(songs_imputed, "data/songs_imputed.RDS")
saveRDS(songs_test_imputed, "data/songs_test_imputed.RDS")
saveRDS(songs_all_imputed, "data/songs_all_imputed.RDS")


# Since we imputed the missing values from both the training and the test set
# at the same time, it also imputed song_popularity, which is a neat first prediction!
hist(songs_test_imputed$song_popularity)

songs_test_imputed |> 
    rename(id = ID) |> 
    select(id, song_popularity) |> 
    write.csv("predictions/mice.csv", row.names = FALSE)
