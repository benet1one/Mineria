
library(tidyverse)
library(visdat)
library(ggplot2)
source("reading.R")

# Exploration of missing values --------------------------------------------

# Analyse missing patron
vis_miss(songs)

naniar::gg_miss_upset(songs)
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


## Pre-analysis to locate rare things.
songs |>
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

mode_prop <- table(songs$audio_mode) |> proportions()
key_mode_prop <- table(songs$key, songs$audio_mode) |> proportions(margin = 1)

songs <- songs |> mutate(
    prob_major = if_else(
        !is.na(audio_mode),
        as.numeric(audio_mode == "Major"),
        if_else(
            !is.na(key),
            key_mode_prop[key, "Major"],
            mode_prop["Major"]
        )
    ),
    audio_mode = factor(prob_major > 0.6, labels = c("Minor", "Major"))
)

# Mode Proportion before
mode_prop
# Mode Proportion now
table(songs$audio_mode) |> proportions()


# Imputing instrumentalness from speechiness ----------------------

songs$instrumentalness[is.na(songs$instrumentalness) & songs$speechiness > 0.5] <- 0


# Found Outliers --------------------------------------------------

# Loudness is suspicious, only two values above 0
songs |> arrange(-loudness) |> select(loudness, energy)
plot(energy ~ loudness, data = songs, col = ifelse(songs$loudness > 0, "red", "black"))

# One of them is slightly above zero. Changing its value to zero is convenient later on.
# The other one is a clear univariate outlier. Because it has high energy,
# which is heavily correlated with loudness, we impute it as zero.
songs$loudness[songs$loudness >= 0] <- 0


# Mice ------------------------------------------------------------

# Transform energy and acousticness
logit <- \(x) log(x / (1 - x))
logit_inv <- \(x) exp(x) / (1 + exp(x))
songs$acousticness <- logit(songs$acousticness)

# Mice allows a predictor matrix with the following interpretation:
# - Rows: Imputed variable
# - Columns: Regressor variables
# A value of 1 means the row variable will be imputed using the column variable.
predictor_matrix <- matrix(1L, nrow = ncol(songs), ncol = ncol(songs))
rownames(predictor_matrix) <- colnames(predictor_matrix) <- names(songs)

# ID and popularity cannot be used as a regressors.
predictor_matrix[, c("ID", "song_popularity")] <- 0L
# Probabily bad to regress by key.
predictor_matrix[, "key"] <- 0L
# Probabily better to use audio_mode.
predictor_matrix[, "prob_major"] <- 0L

# Most variables are correctly imputed with predictive mean, matching, but some aren't.
methods <- character(ncol(songs))
names(methods) <- names(songs)
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
    data = songs,
    m = 5,
    method = methods,
    predictorMatrix = predictor_matrix,
    seed = 5151,
)

songs_imputed <- mice::complete(songs_mice, action = songs_mice$m) |> 
    # Truncate loudness to less than zero
    mutate(loudness = pmin(loudness, 0)) |> 
    # Revert transformation on acousticness
    mutate(acousticness = logit_inv(acousticness)) |> 
    as_tibble()

saveRDS(songs_imputed, "data/songs_imputed.RDS")
