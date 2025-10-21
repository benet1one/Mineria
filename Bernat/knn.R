
library(dplyr)
library(ggplot2)
source("reading.R")

songs <- songs[complete.cases(songs), ]

distance_daisy <- songs |>
    select(-ID, -song_popularity, -key) |>
    # mutate(loudness = scale(loudness)[, 1]) |>
    # mutate(tempo = scale(tempo)[, 1]) |>
    # fastDummies::dummy_cols(remove_selected_columns = TRUE) |>
    cluster::daisy("gower")

distance_mat <- as.matrix(distance_daisy)

k_exploration <- combn(nrow(songs), 2) |>
    t() |>
    as.data.frame() |>
    as_tibble() |>
    rowwise() |>
    mutate(
        distance = distance_mat[V1, V2],
        pop_diff = abs(
            songs$song_popularity[V1] - songs$song_popularity[V2]
        )
    ) |>
    ungroup() |>
    mutate(distance_cat = cut(distance, breaks = 10)) |>
    print()


ggplot(k_exploration, aes(x = distance, y = pop_diff)) +
    geom_point(alpha = 0.1) +
    geom_smooth()

ggplot(k_exploration, aes(x = distance_cat, y = pop_diff)) +
    geom_boxplot()


