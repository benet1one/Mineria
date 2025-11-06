
library(dplyr)
library(ggplot2)

songs <- readRDS("data/songs_imputed.RDS")

distance_daisy <- songs |>
    slice_sample(n = 5000) |> 
    select(-ID, -song_popularity, -key, -prob_major) |>
    cluster::daisy("gower")

distance_mat <- as.matrix(distance_daisy)

k_exploration <- tibble(
    i1 = sample.int(nrow(distance_mat)),
    i2 = sample.int(nrow(distance_mat))
) |> 
    rowwise() |>
    mutate(
        distance = distance_mat[i1, i2],
        pop_diff = abs(
            songs$song_popularity[i1] - songs$song_popularity[i2]
        )
    ) |>
    ungroup() |>
    mutate(distance_cat = cut(distance, breaks = 10)) |>
    print()



dist_contribution <- function(df, i1, i2) {
    lapply(df, function(variable) {
        if (is.numeric(variable)) {
            (variable[i1] - variable[i2])^2
        } else {
            as.integer(variable[i1] != variable[i2])
        }
    })
}

songs_scaled <- songs |> 
    select(-ID, -prob_major) |> 
    mutate(across(where(is.numeric), function(x) scale(x)[, 1]))

nn_idx <- songs_scaled |> 
    select(where(is.numeric), -song_popularity) |> 
    FNN::knn.index()

close_distances <- reshape2::melt(nn_idx, varnames = c("i1", "k"), value.name = "i2") |> 
    as_tibble() |> 
    select(-k) |> 
    slice_sample(n = 10e3) |> 
    rowwise() |> 
    mutate(contribution = list(dist_contribution(songs_scaled, i1, i2))) |> 
    tidyr::unnest_wider(contribution) |> 
    select(-i1, -i2)


design_mat <- model.matrix(song_popularity ~ . + 0, data = close_distances)
head(design_mat)

while (ncol(design_mat) > 1) {
    fit <- lm.fit(x = design_mat, y = close_distances$song_popularity)
    coef <- coefficients(fit)
    
    if (min(coef) < 0) {
        cat("\nEliminated", names(which.min(coef)))
        design_mat <- design_mat[, -which.min(coef)]
        next
    } else {
        break
    }
}



