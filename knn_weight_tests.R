
library(dplyr)
library(ggplot2)

songs <- readRDS("data/songs_imputed.RDS")

# Feature Weighting
set.seed(123)

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

# Sample pairs of close observations and calculate individual contributions to the
# euclidian distance.

nn_idx <- songs_scaled |> 
    select(where(is.numeric), -song_popularity) |> 
    FNN::knn.index()

close_distances <- nn_idx |> 
    reshape2::melt(varnames = c("i1", "k"), value.name = "i2") |> 
    as_tibble() |> 
    select(-k) |> 
    slice_sample(n = 50e3) |> 
    rowwise() |> 
    mutate(contribution = list(dist_contribution(songs_scaled, i1, i2))) |> 
    tidyr::unnest_wider(contribution) |> 
    select(-i1, -i2)

# Fit (y - y)^2 ~ (x1 - x1)^2 + (x2 - x2)^2 + ...

design_mat <- model.matrix(song_popularity ~ . + 0, data = close_distances)
head(design_mat)

while (ncol(design_mat) > 1) {
    fit <- lm.fit(x = design_mat, y = close_distances$song_popularity)
    coef <- coefficients(fit)
    
    if (min(coef) < 0) {
        # Remove negative coefficients and fit again
        cat("Eliminated", names(which.min(coef)), "\n")
        design_mat <- design_mat[, -which.min(coef)]
        next
    } else {
        break
    }
}

last_fit <- lm(song_popularity ~ 0 + (. - instrumentalness - loudness), data = close_distances)
summary(last_fit)

# To make sure we don't eliminate too much information, we keep variables with p-value < 0.2
coef <- coefficients(summary(last_fit)) |> 
    as_tibble(rownames = "variable") |> 
    rename(weight = Estimate, p_value = "Pr(>|t|)") |> 
    filter(p_value < 0.2) |> 
    print()

# Coefficients represent the weight each variable should have in the knn.
saveRDS(coef, file = "knn_weights.RDS")