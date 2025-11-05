
songs <- readRDS("data/songs_imputed.RDS")

# Time signature is very unbalanced
table(songs$time_signature)

# We oversample uncommon time signatures by 
# - Sampling pairs of two close observations
# - Numeric variables are interpolated
# - Categorical variables are sampled at random, all from the same observation in the pair

oversample <- function(s, n = 1000) {
    pair_distances <- s |> 
        select(-ID, -time_signature) |> 
        cluster::daisy("gower") |> 
        as.matrix() |> 
        reshape2::melt(value.name = "distance") |> 
        as_tibble() |> 
        rename(i1 = 1, i2 = 2) |> 
        mutate(pair = 1:n(), .before = i1) |> 
        mutate(w = 1 / (distance + quantile(distance, 0.1)))
    
    pairs <- slice_sample(pair_distances, n = n - nrow(s), weight_by = w) |> 
        select(pair, i1, i2) |> 
        tidyr::pivot_longer(i1:i2, values_to = "i") |> 
        select(-name)
    
    s$..i <- 1:nrow(s)
    
    combined <- pairs |> 
        left_join(s, join_by(i == ..i)) |> 
        group_by(pair) |> 
        summarise(
            ID = paste0(ID[1], "-", ID[2]),
            time_signature = time_signature[1],
            
            cat_from = sample(1:2, size = 1),
            key = key[cat_from],
            audio_mode = audio_mode[cat_from],
            instrumentalness = instrumentalness[cat_from],
            speechiness = speechiness[cat_from],
            
            across(where(is.integer), function(x) as.integer(mean(x))),
            across(where(is.double), function(x) mean(x)),
        )
    
    s$..i <- NULL
    new_obs <- combined |> select(-i, -pair, -cat_from)
    bind_rows(s, new_obs)
}

songs_oversampled <- songs |> 
    mutate(ID = as.character(ID)) |> 
    select(-prob_major) |> 
    group_split(time_signature)

# time_signature == 3/4
songs_oversampled[[1]] <- oversample(songs_oversampled[[1]], n = 12e3)
# time_signature == 5/4
songs_oversampled[[3]] <- oversample(songs_oversampled[[3]], n = 12e3)

songs_oversampled <- bind_rows(songs_oversampled)
table(songs_oversampled$time_signature)

saveRDS(songs_oversampled, file = "data/songs_oversampled.RDS")
