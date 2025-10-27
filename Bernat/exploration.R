
source("reading.R")
par(mfcol = c(1, 1))

songs |> 
    select(where(is.numeric)) |> 
    cor(use = "pairwise") |> 
    corrplot::corrplot()

songs |> 
    select(where(is.numeric)) |> 
    sapply(function(variable) {
        lm(variable ~ time_signature + audio_mode, data = songs) |> 
            summary() |> 
            _$fstatistic["value"] |> 
            unname()
    }) |> 
    sort(decreasing = TRUE)

songs |> 
    filter(!is.na(time_signature), !is.na(audio_mode)) |>
    select(acousticness, danceability, energy, loudness, speechiness, time_signature, audio_valence, audio_mode) |> 
    tidyr::pivot_longer(!c(time_signature, audio_mode), names_to = "variable") |> 
    ggplot(aes(y = value, x = time_signature, color = audio_mode)) +
    facet_wrap(~variable, scales = "free") +
    geom_boxplot(fatten = 1) +
    theme_minimal()

lognorm_loudness <- log(-songs$loudness + 1)
hist(lognorm_loudness)

cbind(
    raw = songs |> select(where(is.numeric)) |> cor(songs$loudness, use = "pairwise"),
    lognorm = songs |> select(where(is.numeric)) |> cor(lognorm_loudness, use = "pairwise")
)

map_phi <- function(phi) {
    scales::rescale(
        exp(phi) / (1 + exp(phi)),
        from = c(0, 1), 
        to = c(0.1, 100)
    )
}

transformed_mean_cor <- function(phi) {
    phi <- map_phi(phi)
    songs |> 
        mutate(
            energy = energy^phi["energy"],
            acousticness = acousticness^phi["acousticness"]
        ) |> 
        select(loudness, energy, acousticness) |> 
        cor(use = "pairwise") |> 
        abs() |> 
        mean()
}

# remotes::install_github("benet1one/layer")
opt <- layer::maximize(transformed_mean_cor, init = c(energy = 0, acousticness = 0))
best_phi <- map_phi(opt$solution)
print(best_phi)


songs_before <- songs |> 
    select(loudness, energy, acousticness, instrumentalness, audio_valence) |> 
    mutate(loudness = scale(loudness)[, 1])
    
songs_before <- songs_before[complete.cases(songs_before), ]

songs_after <- songs_before |> mutate(
    energy = energy^best_phi["energy"],
    acousticness = acousticness^best_phi["acousticness"]
)

pca_before <- prcomp(songs_before)
pca_after <- prcomp(songs_after)

pca_before$sdev / sum(pca_before$sdev)
pca_after$sdev / sum(pca_after$sdev)


with(songs_before, {
    par(mfcol = c(2, 2))
    plot(energy ~ loudness)
    plot(acousticness ~ loudness)
    plot(energy ~ acousticness)
})

with(songs_after, {
    par(mfcol = c(2, 2))
    plot(energy ~ loudness)
    plot(acousticness ~ loudness)
    plot(energy ~ acousticness)
})
