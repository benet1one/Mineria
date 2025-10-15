
source("reading.R")
par(mfcol = c(1, 1))

songs |> 
    select(where(is.numeric)) |> 
    cor(use = "pairwise") |> 
    corrplot::corrplot()

lognorm_loudness <- log(-songs$loudness + 1)
hist(lognorm_loudness)

cbind(
    raw = songs |> select(where(is.numeric)) |> cor(songs$loudness, use = "pairwise"),
    lognorm = songs |> select(where(is.numeric)) |> cor(lognorm_loudness, use = "pairwise")
)

# TEST WITH AND WITHOUT
# songs$loudness <- lognorm_loudness


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
