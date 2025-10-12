
library(ggplot2)
source("reading.R")

# Boxplots
songs |> 
    select(where(is.numeric)) |> 
    tidyr::pivot_longer(!ID) |> 
    filter(!is.na(value)) |> 
    ggplot(aes(x = value)) +
    facet_wrap(~name, scales = "free") +
    geom_boxplot() +
    theme_minimal()


# Loudness is suspicious, only two values above 0
songs |> arrange(-loudness) |> select(loudness, energy)
plot(energy ~ loudness, data = songs)

# Since the biggest value has very high energy, we impute it 
songs$loudness[songs$loudness >= 0] <- (-2e-8)


# Song duration
duration <- hms::hms(songs$song_duration_ms / 1000)
ggplot(NULL, aes(x = duration)) + geom_boxplot()
summary(duration)

# Not necessarily outliers, but they could have a large effect on model fitting.
# Proposal: Split variable taking into account quantiles (3min, 4min).
# Note: Tree based models can use raw variable, don't need to use the qualitative one.
songs$duration_qual <- cut(
    songs$song_duration_ms, 
    breaks = c(0, 3*60e3, 4*60e3, Inf),
    labels = c("Short", "Medium", "Long")
)
summary(songs$duration_qual)


songs_ol <- songs |> 
    # Exclude key because it has too many values
    # and song_duration_ms because of heavy influence
    select(-key, -song_duration_ms) |> 
    # Converting probabilities to logical values
    mutate(
        live = liveness > 0.5,
        acoustic = acousticness > 0.5,
        instrumental = instrumentalness > 0.5,
        liveness = NULL, acousticness = NULL, instrumentalness = NULL
    )


songs_num <- songs_ol |> 
    mutate(across(c(live, acoustic, instrumental), as.integer)) |> 
    select(where(is.numeric), -ID) |> 
    missMDA::imputePCA(ncp = 5, threshold = 0.01) |> 
    _$completeObs

pca <- prcomp(songs_num)
barplot(pca$sdev[1:10])

try_ncomp <- 3:(length(pca$sdev) - 1)
pca_of <- sapply(try_ncomp, function(ncomp) {
    songs_reframed <- tcrossprod(pca$x[, 1:ncomp], pca$rotation[, 1:ncomp])
    abs(as.matrix(songs_num) - songs_reframed) |> rowSums()
})

colnames(pca_of) <- try_ncomp
(cumsum(pca$sdev)[try_ncomp] / sum(pca$sdev)) |> round(3)
cor(songs_num, pca_of) |> corrplot::corrplot()

of <- pca_of[, try_ncomp == 8]
ggplot(songs_num, aes(x = speechiness, y = energy, color = of)) + geom_point()



