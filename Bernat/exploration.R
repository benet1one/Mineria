
source("reading.R")

phi <- seq(0.01, 0.3, by = 0.01)
R2 <- sapply(phi, function(phi) {
    fit <- lm(loudness ~ I(energy^phi), data = songs)
    summary(fit) $ r.squared
})

ggplot(mapping = aes(x = phi, y = R2)) +
    geom_point()

songs |> 
    select(loudness, energy) |> 
    filter(!is.na(loudness), !is.na(energy)) |> 
    slice_sample(prop = 0.1) |> 
    cross_join(tibble(phi = seq(0.05, 0.2, by = 0.025))) |> 
    mutate(energy_to_phi = energy^phi) |> 
    mutate(phi = paste("phi =", phi)) |> 
    ggplot(aes(x = energy_to_phi, y = loudness)) +
    facet_wrap(~phi, scales = "free_x") +
    geom_point() +
    geom_smooth(method = "lm")


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
