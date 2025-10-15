
source("Bernat/also.R")

forest <- solitude::isolationForest$new(
    sample_size = nrow(songs_also),
    num_trees = 100,
    seed = 112358
)

forest$fit(songs_also)
forest_score <- forest$predict(songs_also)

songs_also$forest_of <- forest_score$anomaly_score
cor(select(songs_also, where(is.numeric))) [, "forest_of"]
hist(songs_also$forest_of, breaks = 20)

# 0.65 is a nice cutoff point
songs_also <- songs_also |> mutate(
    forest_of = scales::rescale(
        forest_of,
        from = c(0.6, max(forest_of)),
        to = c(0, 1)
    ) |> pmax(0)
)

cor(select(songs_also, where(is.numeric))) [, "forest_of"]
songs_also |> arrange(-combined_of) |> relocate(also_of, instr_of, combined_of, forest_of)
songs_also |> arrange(-forest_of)   |> relocate(also_of, instr_of, combined_of, forest_of)
