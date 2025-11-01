
library(ggplot2)
songs <- readRDS("data/songs_imputed.RDS")

songs$duration_qual <- cut(
    songs$song_duration_ms, 
    breaks = c(0, 3*60e3, 4*60e3, Inf),
    labels = c("Short", "Medium", "Long")
)

songs_also <- songs |> 
    select(-key, -time_signature, -song_duration_ms, -prob_major) |> 
    mutate(
        major = audio_mode == "Major", audio_mode = NULL, 
        song_popularity = song_popularity / 100,
        tempo = scale(tempo)[, 1]
    )


fit_model <- function(y, data) {
    if (is.logical(y)) {
        glm(y ~ ., family = binomial("logit"), data = data)
        
    } else if (min(y) >= 0  &&  max(y) <= 1) {
        glm(y ~ ., family = quasibinomial("logit"), data = data)
        
    } else if (min(y) >= 0) {
        glm(y ~ ., family = quasipoisson("log"), data = data)
        
    } else {
        lm(y ~ ., data = data)
    }
}

# Based on https://cienciadedatos.net/documentos/67_deteccion_anomalias_also
# Implemented k-fold as the article suggests, so that we don't predict outliers
# with models trained on the same outliers.
also <- function(data, kfold = 5, fitter = fit_model, dont_fit = c(), seed = NULL) {
    if (!is.null(seed)) 
        set.seed(seed)
    
    response_cols <- data |> 
        select(where(is.numeric), where(is.logical)) |> 
        select(-any_of(dont_fit)) |> 
        names()

    data <- data |> 
        mutate(..row = 1:n(), .before = 1) |> 
        group_by(pick(where(is.factor), where(is.logical))) |> 
        mutate(..fold = sample.int(kfold, size = n(), replace = TRUE), .after = 1) |> 
        ungroup()
    
    data_split <- data |> group_split(..fold)
    
    weight <- numeric(length(response_cols))
    error_matrix <- matrix(
        nrow = nrow(data), 
        ncol = length(response_cols)
    )
    
    names(weight) <- response_cols
    colnames(error_matrix) <- response_cols
    
    for (r in response_cols) {
        response <- data[[r]]
        prediction <- numeric(nrow(data))
        
        for (k in 1:kfold) {
            test <- data_split[[k]]
            train <- data_split[setdiff(1:kfold, k)] |> 
                bind_rows() |> 
                select(-..row, -..fold)
            
            y <- train[[r]]
            x <- train[names(train) != r]
            
            fit <- fitter(y, x)
            prediction[test$..row] <- predict(fit, newdata = test, type = "response")
        }
        
        error <- (response - prediction)^2
        rmse <- sqrt(mean(error))
        weight[r] <- max(1 - rmse, 0)
        
        error_matrix[, r] <- weight[r] * error
    }
    
    structure(
        rowMeans(error_matrix),
        error_matrix = error_matrix,
        weight = weight,
        fold = data$..fold
    )
}

# Calculate Outlier Factor (of)
# We don't fit variables that are harder to predict
songs_also$of <- songs_also |> 
    select(-ID) |> 
    also(dont_fit = c("instrumentalness", "speechiness", "loudness"), seed = 112358)

# Make sure of doesn't depend on fold 
boxplot(songs_also$of ~ attr(songs_also$of, "fold"))
kruskal.test(songs_also$of ~ attr(songs_also$of, "fold"))

# Histogram of outlier_factor
hist(songs_also$of, breaks = 50)

cutmin <- 0.06
cutmax <- 0.12
mean(songs_also$of < cutmin)
mean(songs_also$of < cutmax)

# Outlier factor does not seem to depend too much on a single variable.
# It is a good multivariate metric
songs_also |> select(where(is.numeric), -of) |> cor(songs_also$of)

# Most likely outliers and explanations
songs_also |> 
    mutate(error_matrix = attr(of, "error_matrix")) |> 
    filter(of > cutmin) |> 
    rowwise() |> 
    mutate(most_extraordinary = colnames(error_matrix)[which.max(error_matrix)]) |> 
    select(ID, of, most_extraordinary, error_matrix) |> 
    arrange(-of)

# We standarize of between [0, 1]
songs_also$also_of <- scales::rescale(
    songs_also$of,
    from = c(cutmin, cutmax),
    to = c(0, 1)
) |> 
    pmax(0) |> 
    pmin(1)

# Since we didn't predict instrumentalness or speechiness, 
# we manually detect bivariate outliers
songs_also <- songs_also |> mutate(
    instr_of = 
        (instrumentalness > 0.01) * 
        (speechiness > 0.50) *
        pmax(speechiness, instrumentalness),
    
    # We take the maximum of the two outlier factors
    combined_of = pmax(also_of, instr_of)
)

ggplot(songs_also, aes(x = instrumentalness, y = speechiness, color = combined_of)) +
    geom_point(size = 2) +
    scale_color_gradient(low = "black", high = "royalblue1") +
    theme_minimal()

# Percentage of observations with 
mean(songs_also$combined_of > 0)

# Finally, convert outlier factor into a weight to use in models later.
songs$outlier_weight <- 1 - songs_also$combined_of
saveRDS(songs, file = "data/songs_outlied.RDS")