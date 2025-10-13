
source("reading.R")

songs$duration_qual <- cut(
    songs$song_duration_ms, 
    breaks = c(0, 3*60e3, 4*60e3, Inf),
    labels = c("Short", "Medium", "Long")
)

songs_also <- songs |> 
    select(-key, -time_signature, -song_duration_ms) |> 
    mutate(
        major = audio_mode == "Major", audio_mode = NULL, 
        song_popularity = song_popularity / 100,
        loudness = -loudness / sd(loudness, na.rm = TRUE),
        tempo = scale(tempo)[, 1]
    )

# Delete once imputed
songs_also <- songs_also[complete.cases(songs_also), ]


fit_model <- function(y, data) {
    if (is.logical(y)) {
        glm(y ~ ., family = binomial("logit"), data = data)
        
    } else if (min(y) >= 0  &&  max(y) <= 1) {
        glm(y ~ ., family = quasibinomial("logit"), data = data)
        
    } else if (min(y) >= 0) {
        # lm(log(y) ~ ., data = data)
        glm(y ~ ., family = quasipoisson("log"), data = data)
        
    } else {
        lm(y ~ ., data = data)
    }
}
also <- function(data, kfold = 5, fitter = fit_model, omit_cols = c(), seed = NULL) {
    if (!is.null(seed)) 
        set.seed(seed)
    
    response_cols <- data |> 
        select(where(is.numeric), where(is.logical), -any_of(omit_cols)) |> 
        names()
        
    folds <- rep_len(1:kfold, nrow(data))[sample.int(nrow(data))]
    data <- data |> mutate(..row = 1:n(), .before = 1)
    data_split <- data |> group_split(!!folds)
    
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
                select(-..row)
            
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
        weight = weight,
        fold = folds
    )
}

# Calculate Outlier Factor (of)
songs_also$of <- also(songs_also, omit_cols = "ID", seed = 112358)

# Make sure of doesn't depend on fold 
boxplot(songs_also$of ~ attr(songs_also$of, "fold"))

# Histogram of of
hist(songs_also$of, breaks = 20)

# 0.14 is a nice cutoff point
mean(songs_also$of < 0.15)
songs_also |> select(where(is.numeric)) |> cor(songs_also$of)

# Most likely outliers
songs_also |> arrange(-of) |> relocate(of, .after = ID)
