
library(dplyr)
source("KPI.R")

songs <- readRDS("data/songs_outlied.RDS")
train <- songs |> slice_sample(prop = 0.7)
test <- songs |> rows_delete(train, by = "ID") |> suppressMessages()

formula <- song_popularity ~ 
    loudness + acousticness + key + tempo + song_duration_ms + 
    danceability + energy + audio_valence + audio_mode

# cat("\nWorst MAPE =", mape(test$song_popularity, predicted = 4))
# max_prediction <- 10

# Default Regression Loss Function: ANOVA ------------------------------

tree_anova <- rpart::rpart(
    formula = formula,
    data = train,
    method = "anova",
    weights = train$outlier_weight,
    control = rpart::rpart.control(
        minbucket = 20,
        maxdepth = 8,
        cp = -1
    )
)

tree_anova
predicted_anova <- predict(tree_anova, test)


# Defining new loss function for rpart: MAPE -------------------------

mape_method <- list(
    init = function(y, offset, parms, wt) {
        summary_fun <- function(yval, dev, wt, ylevel, digits) {
            paste("Prediction =", signif(yval, digits), "| MAPE =", signif(dev, digits))
        }
        
        list(y = y, parms = parms, numresp = 1, numy = 1, summary = summary_fun)
    },
    split = function(y, wt, x, parms, continuous) {
        if (continuous) {
            # Continuous x
            n <- length(y)
            goodness <- rep(0, n - 1)
            
            p_splits <- ppoints(parms$n_splits_continuous, a = 0)
            i_splits <- round(n * p_splits)
            
            for (i in i_splits) {
                l <- 1:i
                r <- (i + 1):n
                
                predicted <- numeric(n)
                predicted[l] <- minimize_weighted_mape(draws = y[l], weights = wt[l])
                predicted[r] <- minimize_weighted_mape(draws = y[r], weights = wt[r])

                loss <- weighted_mape(actual = y, predicted = predicted, weights = wt)
                goodness[i] <- 1/loss
            }
            
            list(goodness = goodness, direction = rep(-1, n - 1))
            
        } else {
            # Categorical x
            medians <- tapply(y, x, median)
            ord <- order(medians)
            ux <- sort(unique(x))
            
            n <- length(y)
            k <- length(ux)
            goodness <- rep(0, k - 1)
            
            for (i in seq_len(k - 1)) {
                l <- x %in% ux[ord][1:i]
                r <- !l
                
                predicted <- numeric(n)
                predicted[l] <- minimize_weighted_mape(draws = y[l], weights = wt[l])
                predicted[r] <- minimize_weighted_mape(draws = y[r], weights = wt[r])
                
                loss <- weighted_mape(actual = y, predicted = predicted, weights = wt)
                goodness[i] <- 1/loss
            }
            
            list(goodness = goodness, direction = ux[ord])
        }
    },
    eval = function(y, wt, parms) {
        full_y <- parms$full_response
        full_wt <- parms$full_weights * 
            parms$prediction_safety * 
            length(y) / length(full_y)
        
        y <- c(y, full_y)
        wt <- c(wt, full_wt)
        
        prediction <- minimize_weighted_mape(y, weights = wt)
        loss <- weighted_mape(actual = y, predicted = prediction, weights = wt)
        list(label = prediction, deviance = loss)
    }
)

tree_custom <- rpart::rpart(
    formula = formula,
    data = train,
    method = mape_method,
    weights = train$outlier_weight,
    parms = list(
        n_splits_continuous = 3,
        full_response = train$song_popularity,
        full_weights = train$outlier_weight,
        prediction_safety = 0.02
    ),
    control = rpart::rpart.control(
        minbucket = 50,
        maxdepth = 8,
        cp = -1
    )
)

tree_custom
predicted_custom <- predict(tree_custom, test)

models <- tibble(
    number_4 = 4,
    anova_tree = predicted_anova,
    custom_tree = predicted_custom,
    ID = test$ID,
) |> 
    tidyr::pivot_longer(
        !ID, 
        names_to = "model", 
        values_to = "predicted"
    ) |> 
    mutate(
        predicted_capped_4 = pmin(predicted, 4),
        predicted_capped_5 = pmin(predicted, 5),
        predicted_capped_6 = pmin(predicted, 6),
        predicted_capped_7 = pmin(predicted, 7),
        predicted_capped_8 = pmin(predicted, 8)
    ) |> 
    rename(predicted_uncapped = predicted) |> 
    tidyr::pivot_longer(
        starts_with("predicted"), 
        names_to = "modified", 
        names_prefix = "predicted_", 
        values_to = "predicted"
    )

models |> 
    group_by(model, modified) |> 
    summarise(
        MAPE = mape(actual = test$song_popularity, predicted = predicted),
        .groups = "drop"
    ) |> 
    distinct(model, MAPE, .keep_all = TRUE) |> 
    mutate(improvement = MAPE[model == "number_4"] - MAPE) |> 
    arrange(MAPE) |> 
    print()


# Custom Tree Capped at 6 gives consistently good results, 
# although ANOVA Capped at 4 beats it sometimes

# Train using the full dataset
tree_custom_full <- rpart::rpart(
    formula = formula,
    data = songs,
    method = mape_method,
    weights = songs$outlier_weight,
    # Increase n_splits for continuous variables
    parms = list(
        n_splits_continuous = 3,
        full_response = songs$song_popularity,
        full_weights = songs$outlier_weight,
        prediction_safety = 0.02
    ),
    control = rpart::rpart.control(
        minbucket = 50,
        maxdepth = 8,
        cp = -1
    )
)

songs_test <- readRDS("data/songs_test_imputed.RDS")
final_prediction <- tibble(
    id = songs_test$ID,
    song_popularity = predict(tree_custom_full, songs_test) |> 
        pmin(6) |> 
        round()
)

write.csv(
    final_prediction, 
    file = "predictions/custom_tree_correct.csv", 
    row.names = FALSE
)
