
library(dplyr)
library(ggplot2)
library(FNN)

# --- Divisione train/test per il dataset songs ---

set.seed(123)  # per riproducibilità
songs <- readRDS("data/songs_imputed.RDS")

# Feature selection and weighting in knn_weight_tests.R
knn_weights <- readRDS("knn_weights.RDS")
knn_weights

songs_weighted <- songs |> 
    select(ID, liveness, danceability, audio_valence, energy, acousticness, speechiness)

for (v in names(songs_weighted)) {
    if (v == "ID")
        next
    weight <- knn_weights$weight[knn_weights$variable == v]
    songs_weighted[[v]] <- weight * scale(songs_weighted[[v]]) [, 1]
}

songs_num <- songs |> 
    rows_update(songs_weighted, by = "ID") |> 
    select(liveness, danceability, audio_valence, energy, acousticness, speechiness, 
           key, audio_mode, time_signature, song_popularity) |> 
    # Conversione di tutte le features in numeriche (necessario per KNN)
    fastDummies::dummy_columns() |> 
    # Weighting Categorical Variables
    mutate(across(starts_with("key_"), \(key) sqrt(1.65) * key)) |> 
    mutate(across(starts_with("audio_mode_"), \(mode) sqrt(0.27) * mode)) |> 
    mutate(ID = songs$ID, .before = 1)

test <- songs_num |> 
    group_by(audio_mode) |> 
    slice_sample(prop = 0.3) |> 
    ungroup()

train <- songs_num |> rows_delete(test, by = "ID") |> suppressMessages()
test  <- test  |> select(where(is.numeric), -ID)
train <- train |> select(where(is.numeric), -ID)

# Nome della colonna target
target_col <- "song_popularity"

# Indice della colonna target
ind_col <- which(names(songs_num) == target_col)

# Separazione features e target
X_train <- train[, -ind_col]
X_test <- test[, -ind_col]

y_train <- train$song_popularity
y_test <- test$song_popularity

# --- Regressione KNN per song_popularity ---

# Prova KNN
pred <- FNN::knn.reg(
    train = X_train,
    test  = X_test,
    y     = y_train,
    k     = 5
)$pred

head(pred)

# --- Funzione per calcolare MAPE ---
mape <- function(actual, predicted) {
    mean(abs(predicted - actual) / pmax(actual, 1))
}

# Calcola mape (Mean absolute percentage error)
cat("MAPE:", mape(y_test, pred), "\n")

# Correlazione tra predetto e reale
correlation <- cor(pred, y_test)
cat("Correlazione:", correlation, "\n")


# --- Funzione per eseguire KNN e restituire MAPE ---
make_knn_pred <- function(k = 1, training, predicting, 
                          valueTrain, valueTrue) {
    pred <- FNN::knn.reg(
        train = training,
        test  = predicting,
        y     = valueTrain,
        k     = k
    )$pred
    
    mape(actual = valueTrue, predicted = pred)
}

# --- Valori di k da testare ---
k_values <- c(1, 3, 5, 7, 9, 25)

# --- Calcolo MAPE sul TRAIN (quanto bene apprende sui dati noti) ---
knn_train_mape <- sapply(
    k_values,
    make_knn_pred,
    training   = X_train,
    predicting = X_train,
    valueTrain = y_train,
    valueTrue  = y_train
)

# --- Calcolo MAPE sul TEST (quanto generalizza) ---
knn_test_mape <- sapply(
    k_values,
    make_knn_pred,
    training   = X_train,
    predicting = X_test,
    valueTrain = y_train,
    valueTrue  = y_test
)

# --- Determina il k con errore minimo ---
best_k <- k_values[which.min(knn_test_mape)]
best_k

cat("Miglior k:", best_k, "\n")
cat("MAPE train:", knn_train_mape[which.min(knn_test_mape)], "\n")
cat("MAPE test :", min(knn_test_mape), "\n")

plot(k_values, knn_test_mape, type = "b", pch = 19, col = "blue",
     main = "MAPE KNN - Dataset Songs",
     xlab = "k (numero di vicini)",
     ylab = "MAPE (test set)")

final_pred <- FNN::knn.reg(
    train = X_train,
    test  = X_test,
    y     = y_train,
    k     = best_k
)$pred

df_mape <- data.frame(
    k = k_values,
    mape_test = knn_test_mape,
    mape_train = knn_train_mape
)

library(tidyr)
df_mape_long <- df_mape |> 
    pivot_longer(cols = c(mape_test, mape_train),
                 names_to = "tipo", values_to = "mape")

ggplot(df_mape_long, aes(x = factor(k), y = mape, fill = tipo)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "mape per diversi valori di k",
         x = "Numero di vicini (k)",
         y = "mape") +
    scale_fill_manual(values = c("steelblue", "orange")) +
    theme_minimal()


# --- Make the prediction with the full training set ---

songs_full <- readRDS("data/songs_all_imputed.RDS") |> 
    mutate(ID = if_else(training, paste("training", ID), paste("test", ID)))

songs_weighted <- songs_full |> 
    select(ID, liveness, danceability, audio_valence, energy, acousticness, speechiness)

for (v in names(songs_weighted)) {
    if (v == "ID")
        next
    weight <- knn_weights$weight[knn_weights$variable == v]
    songs_weighted[[v]] <- weight * scale(songs_weighted[[v]]) [, 1]
}

songs_num <- songs_full |>  
    rows_update(songs_weighted, by = "ID") |> 
    select(liveness, danceability, audio_valence, energy, acousticness, speechiness, 
           key, audio_mode, time_signature, song_popularity, training) |> 
    # Conversione di tutte le features in numeriche (necessario per KNN)
    fastDummies::dummy_columns() |> 
    # Weighting Categorical Variables
    mutate(across(starts_with("key_"), \(key) sqrt(1.65) * key)) |> 
    mutate(across(starts_with("audio_mode_"), \(mode) sqrt(0.27) * mode)) |> 
    mutate(ID = songs_full$ID, .before = 1)

train <- songs_num |> filter( training) |> select(where(is.numeric))
test  <- songs_num |> filter(!training) |> select(where(is.numeric))

y_train <- train |> _$song_popularity
X_train <- train |> select(-song_popularity)
X_test  <- test  |> select(-song_popularity)

pred <- FNN::knn.reg(
    train = X_train,
    test = X_test,
    y = y_train,
    k = best_k
)$pred

prediction <- tibble(
    id = 1:length(pred),
    song_popularity = as.integer(pred)
)

write.csv(prediction, file = "predictions/knn.csv", row.names = FALSE)
