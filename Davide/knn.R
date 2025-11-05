
library(dplyr)
library(ggplot2)
library(FNN)

songs <- readRDS("data/songs_imputed.RDS")
songs_oversampled <- readRDS("data/songs_oversampled.RDS")

distance_daisy <- songs |>
    slice_sample(n = 4000) |> 
    select(-ID, -song_popularity, -key) |>
    cluster::daisy("gower")

distance_mat <- as.matrix(distance_daisy)

k_exploration <- tibble(
    V1 = sample.int(nrow(distance_mat)),
    V2 = sample.int(nrow(distance_mat))
) |> 
    rowwise() |>
    mutate(
        distance = distance_mat[V1, V2],
        pop_diff = abs(
            songs$song_popularity[V1] - songs$song_popularity[V2]
        )
    ) |>
    ungroup() |>
    mutate(distance_cat = cut(distance, breaks = 10)) |>
    print()


ggplot(k_exploration, aes(x = distance, y = pop_diff)) +
    geom_point(alpha = 0.1) +
    geom_smooth()

ggplot(k_exploration, aes(x = distance_cat, y = pop_diff)) +
    geom_boxplot()

# --- Divisione train/test per il dataset songs ---

set.seed(123)  # per riproducibilità

# Nome della colonna target
target_col <- "song_popularity"

# Indice della colonna target
ind_col <- which(names(songs) == target_col)

songs_num <- songs_oversampled |>  
    select(-ID) |> 
    # Applica la standardizzazione
    mutate(across(.fns = scale, c(where(is.numeric), -song_popularity))) |> 
    # Conversione di tutte le features in numeriche (necessario per KNN)
    fastDummies::dummy_columns() |> 
    mutate(ID = songs_oversampled$ID, .before = 1)


test <- songs_num |> 
    # Filter out oversampled pairs
    filter(!stringr::str_detect(ID, "-")) |> 
    group_by(time_signature) |> 
    slice_sample(prop = 0.3) |> 
    ungroup()

train <- songs_num |> 
    filter(!is.element(ID, test$ID))

test <- test |> select(where(is.numeric))
train <- train |> select(where(is.numeric))

# Separazione features e target
X_train <- train |> select(-song_popularity)
X_test <- test |> select(-song_popularity)

y_train <- train$song_popularity
y_test <- test$song_popularity

# --- Regressione KNN per song_popularity ---

# Prova KNN
pred <- FNN::knn.reg(
  train = X_train,
  test  = X_test,
  y     = y_train,
  k     = 5
)

head(pred$pred)

# Calcola RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((pred$pred - y_test)^2))
cat("RMSE:", rmse, "\n")

# Correlazione tra predetto e reale
correlation <- cor(pred$pred, y_test)
cat("Correlazione:", correlation, "\n")

# --- Funzione per calcolare RMSE ---
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# --- Funzione per eseguire KNN e restituire RMSE ---
make_knn_pred <- function(k = 1, training, predicting, 
                          valueTrain, valueTrue) {
  pred <- FNN::knn.reg(
    train = training,
    test  = predicting,
    y     = valueTrain,
    k     = k
  )$pred
  
  rmse(actual = valueTrue, predicted = pred)
}

# --- Valori di k da testare ---
k_values <- c(1, 5, 10, 25)

# --- Calcolo RMSE sul TRAIN (quanto bene apprende sui dati noti) ---
knn_train_rmse <- sapply(
  k_values,
  make_knn_pred,
  training   = X_train,
  predicting = X_train,
  valueTrain = y_train,
  valueTrue  = y_train
)

# --- Calcolo RMSE sul TEST (quanto generalizza) ---
knn_test_rmse <- sapply(
  k_values,
  make_knn_pred,
  training   = X_train,
  predicting = X_test,
  valueTrain = y_train,
  valueTrue  = y_test
)

# --- Determina il k con errore minimo ---
best_k <- k_values[which.min(knn_test_rmse)]

cat("Miglior k:", best_k, "\n")
cat("RMSE train:", knn_train_rmse[which.min(knn_test_rmse)], "\n")
cat("RMSE test :", min(knn_test_rmse), "\n")

plot(k_values, knn_test_rmse, type = "b", pch = 19, col = "blue",
     main = "RMSE KNN - Dataset Songs",
     xlab = "k (numero di vicini)",
     ylab = "RMSE (test set)")

final_pred <- FNN::knn.reg(
  train = X_train,
  test  = X_test,
  y     = y_train,
  k     = best_k
)$pred

df_rmse <- data.frame(
  k = k_values,
  RMSE_test = knn_test_rmse,
  RMSE_train = knn_train_rmse
)

library(tidyr)
df_rmse_long <- df_rmse |> 
  pivot_longer(cols = c(RMSE_test, RMSE_train),
               names_to = "tipo", values_to = "RMSE")

ggplot(df_rmse_long, aes(x = factor(k), y = RMSE, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE per diversi valori di k",
       x = "Numero di vicini (k)",
       y = "RMSE") +
  scale_fill_manual(values = c("steelblue", "orange")) +
  theme_minimal()
