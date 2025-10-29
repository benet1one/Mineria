library(tidyverse)
library(visdat)
library(naniar)
library(ggplot2)

setwd("D:/Iker/VSC-Python/Mineria")
source("reading.R")

# Starting of the missing treatment --------------------------------------------
# 1. Identify which variables and observations have missing values.
## Variables
songs %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  pivot_longer(everything(), 
               names_to = "Variables", 
               values_to = "N_missings") %>% 
  arrange(desc(N_missings)) %>% 
  print(n = Inf)

## Observations
songs %>% 
  mutate(N_missings = rowSums(is.na(.))) %>% 
  arrange(desc(N_missings)) %>% 
  select(N_missings)

songs %>%
  mutate(N_missings = rowSums(is.na(.))) %>%
  count(N_missings, name = "n_observaciones") %>%
  arrange(desc(N_missings))

# 2. Standardize missing code
## Pre-analysis to locate rare thinks.
songs %>%
  summarise(across(where(is.numeric),
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_") %>%
  mutate(across(c(min, max), as.numeric)) %>%
  arrange(desc(max))

## Solving rare cases.
songs %>%
  summarise(across(where(is.numeric),
                   list(min = ~min(., na.rm = TRUE),
                        q1  = ~quantile(., 0.25, na.rm = TRUE),
                        median = ~median(., na.rm = TRUE),
                        mean = ~mean(., na.rm = TRUE),
                        q3  = ~quantile(., 0.75, na.rm = TRUE),
                        max = ~max(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_") %>%
  arrange(desc(max))

"Loudness is suspicious, only two values above 0"
songs |> arrange(-loudness) |> select(loudness, energy)
plot(energy ~ loudness, data = songs, col = ifelse(songs$loudness > 0, "red", "black"))

"One of them is slightly above zero. Changing its value to zero is convenient later on.
The other one is a clear univariate outlier. Because it has high energy,
which is heavily correlated with loudness, we impute it as zero."
songs$loudness[songs$loudness >= 0] <- (-2e-8)

numeric_data <- songs %>% select(where(is.numeric))
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
print(cor_matrix)

plot(audio_valence ~ energy, data = songs)

## Generic transformation of other format missings.
missing_expressions <- c("?", "NA ", "N/A", "none", "None", ".", "", " ")

songs_original <- songs

songs <- songs %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(. %in% missing_expressions, NA, .)
    )
  )

na_comparison <- tibble(
  variable = names(songs),
  antes = map_int(songs_original, ~ sum(is.na(.))),
  despues = map_int(songs, ~ sum(is.na(.))),
  nuevos_NA = despues - antes
)

na_comparison %>%
  filter(nuevos_NA > 0)

na_comparison
"There are not an irregular format of missing expresion."

# 3. Count missing values by column and row to evaluate the fiability.
## Variables.
missing_vars <- songs %>%
  summarise(across(everything(),
                   list(n_miss = ~ sum(is.na(.)),
                        prop_miss = ~ mean(is.na(.))))) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(n_miss|prop_miss)$"
  ) %>%
  arrange(desc(prop_miss))

## Observations
missing_rows <- songs %>%
  mutate(n_miss = rowSums(is.na(.)),
         prop_miss = n_miss / ncol(songs))

missing_rows %>%
  ggplot(aes(x = prop_miss)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of proportion missings by instance",
       x = "Proportion  missing values",
       y = "Frequency")

# 4. Analyse missing patron
vis_miss(songs)

gg_miss_upset(songs)

test_MCAR <- mcar_test(songs)
test_MCAR$p.value

"As p-value > 0.05, we can not reject H_0, so the missings are MCAR."

# Imputation of the missing ----------------------------------------------------
"We can impute mode from key signature"

mode_prop <- table(songs$audio_mode) |> proportions()
key_mode_prop <- table(songs$key, songs$audio_mode) |> proportions(margin = 1)

songs <- songs |> mutate(
  prob_major = if_else(
    !is.na(audio_mode),
    as.numeric(audio_mode == "Major"),
    if_else(
      !is.na(key),
      key_mode_prop[key, "Major"],
      mode_prop["Major"]
    )
  ),
  audio_mode = factor(prob_major > 0.6, labels = c("Minor", "Major"))
)

# Mode Proportion before
mode_prop
# Mode Proportion now
table(songs$audio_mode) |> proportions()
  
ggplot(songs, aes(x = loudness, y = energy)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Relación Loudness vs Energy",
       x = "Loudness",
       y = "Energy")

lm_model <- lm(energy ~ loudness, data = songs)
summary(lm_model)

# Revisar residuos
plot(lm_model, which = 1)  # Residuals vs Fitted
plot(lm_model, which = 2)  # Q-Q plot

# Transform energy and acousticness
logit <- \(x) log(x / (1 - x))
logit_inv <- \(x) exp(x) / (1 + exp(x))
songs$acousticness <- logit(songs$acousticness)

# Mice allows a predictor matrix with the following interpretation:
# - Rows: Imputed variable
# - Columns: Regressor variables
# A value of 1 means the row variable will be imputed using the column variable.
predictor_matrix <- matrix(1L, nrow = ncol(songs), ncol = ncol(songs))
rownames(predictor_matrix) <- colnames(predictor_matrix) <- names(songs)

# ID and popularity cannot be used as a regressors.
predictor_matrix[, c("ID", "song_popularity")] <- 0L
# Probabily bad to regress by key.
predictor_matrix[, "key"] <- 0L
# Probabily better to use audio_mode.
predictor_matrix[, "prob_major"] <- 0L

# Most variables are correctly imputed with predictive mean, matching, but some aren't.
methods <- character(ncol(songs))
names(methods) <- names(songs)
methods[] <- "pmm"
methods["loudness"] <- "lasso.norm"
methods["instrumentalness"] <- "rf"

# Acousticness is very annoying, I will try to make it depend only on energy, loudness and the
# categorical variables.
methods["acousticness"] <- "pmm"
predictor_matrix["acousticness", ] <- 0L
predictor_matrix["acousticness", c("energy", "loudness", "time_signature", "audio_mode")] <- 1L

# Sensitively imputed variables should not depend on other sensitively imputed variables
# unless their correlation is strong.
predictor_matrix["loudness", "speechiness"] <- 0L
predictor_matrix[c("loudness", "instrumentalness", "speechiness"), "acousticness"] <- 0L

songs_mice <- mice::mice(
  data = songs,
  m = 5,
  method = methods,
  predictorMatrix = predictor_matrix,
  seed = 5151,
)

songs_imputed <- mice::complete(songs_mice, action = songs_mice$m) |> 
  mutate(loudness = pmin(loudness, 0), acousticness = logit_inv(acousticness)) |> 
  as_tibble()

saveRDS(songs_imputed, "data/songs_imputed.RDS")


