
library(tidyverse)
library(visdat)
source("reading.R")

" EXecute after reading.R code"
"(idea for other part) Data integration is not rellevant in this project since there is only a single data source."

"Once the data has been integrated, we can summarize the characteristics of attributes (means, miniums, maximums etc.)"
summary(songs)

"Things that we can observe:
  1. Same cuantity of NA's in every attribute, but they are located randomly.
  2. What is '(Other)' value on key and signature attributes?
"

songs_long <- songs %>% 
  pivot_longer(
    cols = where(is.numeric),
    names_to = "attributes",
    values_to = "values"
  )

ggplot(songs_long, aes(x = values)) +
  geom_histogram() +
  facet_wrap(~ attributes, scales = "free") +
  theme_minimal()

"By analyzing missing values, we can detect potential data errors, such as values that are clearly incorrect. 
When an error is identified, the corresponding value should be replaced with a missing value. The most common data 
errors are outliers that represent impossible or unrealistic values. (this should be make in other part)"
"I want to give alternative ideas to histograms: Box plots, histogram of 2 variables, scatterplots, colored 
scatterplots (acatterplots with groups), labeled scatterplot (scatterplot with labels) and bubble plot.
"

### Real start of missings values ###

"First of all we want to see the quality of our instances"
songs_modified <- songs %>% 
  mutate(missing_quantity = rowSums(is.na(.)))

summary(songs_modified$missing_quantity)

"We can observe that there are 10 missings attributes of a total of 14 attributes in some instaces. We need to 
study if this instances are a problem by her high missing ratio."

"The reasons why we want to replace the missing values are:
1. The mining method may have issues handling missing data.
2. If we want to aggregate the data for other mining processes, the missing values can prevent us from aggregating 
the data correctly.
2. If the mining method has an automatic way of handling missing values, it will most likely eliminate the instances 
with missing data, which can introduce bias. Other methods may perform automatic substitutions, but these replacements 
might not be appropriate for our specific context or because the mining method doesn’t have access to all the relevant 
information from other attributes or tables."

"We have five options to handle missing values:
1. Ignore: Some algorithms are robust to missing values (for example, decision trees).
2. Eliminate an attribute: When a column has an extremely high proportion of missing values, sometimes it’s better 
to remove it entirely if it cannot be properly recovered.
3. Filter out the row: This may introduce bias, since the reasons for missing data are often related to specific 
cases or patterns in the dataset.
4. Replace the value: We can replace missing values manually (if there are only a few) or automatically using 
methods that preserve the mean or variance (globally or within specific groups). A more sophisticated approach 
is to predict the missing value using other examples — for instance, through regression models or other imputation 
techniques.
5. Segment: The tuples are segmented based on the values they have available. Different models are then built for 
each segment and later combined.

The most common approach is value replacement, but it comes with two main problems:
First, we lose information, because the imputed value becomes indistinguishable from the original values, and we lose 
the signal that it was missing.
Second, we fabricate information, since the imputed value might be inaccurate, introducing potential errors.

A good solution is to create an additional logical (boolean) attribute indicating whether the original value was 
missing or not. Many data mining methods can take this extra attribute into account to handle missing values more 
carefully."

# (Iker)
"
- Imputation
- Interpolation (for temporal data), 
- Model-based imputation (predictive model)(regression, local least square imputation, Support Vector Machine, 
    decision tree, artificial neural net),  
- Performing the missing imputation (Maximum Likelihood Estimation),
- EM algorithms (Expectetion-Maximization algorithms),
- Multiples imputation,
- Distance-based methods (k-NN),
- MICE method,
- MIMMI method,
- Other solutions (fuzzy sets, probability distributions or confidence intervals)


Well, it's a list of methods that can be usefull depending of the situation, but we need to decide which are the best
methods for our data. 
Imputation it's easy, but distort the variance and correlations, so we will not use it.
Interpoletion it's one of the best methods when we have temporal data, but iy's not our case.
Model-based imputation take advantage of the relationship between variables for imput with precision, we can use it.
Advanced statistical methods like MLE and EM use statistical theory and are useful, but they are more complicated. 
  Es don't descart to use it.
Multiples imputaton like MICE and MIMMI are useful because preserve the uncertainity of the missings. We can use it.
k-NN is useful because he not need to assume some specific distribution. We can use it.
Other methods are necesary to evaluate if are useful in this case like fuzzy sets, naive bayes, imputation using 
  probability distributions or confidence intervals, etc.

"
library(visdat)
vis_miss(songs)

# (Bernat)
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


# Found Outliers --------------------------------------------------

# Loudness is suspicious, only two values above 0
songs |> arrange(-loudness) |> select(loudness, energy)
plot(energy ~ loudness, data = songs, col = ifelse(songs$loudness > 0, "red", "black"))

# One of them is slightly above zero. Changing its value to zero is convenient later on.
# The other one is a clear univariate outlier. Because it has high energy,
# which is heavily correlated with loudness, we impute it as zero.
songs$loudness[songs$loudness >= 0] <- (-2e-8)


# Mice ------------------------------------------------------------

# Mice allows a predictor matrix with the following interpretation:
# - Rows: Imputed variable
# - Columns: Regressor variables
# A value of 1 means the row variable will be imputed using the column variable.
predictor_matrix <- matrix(1L, nrow = ncol(songs), ncol = ncol(songs))
rownames(predictor_matrix) <- colnames(predictor_matrix) <- names(songs)

# ID and popularity cannot be used as a regressors
# because we will use CART, it's better to use audio_mode as binary than to use prob_major.
dont_regress <- c("ID", "song_popularity", "prob_major")
predictor_matrix[, dont_regress] <- 0L

songs_mice <- mice::mice(
    data = songs,
    m = 3,
    method = "cart",
    predictorMatrix = predictor_matrix,
    seed = 5151,
)

songs_imputed <- mice::complete(songs_mice, action = songs_mice$m) |> 
    as_tibble()

saveRDS(songs_imputed, "data/songs_imputed.RDS")
