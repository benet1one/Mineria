
library(tidyverse)
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

"We can observe diferent quality of the instances. We can observe instances with 13 missings of a total of 18 attributes 
(not including ID). In reality there are missings in attributes that are not rellevant like signature, common_signature
and song_duration. Even so, there are 10 missings attributes of a total of 15 attributes in some instaces. We need to 
study if this instances are a problem by her high missing ratio."


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
    audio_mode = factor(round(prob_major), labels = c("Minor", "Major"))
)

# Mode Proportion before
mode_prop
# Mode Proportion now
table(songs$audio_mode) |> proportions()
