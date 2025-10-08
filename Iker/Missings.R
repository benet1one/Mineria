library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
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



