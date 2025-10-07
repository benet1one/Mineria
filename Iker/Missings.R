library(dplyr, warn.conflicts = FALSE)

# First we need to find all the possible missings formats in the data. If everything it's make correctly, 
# in reading.R it has been standarized de missings.

summary(songs)

# If there are some type of missing worng write like "*", or "?", or"NA", or another type of missings wrong format,
# summary should not calculate anything in the variable where there are wrong missing format. As we can see all the
# summary calculates like there are unique missing format. But we need to observe if theere are a numerical missing format,
# If we order the diferents columns we can see if there are some type of irregular number that could represent a missing.
# There are any irrugular number, so we can think that there are any other wrong missing format.

### Imputations ###

