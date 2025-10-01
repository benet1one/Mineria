
library(dplyr, warn.conflicts = FALSE)

## I don't think it makes sense to use this variable but sure.
KEY_NOTES <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")

## Also could make this distinction but this could have issues.
# KEY_NOTES <- list(
#     major = c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"),
#     minor = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "Bb", "B")
# )

# Standarizes signature to major relative key.
# For instancce, F Major and D minor have the same signature.
major_relative <- function(key, mode) {
    if_else(
        mode == "Major",
        true = key,
        false = {
            int_key <- as.integer(key)
            major_rel <- (int_key + 2) %% 12
            factor(major_rel, levels = 0:11, labels = KEY_NOTES)
        }
    )
}

songs <- read.csv("data/train.csv") |> 
    as_tibble() |> 
    mutate(
        # Minor or Major
        audio_mode = factor(audio_mode, levels = 0:1, labels = c("Minor", "Major")),
        # Key
        key = factor(key, levels = 0:11, labels = KEY_NOTES),
        # Signature
        signature = major_relative(key, audio_mode),
        # Duration in human readable format
        song_duration = hms::hms(song_duration_ms / 1000)
    )

# Obvious error, tempo can not be zero
songs$tempo[songs$tempo == 0L] <- NA
# Next lowest value is feasible
min(songs$tempo, na.rm = TRUE)

# Values 0 and 1 are not valid time signatures.
# Metadata confirms values should range from 3 to 7, but data only contains up to 5.
# I convert it into factor because, while the information is technically numerical, 
# its interpretation is not.
songs$time_signature <- factor(songs$time_signature, levels = 3:5)

# Making sure signature is correct
songs |> filter(signature == "F") |> count(key, audio_mode)
songs |> filter(signature == "C") |> count(key, audio_mode)

rm(KEY_NOTES)
