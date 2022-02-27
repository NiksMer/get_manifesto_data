# Setup
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

# Daten laden
df <- read_csv("complete_data.csv")

# Country
df %>%
    group_by(countryname) %>%
    summarise(n_saetze=n(),n_wahlprogramme=n_distinct(corpus_code)) %>%
    print()
