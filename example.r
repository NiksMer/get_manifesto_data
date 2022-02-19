# Packges laden
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(manifestoR)
library(tm)

# Funktion laden
source("functions.R")

# Anwendung
df <- get_training_data(
  api_key = "manifesto_apikey.txt",
  start_day = lubridate::date("2019-01-01"),
  country_vec = c(
    "United Kingdom"
  ),
  reliability = 0.0
)

# Output
print(sample_n(df,size=10))