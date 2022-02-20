# Packges laden
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(manifestoR))
suppressPackageStartupMessages(library(tm))

# Funktion laden
source("functions.R")

# Anwendung
df <- get_training_data(
  api_key = "manifesto_apikey.txt",
  start_day = lubridate::date("2019-01-01"),
  country_vec = c(
    "United Kingdom"
  ),
  reliability = 0.0,
  seed=1
)

# Output
print(sample_n(df,size=10))