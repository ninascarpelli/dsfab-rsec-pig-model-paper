# --- Load Required Libraries ---
# These libraries support data manipulation, date/time handling, and spatial data processing.
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(terra)
library(tidyterra)
library(hms)
library(stringr)

source("pig_model_paper/R/load_data.R")
source("pig_model_paper/R/save_utils.R")

# --- Define Field Season Dates ---
start_field_season <- ymd("2023-06-26")
end_field_season <- ymd("2024-06-09")

# --- Load and Process Feral Pig Take Data ---
feral_pig_take <- load_csv("pig_model_paper/data/raw_pig_removal.csv") %>%
  dplyr::select(taken, lat, long, date, time, sighted, trapped)

feral_pig_take$time[feral_pig_take$time == ""] <- NA
feral_pig_take$time <- as_hms(feral_pig_take$time)
feral_pig_take$date <- ymd(feral_pig_take$date)

feral_pig_take <- feral_pig_take %>%
  filter(date >= start_field_season, date <= end_field_season) %>%
  rename(n_detections_per_site_date_time = taken) %>%
  mutate(source = "feral-pig-take-sheet", sp = "pig", site = "varied") %>%
  dplyr::select(site, date, time, n_detections_per_site_date_time, source, sp, lat, long)


# Ensure detection counts are integers
feral_pig_take$n_detections_per_site_date_time <- as.integer(feral_pig_take$n_detections_per_site_date_time)

# --- Export Final Dataset ---
save_csv(feral_pig_take, "pig_model_paper/data/processed_pig_removal.csv")
