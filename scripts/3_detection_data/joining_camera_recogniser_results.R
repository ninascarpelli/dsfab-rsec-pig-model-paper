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

# --- Load Recogniser Results ---
recogniser_results <- load_csv("pig_model_paper/data/processed_ensemble_recogniser_results.csv")

# --- Define Field Season Dates ---
start_field_season <- ymd("2023-06-26")
end_field_season <- ymd("2024-06-09")

# --- Process Recogniser Data ---
temporal_summary <- recogniser_results %>%
  rename(date_time = dt) %>%
  mutate(date_time = gsub(".wav", "", date_time)) %>%
  separate(date_time, into = c("date", "time"), sep = "T", remove = FALSE) %>%
  separate(date_time, into = c("dt", "tz"), sep = "[+]", remove = FALSE) %>%
  separate(time, into = c("time", "c1"), sep = "[+]", remove = FALSE) %>%
  dplyr::select(site, date, time, n_detections_per_site_date_time, source, sp)

# Convert date and time to proper formats
temporal_summary$date <- ymd(temporal_summary$date)
temporal_summary$time <- paste0(substr(temporal_summary$time, 1, 2), ":", substr(temporal_summary$time, 3, 4), ":", substr(temporal_summary$time, 5, 6))
temporal_summary$time <- as_hms(temporal_summary$time)

# --- Load and Process Spatial Metadata ---
metadata <- load_csv("pig_model_paper/data/processed_monitoring_points.csv") %>% 
  dplyr::select(site, latitude, longitude)

# --- Load and Process Camera Trap Data ---
ct <- load_csv("pig_model_paper/data/raw_camera_trap_results.csv") %>%
  dplyr::select(-obs)

ct$time <- as_hms(ct$time)
ct$date <- ymd(ct$date)

# removing traps from the detections
ct <- ct %>%
  distinct(date, time, site, .keep_all = TRUE) %>%
  mutate(deployment = case_when(
    date >= "2023-06-28" & date <= "2023-08-10" ~ "1-Jun",
    date > "2023-08-10" & date <= "2023-10-04" ~ "2-aug",
    TRUE ~ "3-oct"
  )) %>%
  mutate(trap = case_when(
    site == "site-i" & deployment %in% c("1-Jun", "2-aug") ~ "trap",
    site == "site-j" & deployment %in% c("1-Jun", "2-aug") ~ "trap",
    site == "site-k" & deployment == "3-oct" ~ "trap",
    TRUE ~ "no-trap"
  )) %>%
  filter(trap == "no-trap") %>%
  dplyr::select(site, sp, date, time, source, n_detections_per_site_date_time)

# --- Combine Recogniser and Camera Trap Data ---
detections_all <- bind_rows(temporal_summary, ct) %>%
  left_join(metadata, by = "site")

# Ensure detection counts are integers
detections_all$n_detections_per_site_date_time <- as.integer(detections_all$n_detections_per_site_date_time)

# --- Export Final Dataset ---
save_csv(detections_all, "pig_model_paper/data/processed_detections.csv")
