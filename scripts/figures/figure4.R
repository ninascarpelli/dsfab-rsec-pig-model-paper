# Load required libraries
library(lubridate)
library(stringr)
library(tidyverse)
library(tidyterra)
library(terra)
library(raster)

# Set recogniser version
recogniser_version <- "ensemble"

# Define date range for filtering
date_start <- dmy("26/06/2023")
date_end <- dmy("08/06/2024")

# ---- Detections presence/absence ----
# Read and prepare detection data
df <- read.csv("data/processed_detections.csv") %>%
  mutate(week = week(date)) %>%
  filter(date >= date_start & date <= date_end)

# Convert date column to Date format
df$date <- ymd(df$date)

# Prepare camera trap data: group by site and week, sum detections
df_prep_ct <- df %>%
  filter(source == "ct") %>%
  group_by(site, week) %>%
  summarise(count = sum(n_detections_per_site_date_time)) %>%
  rename(ct = count) %>%
  ungroup()

# Prepare recogniser data: group by site and week, sum detections
df_prep_recog <- df %>%
  filter(source == "recogniser") %>%
  group_by(site, week) %>%
  summarise(count = sum(n_detections_per_site_date_time)) %>%
  rename(recogniser = count) %>%
  ungroup()

# Merge recogniser and camera trap data, fill NAs with 0
both <- full_join(df_prep_recog, df_prep_ct, by = c("week", "site")) %>%
  mutate(ct = case_when(is.na(ct) ~ 0,
                        TRUE ~ ct),
         recogniser = case_when(is.na(recogniser) ~ 0,
                                TRUE ~ recogniser))

# ---- Expand survey data using metadata ----
# Step 1: Create a full sequence of dates and weeks for each site
expanded_dates <- df %>%
  rowwise() %>%
  mutate(date = list(seq.Date(from = date_start, to = date_end, by = "day"))) %>%
  unnest(date) %>%
  mutate(week = week(date)) %>%
  dplyr::select(site, week) %>%
  distinct(site, week)

# Step 2: Merge expanded survey data with detection data
completed_survey <- expanded_dates %>%
  left_join(both, by = c("site", "week"))

# Step 3: Fill missing values and classify detection method and presence
completed_survey <- completed_survey %>%
  mutate(ct = ifelse(is.na(ct), 0, ct),
         recogniser = ifelse(is.na(recogniser), 0, recogniser)) %>%
  mutate(method = case_when(recogniser >= 1 & ct >= 1 ~ "both",
                            recogniser >= 1 & ct == 0 ~ "recogniser",
                            recogniser == 0 & ct >= 1 ~ "ct",
                            recogniser == 0 & ct == 0 ~ "both",
                            TRUE ~ "nothing")) %>%
  mutate(presence = case_when(recogniser >= 1 | ct >= 1 ~ 1,
                              TRUE ~ 0))

# Check week extraction
week(df$date)

# ---- Plot detections over time ----
detections_time <- df %>%
  group_by(source, date) %>%
  mutate(summary = n()) %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = summary, fill = source)) +
  scale_fill_manual(values = c("green", "magenta"), labels = c("Camera trap", "Acoustics")) +
  labs(y = "Overall number of detections per day", x = "Date", fill = "Monitoring method", title = "A") +
  theme_bw() +
  theme(legend.position = "none")

# ---- Spatial plot of detections ----
# Load shapefile for Yourka boundaries
yourka_boundaries <- vect("data/yourka_shp/yourka-boundaries.shp")

# Convert detection data to spatial object
spatial <- vect(df, geom = c("longitude", "latitude"), crs = "epsg:4326")

# Plot spatial detections grouped by source, month, and site
detections_spatial <- spatial %>%
  group_by(source, month(date), site) %>%
  mutate(summary = n()) %>%
  ggplot() +
  geom_spatvector(aes(size = summary, colour = source)) +
  scale_colour_manual(values = c("green", "magenta"), labels = c("Camera trap", "Acoustic Recorder")) +
  labs(size = "Number of detections (reference sizes only)", colour = "Monitoring method", title = "B") +
  geom_spatvector(data = yourka_boundaries, fill = NA) +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  facet_wrap(year(date) ~ month(date, label = TRUE), nrow = 1)

# ---- Combine plots and save ----
grid_plot <- gridExtra::grid.arrange(detections_time, detections_spatial, nrow = 2)

# Save the combined plot
ggsave("results/figures/figure4.tiff", grid_plot, width = 10, height = 8)





