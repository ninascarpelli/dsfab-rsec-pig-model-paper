#probability of detection for camera trap and acoustics using occupancy modelling

set.seed(123)

# --- Load Required Libraries ---
library(unmarked)     # For occupancy modeling
library(lubridate)    # For date manipulation
library(stringr)      # For string operations
library(tidyverse)    # For data wrangling and visualization
library(tidyterra)    # For tidy spatial data handling
library(terra)        # For raster and spatial data
library(raster)       # For legacy raster support

source("R/load_data.R")
source("R/save_utils.R")

# --- Load Detection Data and Add Week Column ---
df <- load_csv("data/processed_detections.csv") %>%
  mutate(week = week(date))

# --- Load Metadata for Monitoring Sites ---
metadata <- load_csv("data/processed_monitoring_points.csv")

sites <- unique(metadata$site)

# --- Convert Date Column to Date Format ---
df$date <- ymd(df$date)

#for the detection data you'll need one dataframe with the detections per day + methods per day for each site, being the methods: recog, ct or both

# --- Prepare camera trap Detection Data (Summarised Weekly) ---
df_prep_ct <- df %>%
  filter(source == "ct") %>%
  group_by(site, week) %>%
  summarise(count = sum(n_detections_per_site_date_time), .groups = "drop") %>%
  rename(ct = count) %>%
  ungroup()

# --- Prepare Recogniser Detection Data (Summarised Weekly) ---
df_prep_recog <- df %>%
  filter(source == "recogniser") %>%
  group_by(site, week) %>%
  summarise(count = sum(n_detections_per_site_date_time), .groups = "drop") %>%
  rename(recogniser = count)
 
# Merge two data frames by 'week' and 'site', then replace NAs with 0s
both <- full_join(df_prep_recog, df_prep_ct, by = c("week", "site")) %>%
  mutate(
    # If 'ct' is NA, replace with 0; otherwise keep original value
    ct = case_when(
      is.na(ct) ~ 0,
      TRUE ~ ct
    ),
    # If 'recogniser' is NA, replace with 0; otherwise keep original value
    recogniser = case_when(
      is.na(recogniser) ~ 0,
      TRUE ~ recogniser
    )
  )


# --- Expand Survey Dates Based on Metadata ---
expanded_dates <- metadata %>%
  rowwise() %>%
  mutate(date = list(seq.Date(from = dmy(date.start), to = dmy(date.end), by = "day"))) %>%
  unnest(date) %>%
  mutate(week = week(date)) %>%
  dplyr::select(site, week) %>%
  distinct(site, week)

# Step 2: Merge expanded survey dates with detection data
completed_survey <- expanded_dates %>%
  left_join(both, by = c("site", "week")) %>%  # Join on site and week to align survey schedule with detection data
  mutate(
    # Replace NA values in 'ct' and 'recogniser' with 0 (indicating no detection)
    ct = ifelse(is.na(ct), 0, ct),
    recogniser = ifelse(is.na(recogniser), 0, recogniser)
  ) %>%
  group_by(site, week) %>%
  summarise(
    # Sum detections from both sources to get total presence indicator
    presence = sum(ct + recogniser),
    .groups = "drop"  # Ungroup after summarising
  ) %>% 
  mutate(presence_binary = case_when(presence > 0 ~ 1,
                                       TRUE ~ 0))

# Create a wide-format presence matrix (sites x weeks)
presence_weekly <- completed_survey %>% 
  dplyr::select(site, week, presence_binary) %>%  # Select relevant columns
  pivot_wider(
    names_from = week,                    # Spread weeks into columns
    values_from = presence_binary,               # Fill values with presence data
    id_cols = site,                       # Use site as row identifier
    values_fill = NA                       # Fill missing values with 0
  )

# Convert to data frame and set row names
presence_weekly <- as.data.frame(presence_weekly) %>% 
  dplyr::select(c(1,25:52,2:24))
rownames(presence_weekly) <- presence_weekly[, 1]  # Set site names as row names
presence_weekly <- presence_weekly[, -1]           # Remove site column (now row names)

# ATTENTION: Removing the first week to make total weeks = 50 (even seasonal splitting)
presence_weekly <- presence_weekly %>% 
  dplyr::select(2:51)  # Keep only weeks 2 to 51

# --- Load Site-Level Environmental Variables ---
other_vars <- load_csv("data/processed_gis_variables_3577.csv") %>%
  filter(!point %in% c("K", "L", "J")) %>%
  mutate(site = paste0("site-", point)) %>%
  dplyr::select(site, everything(), -point)

other_vars$site <- str_to_lower(other_vars$site)

# --- Load Vegetation Structure Data ---
veg <- load_csv("data/processed_vegetation.csv") %>%
  mutate(site = gsub("bar", "site", point)) %>%
  dplyr::select(site, everything(), -point)

# --- Merge Environmental and Vegetation Data ---
site_variables <- left_join(other_vars, veg, by = "site") %>%
  as.data.frame()

rownames(site_variables) <- site_variables$site
site_variables <- site_variables[,-1]


# --- Filter and Prepare Pig Removal Data ---

removals <- load_csv("data/processed_pig_removal.csv") %>%
  mutate(week = week(date)) %>% 
  filter(source == "feral-pig-take-sheet") %>%
  filter(n_detections_per_site_date_time != 0) %>%
  group_by(date, lat, long) %>%
  mutate(n_detections_per_site_date_time = sum(n_detections_per_site_date_time)) %>%
  dplyr::select(date, week, n_detections_per_site_date_time, lat, long)

# --- Prepare Site Coordinates for Spatial Analysis ---
recording_sites <- df %>%
  dplyr::select(site, latitude, longitude) %>% 
  distinct()

# --- Convert to Spatial Vectors (terra) ---
removals_vect <- vect(removals, geom = c("long", "lat"), crs = "EPSG:4326")
recording_sites_vect <- vect(recording_sites, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# --- Project to UTM for Distance Calculations (meters) ---
removals_vect <- project(removals_vect, "EPSG:32633")
recording_sites_vect <- project(recording_sites_vect, "EPSG:32633")

# --- Create 5 km Buffers Around Each Site ---
recording_sites_buffers <- buffer(recording_sites_vect, width = 5000)

# --- Create All Combinations of Sites and Dates ---
trap_dates <- expand.grid(
  site_id = unique(recording_sites$site),
  date = unique(removals$date)
)

# --- Initialize Results List ---
results <- list()

# --- Loop Through Each Site-Date Combination ---
for (i in 1:nrow(trap_dates)) {
  trap_id <- trap_dates$site_id[i]
  trap_date <- trap_dates$date[i]
  
  trap_geom <- recording_sites_vect[recording_sites_vect$site == trap_id, ]
  removals_on_date <- removals_vect[removals_vect$date == trap_date, ]
  trap_buffer <- buffer(trap_geom, width = 5000)
  
  
  # --- Check Which Removals Fall Within the Buffer ---
  overlap_matrix <- relate(removals_on_date, trap_buffer, relation = "within")
  
  if (any(overlap_matrix)) {
    removals_in_buffer <- removals_on_date[overlap_matrix, ]
    total_fly_count <- sum(removals_in_buffer$n_detections_per_site_date_time, na.rm = TRUE)
    total_removal_frequency <- nrow(removals_in_buffer)
  } else {
    total_fly_count <- 0
    total_removal_frequency <- 0
  }
  
  # --- Store Results ---
  results[[i]] <- data.frame(
    trap_id = trap_id,
    date = trap_date,
    total_fly_count = total_fly_count,
    total_removal_frequency = total_removal_frequency
  )
}

# --- Combine All Results ---
results_df <- do.call(rbind, results)

results_df$date <- ymd(results_df$date, tz = "Australia/Brisbane")

# --- Summarize Weekly Removals ---
final_results <- results_df %>%
  mutate(week = week(date)) %>%
  rename(site = trap_id) %>%
  group_by(site, week) %>%
  summarise(
    total_pig_removal = sum(total_fly_count),
    pig_removal_frequency = sum(total_removal_frequency),
    .groups = "drop"
  )

# --- Merge with Survey Effort Grid ---
completed_removals <- expanded_dates %>%
  left_join(final_results, by = c("site", "week")) %>%
  mutate(
    total_removals = ifelse(is.na(total_pig_removal), 0, total_pig_removal),
    removal_frequency = ifelse(is.na(pig_removal_frequency), 0, pig_removal_frequency)
  ) %>%
  dplyr::select(site, week, total_removals, removal_frequency)

# --- Visualize Weekly Removals ---
ggplot(completed_removals) +
  geom_col(aes(x = week, y = removal_frequency)) +
  facet_wrap(~site)

ggplot(completed_removals) +
  geom_col(aes(x = week, y = total_removals)) +
  facet_wrap(~site)

# --- Prepare Weekly Removal Matrix (for modeling) ---
completed_removals <- completed_removals[order(completed_removals$week), ]

removals_frequency_weekly <- completed_removals %>%
  dplyr::select(week, site, removal_frequency) %>%
  pivot_wider(names_from = week, values_from = removal_frequency, values_fill = 0) %>%
  dplyr::select(site, 25:52, 1:26)  # Reorder weeks for seasonal split

removals_frequency_weekly <- as.data.frame(removals_frequency_weekly)
rownames(removals_frequency_weekly) <- removals_frequency_weekly[, 1]
removals_frequency_weekly <- removals_frequency_weekly[, -1]

removals_frequency_weekly <- removals_frequency_weekly %>% 
  dplyr::select(2:51)

# --- Prepare Seasonal Removal Summary ---
removals_number_seasonal <- completed_removals %>%
  mutate(season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            #week > 45 & week <= 53 ~ "wet2",
                            TRUE ~ "wrong")) %>%
  group_by(site, season) %>%
  summarise(cummulative_seasonal_removal = sum(total_removals), .groups = "drop") %>%
  pivot_wider(names_from = season, values_from = cummulative_seasonal_removal, values_fill = 0) %>%
  dplyr::select(site, dry, late_dry, wet, late_wet, early_dry)

removals_number_seasonal <- as.data.frame(removals_number_seasonal)
rownames(removals_number_seasonal) <- removals_number_seasonal[, 1]
removals_number_seasonal <- removals_number_seasonal[, -1]

# --- Prepare Weekly Removal Count Matrix ---
removals_number_weekly <- completed_removals %>%
  group_by(site, week) %>%
  summarise(cummulative_weekly_removal = sum(total_removals), .groups = "drop") %>%
  pivot_wider(names_from = week, values_from = cummulative_weekly_removal, values_fill = 0) %>%
  dplyr::select(site, 25:52, 1:26)

removals_number_weekly <- as.data.frame(removals_number_weekly)
rownames(removals_number_weekly) <- removals_number_weekly[, 1]
removals_number_weekly <- removals_number_weekly[, -1]

removals_number_weekly <- removals_number_weekly %>% 
  dplyr::select(2:51)


# --- Load and Process climatic Data ---

climatic <- load_csv("data/processed_composite_climatic.csv")

climatic$date <- ymd(climatic$date, tz = "Australia/Brisbane")



# Assign week and season, then calculate weekly rainfall

rain <- climatic %>% 
  mutate(week = week(date),
         month = month(date),
         season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            #week > 45 & week <= 53 ~ "wet2",
                            TRUE ~ "wrong")) %>%
  group_by(week, season, site) %>% 
  summarise(weekly_rain = sum(silo_rain)) %>% 
  ungroup()

# --- Prepare Weekly Rain Matrix for Modeling ---
new_weekly_rain <- rain %>%
  # crossing(site = sites) %>%
  dplyr::select(site, week, weekly_rain) %>%
  pivot_wider(names_from = week, values_from = weekly_rain) %>%
  dplyr::select(site, 26:52, 2:24)

new_weekly_rain <- as.data.frame(new_weekly_rain)
rownames(new_weekly_rain) <- new_weekly_rain[, 1]
new_weekly_rain <- new_weekly_rain[, -1]


# Summarize seasonal rainfall and expand to all sites

seasonal_rain <- load_csv("data/processed_seasonal_rain.csv") %>% 
  dplyr::select(year, week, groups, cumulative_rain, season) %>% 
  distinct() %>% 
  group_by(season, year, groups) %>%
  summarise(cumulative_seasonal_rain = sum(cumulative_rain))

seasonal_rain_per_site <- expanded_dates %>% mutate(group = case_when(site == "site-a" ~ 10,
                                                                      site == "site-b" ~ 10,
                                                                      site == "site-b2" ~ 10,
                                                                      site == "site-c" ~ 11,
                                                                      site == "site-d" ~ 11,
                                                                      site == "site-e" ~ 1,
                                                                      site == "site-f" ~ 8,
                                                                      site == "site-g" ~ 8,
                                                                      site == "site-h" ~ 8,
                                                                      site == "site-i" ~ 7
)) %>% 
  dplyr::select(group, site) %>% 
  distinct() %>% 
  left_join(seasonal_rain, by = c("group" = "groups")) %>% 
  ungroup() %>%
  dplyr::select(season, year, site, cumulative_seasonal_rain) %>%
  pivot_wider(names_from = c(season, year), values_from = cumulative_seasonal_rain) %>%
  mutate(wet = sum(wet_2023, wet_2024)) %>% 
  dplyr::select(site, early_dry_2023, dry_2023, late_dry_2023, wet, late_wet_2024)

# Format for modeling
seasonal_rain_per_site <- as.data.frame(seasonal_rain_per_site)

rownames(seasonal_rain_per_site) <- seasonal_rain_per_site[,1]

seasonal_rain_per_site <- seasonal_rain_per_site[,-1]

# --- Load and Process Composite Temperature Data ---
temperature <- climatic %>%
  filter(date < "2024-07-30")

temperature_weekly <- temperature %>%
  mutate(week = week(date),
         season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            TRUE ~ "wrong")) %>%
  dplyr::select(week, composite_temperature_max, site, season) %>%
  group_by(week, site, season) %>%
  summarise(mean_composite_max_temperature = round(mean(composite_temperature_max), digits = 2), .groups = "drop")

# --- Summarize Seasonal Temperature ---
temperature_seasonal <- temperature_weekly %>%
  group_by(season, site) %>%
  summarise(mean_seasonal_max_temperature = round(mean(mean_composite_max_temperature), digits = 2), .groups = "drop") %>%
  pivot_wider(names_from = season, values_from = mean_seasonal_max_temperature) %>%
  dplyr::select(site, dry, late_dry, wet, late_wet, early_dry)

temperature_seasonal <- as.data.frame(temperature_seasonal)
rownames(temperature_seasonal) <- temperature_seasonal[, 1]
temperature_seasonal <- temperature_seasonal[, -1]



# --- Prepare Weekly Temperature Matrix for Modeling ---
new_weekly_temp <- temperature_weekly %>%
  dplyr::select(site, week, mean_composite_max_temperature) %>%
  pivot_wider(names_from = week, values_from = mean_composite_max_temperature) %>%
  dplyr::select(site, 26:52, 2:24)

new_weekly_temp <- as.data.frame(new_weekly_temp)
rownames(new_weekly_temp) <- new_weekly_temp[, 1]
new_weekly_temp <- new_weekly_temp[, -1]

# --- Prepare Weekly Season Labels for Modeling ---
new_weekly_season <- expanded_dates %>%
  mutate(season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            #week > 45 & week <= 53 ~ "wet2",
                            TRUE ~ "wrong")) %>%
  dplyr::select(site, week, season) %>%
  pivot_wider(names_from = week, values_from = season)

new_weekly_season <- as.data.frame(new_weekly_season)
rownames(new_weekly_season) <- new_weekly_season[, 1]
new_weekly_season <- new_weekly_season[, -1]

new_weekly_season <- new_weekly_season %>% 
  dplyr::select(2:51)

# --- Create an unmarkedMultFrame object for dynamic occupancy modeling ---

unmarkedMultFrame <- unmarkedMultFrame(
  # Detection history matrix: 1 = detection, 0 = no detection
  y = as.matrix(presence_weekly),
  
  # Number of primary periods (e.g., seasons or survey blocks)
  numPrimary = 5,  # Adjusted to group weeks into 5 primary periods
  
  # Observation-level covariates (vary by site and week)
  obsCovs = list(
    removal_freq_weekly = removals_frequency_weekly,
    removal_num_weekly = removals_number_weekly,
    rain_weekly = new_weekly_rain,
    temp_weekly = new_weekly_temp,
    season_weekly = new_weekly_season
  ),
  
  # Site-level covariates (do not vary over time)
  siteCovs = site_variables[c("shrub_cover")],
  
  # Site-level covariates that vary across primary periods
  yearlySiteCovs = list(
    temperature_seasonal = temperature_seasonal,
    removals_seasonal = removals_number_seasonal,
    rain_seasonal = seasonal_rain_per_site
  )
)

# --- Fit a null dynamic occupancy model (no covariates) ---

fm0 <- colext(
  psiformula = ~1,       # Initial occupancy is constant
  gammaformula = ~1,     # Colonization is constant
  epsilonformula = ~1,   # Extinction is constant
  pformula = ~1,         # Detection probability is constant
  data = unmarkedMultFrame,
  method = "BFGS"        # Optimization method
)



# --- Detection-only models with fixed occupancy, colonization, and extinction ---

# Model 100: Detection ~ removal frequency + rain + temp + season
dynamic_occ_full_100 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + rain_weekly + temp_weekly + season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Model 101: Detection ~ removal number + rain + temp + season
dynamic_occ_full_101 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_num_weekly + rain_weekly + temp_weekly + season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full detection model with both removal metrics + rain + temp + season
dynamic_occ_full_fixed <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + removal_num_weekly + rain_weekly + temp_weekly + season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# --- Reduced detection models ---

# f500: Detection ~ removal frequency + removal number + season
dynamic_occ_full_f500 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + removal_num_weekly + season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f501: Detection ~ removal frequency + removal number + season + rain
dynamic_occ_full_f501 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + removal_num_weekly + season_weekly + rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f502: Detection ~ removal frequency + removal number + season + temp
dynamic_occ_full_f502 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + removal_num_weekly + season_weekly + temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f503: Detection ~ removal frequency + season + temp
dynamic_occ_full_f503 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + season_weekly + temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f514: Detection ~ removal number + season + temp
dynamic_occ_full_f514 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_num_weekly + season_weekly + temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f504: Detection ~ removal frequency + removal number
dynamic_occ_full_f504 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + removal_num_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# --- Additional detection-only models with fixed occupancy, colonization, and extinction ---

# f505: Detection ~ removal frequency + season
dynamic_occ_full_f505 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f506: Detection ~ removal number + season
dynamic_occ_full_f506 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_num_weekly + season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f508: Detection ~ removal number + rain + temp
dynamic_occ_f508 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_num_weekly + rain_weekly + temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f509: Detection ~ removal number + rain
dynamic_occ_f509 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_num_weekly + rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f510: Detection ~ removal number + temp
dynamic_occ_f510 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_num_weekly + temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f507: Detection ~ removal frequency + rain + temp
dynamic_occ_f507 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + rain_weekly + temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f511: Detection ~ removal frequency + temp
dynamic_occ_f511 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f512: Detection ~ removal frequency + rain
dynamic_occ_f512 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly + rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)


# --- Additional single-variable and two-variable detection models with fixed intercepts ---

# f1: Detection ~ removal number only
dynamic_occ_f1 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_num_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f513: Detection ~ removal frequency only
dynamic_occ_f513 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ removal_freq_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f2: Detection ~ rain only
dynamic_occ_f2 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f3: Detection ~ temperature only
dynamic_occ_f3 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f4: Detection ~ rain + temperature
dynamic_occ_f4 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ rain_weekly + temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# --- Detection models with interaction terms and seasonal structure ---

# f8: Detection ~ interaction between rain and temperature
dynamic_occ_f8 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ rain_weekly:temp_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f9: Detection ~ season (no intercept)
dynamic_occ_f9 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ season_weekly - 1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f10: Detection ~ season + removal number
dynamic_occ_f10 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ season_weekly + removal_num_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# f103: Detection ~ season + removal frequency (no intercept)
dynamic_occ_f103 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ season_weekly + removal_freq_weekly - 1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# --- Dynamic models with seasonal covariates on colonization/extinction ---

# Model 24: Colonization and extinction ~ seasonal rainfall
dynamic_occ_24 <- colext(
  psiformula = ~1,
  gammaformula = ~ rain_seasonal,
  epsilonformula = ~ rain_seasonal,
  pformula = ~1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Model 25: Extinction ~ seasonal rainfall only
dynamic_occ_25 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~ rain_seasonal,
  pformula = ~1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# --- Dynamic occupancy models with seasonal covariates on colonization/extinction and rain-based detection ---

# Model 28: Colonization ~ temperature_seasonal
dynamic_occ_28 <- colext(
  psiformula = ~1,
  gammaformula = ~ temperature_seasonal,
  epsilonformula = ~1,
  pformula = ~1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Model 27: Extinction ~ temperature_seasonal
dynamic_occ_27 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~ temperature_seasonal,
  pformula = ~1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Model r10: Colonization & extinction ~ removals + temperature; Detection ~ rain
dynamic_occ_r10 <- colext(
  psiformula = ~1,
  gammaformula = ~ removals_seasonal + temperature_seasonal,
  epsilonformula = ~ removals_seasonal + temperature_seasonal,
  pformula = ~ rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Model r11: Colonization ~ removals + temperature; Extinction ~ temperature; Detection ~ rain
dynamic_occ_r11 <- colext(
  psiformula = ~1,
  gammaformula = ~ removals_seasonal + temperature_seasonal,
  epsilonformula = ~ temperature_seasonal,
  pformula = ~ rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Model r12: Colonization & extinction ~ temperature; Detection ~ rain
dynamic_occ_r12 <- colext(
  psiformula = ~1,
  gammaformula = ~ temperature_seasonal,
  epsilonformula = ~ temperature_seasonal,
  pformula = ~ rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Model r13: Colonization & extinction ~ constant; Detection ~ rain
dynamic_occ_r13 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Model r14: Extinction ~ temperature; Colonization ~ constant; Detection ~ constant
dynamic_occ_r14 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~ temperature_seasonal,
  pformula = ~1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# --- Detection models with interaction terms ---

# r19: Detection ~ interaction (no main effects) between temp and rain
dynamic_occ_r19 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ temp_weekly:rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# r20: Detection ~ interaction + main effects (temp * rain)
dynamic_occ_r20 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ temp_weekly * rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)


# --- Full models with seasonal detection and various colonization/extinction covariates ---

# s8: Colonization & extinction ~ temperature + removals; Detection ~ season
dynamic_occ_s8 <- colext(
  psiformula = ~1,
  gammaformula = ~ temperature_seasonal + removals_seasonal,
  epsilonformula = ~ removals_seasonal + temperature_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# s9: Colonization & extinction ~ temperature; Detection ~ season
dynamic_occ_s9 <- colext(
  psiformula = ~1,
  gammaformula = ~ temperature_seasonal,
  epsilonformula = ~ temperature_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# s10: Colonization & extinction ~ removals; Detection ~ season
dynamic_occ_s10 <- colext(
  psiformula = ~1,
  gammaformula = ~ removals_seasonal,
  epsilonformula = ~ removals_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# s11: Colonization & extinction ~ rainfall; Detection ~ season
dynamic_occ_s11 <- colext(
  psiformula = ~1,
  gammaformula = ~ rain_seasonal,
  epsilonformula = ~ rain_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# s12: Colonization & extinction ~ constant; Detection ~ season
dynamic_occ_s12 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# s120: Detection ~ interaction between season and rain
dynamic_occ_s120 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~ season_weekly:rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# s13: Colonization & extinction ~ temperature + rainfall; Detection ~ season
dynamic_occ_s13 <- colext(
  psiformula = ~1,
  gammaformula = ~ temperature_seasonal + rain_seasonal,
  epsilonformula = ~ temperature_seasonal + rain_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)


# ---- Dynamic Occupancy Models: Shrub Only ----

# Full model: detection ~ removal_num_weekly, colonization/extinction ~ rain + removals + temperature
dynamic_occ_s1 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + removals_seasonal + temperature_seasonal, 
  epsilonformula = ~ rain_seasonal + removals_seasonal + temperature_seasonal,
  pformula = ~ removal_num_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ removal_freq_weekly, colonization/extinction ~ rain + removals + temperature
dynamic_occ_s2 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + removals_seasonal + temperature_seasonal, 
  epsilonformula = ~ rain_seasonal + removals_seasonal + temperature_seasonal,
  pformula = ~ removal_freq_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ removal_num_weekly, no removals in colonization/extinction
dynamic_occ_s3 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + temperature_seasonal, 
  epsilonformula = ~ rain_seasonal + temperature_seasonal,
  pformula = ~ removal_num_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ removal_freq_weekly, no removals in colonization/extinction
dynamic_occ_s4 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + temperature_seasonal, 
  epsilonformula = ~ rain_seasonal + temperature_seasonal,
  pformula = ~ removal_freq_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ season_weekly, no removals in colonization/extinction
dynamic_occ_s5 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + temperature_seasonal, 
  epsilonformula = ~ rain_seasonal + temperature_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ removal_num_weekly, extinction includes removals
dynamic_occ_s6 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + temperature_seasonal, 
  epsilonformula = ~ removals_seasonal + temperature_seasonal,
  pformula = ~ removal_num_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ removal_freq_weekly, extinction includes removals
dynamic_occ_s7 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + temperature_seasonal, 
  epsilonformula = ~ removals_seasonal + temperature_seasonal,
  pformula = ~ removal_freq_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ season_weekly, extinction includes removals
dynamic_occ_s8 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + temperature_seasonal, 
  epsilonformula = ~ removals_seasonal + temperature_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ removal_num_weekly, reordered colonization terms
dynamic_occ_s9 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal + removals_seasonal, 
  epsilonformula = ~ removals_seasonal + temperature_seasonal,
  pformula = ~ removal_num_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ removal_freq_weekly, reordered colonization terms
dynamic_occ_s10 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal + removals_seasonal, 
  epsilonformula = ~ removals_seasonal + temperature_seasonal,
  pformula = ~ removal_freq_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ season_weekly, reordered colonization terms
dynamic_occ_s11 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal + removals_seasonal, 
  epsilonformula = ~ removals_seasonal + temperature_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)


# ---- Dynamic Occupancy Models: Shrub Only ----

# Full model: detection ~ season_weekly, all three covariates in colonization/extinction
dynamic_occ_s12 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal + removals_seasonal + rain_seasonal, 
  epsilonformula = ~ removals_seasonal + temperature_seasonal + rain_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ season_weekly, extinction excludes removals
dynamic_occ_s13 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal + removals_seasonal + rain_seasonal, 
  epsilonformula = ~ temperature_seasonal + rain_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ 1, extinction includes removals
dynamic_occ_s14 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal + rain_seasonal, 
  epsilonformula = ~ temperature_seasonal + rain_seasonal + removals_seasonal,
  pformula = ~ 1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ 1, extinction excludes removals
dynamic_occ_s15 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal + rain_seasonal, 
  epsilonformula = ~ temperature_seasonal + rain_seasonal,
  pformula = ~ 1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ 1, colonization/extinction with interaction (:) only
dynamic_occ_s16 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal:rain_seasonal, 
  epsilonformula = ~ temperature_seasonal:rain_seasonal,
  pformula = ~ 1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ 1, colonization/extinction with interaction (*)
dynamic_occ_s17 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal * rain_seasonal, 
  epsilonformula = ~ temperature_seasonal * rain_seasonal,
  pformula = ~ 1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ 1, colonization/extinction ~ rain only
dynamic_occ_s18 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal, 
  epsilonformula = ~ rain_seasonal,
  pformula = ~ 1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ 1, colonization/extinction ~ temperature only
dynamic_occ_s19 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ temperature_seasonal, 
  epsilonformula = ~ temperature_seasonal,
  pformula = ~ 1,
  data = unmarkedMultFrame,
  method = "BFGS"
)


# ---- Dynamic Occupancy Models: Shrub Only ----

# Full model: detection ~ rain_weekly, colonization/extinction ~ removals + temperature
dynamic_occ_s20 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ removals_seasonal + temperature_seasonal, 
  epsilonformula = ~ removals_seasonal + temperature_seasonal,
  pformula = ~ rain_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# Full model: detection ~ season_weekly, colonization/extinction ~ rain + removals + temperature
dynamic_occ_s21 <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + removals_seasonal + temperature_seasonal, 
  epsilonformula = ~ rain_seasonal + removals_seasonal + temperature_seasonal,
  pformula = ~ season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)

dynamic_occ_s22_full <- colext(
  psiformula = ~ shrub_cover,
  gammaformula = ~ rain_seasonal + removals_seasonal + temperature_seasonal, 
  epsilonformula = ~ rain_seasonal + removals_seasonal + temperature_seasonal,
  pformula = ~ removal_freq_weekly + removal_num_weekly + rain_weekly + temp_weekly + season_weekly,
  data = unmarkedMultFrame,
  method = "BFGS"
)




# ---- Function to calculate c-hat and QAIC for a list of colext models ----

# calculate_qaic <- function(models) {
#   results <- data.frame(Model = character(), c_hat = numeric(), QAIC = numeric(), AIC = numeric(), stringsAsFactors = FALSE)
# 
#   for (i in seq_along(models)) {
#     model <- models[[i]]
#     model_name <- names(models)[i]
# 
#     mb.boot <- AICcmodavg::mb.gof.test(model, nsim = 1000, plot.hist = FALSE)
# 
#     c_hat <- mb.boot$c.hat.est
#     QAIC <- AIC(model) / c_hat
# 
#     results <- rbind(results, data.frame(Model = model_name, c_hat = c_hat, QAIC = QAIC, AIC = AIC(model), stringsAsFactors = FALSE))
#   }
# 
#   results <- results[order(results$QAIC), ]
#   return(results)
# }

# ---- Model groups ----

models1 <- list(
  full = dynamic_occ_full_fixed,
  null = fm0,
  m2 = dynamic_occ_24,
  m3 = dynamic_occ_25,
  m4 = dynamic_occ_27,
  m5 = dynamic_occ_28,
  s1 = dynamic_occ_s1,
  s2 = dynamic_occ_s2,
  s3 = dynamic_occ_s3,
  s4 = dynamic_occ_s4,
  s5 = dynamic_occ_s5,
  s6 = dynamic_occ_s6,
  s7 = dynamic_occ_s7,
  s8 = dynamic_occ_s8,
  s9 = dynamic_occ_s9,
  s10 = dynamic_occ_s10,
  s11 = dynamic_occ_s11,
  s12 = dynamic_occ_s12,
  s13 = dynamic_occ_s13,
  s14 = dynamic_occ_s14,
  s15 = dynamic_occ_s15,
  s16 = dynamic_occ_s16,
  s17 = dynamic_occ_s17,
  s18 = dynamic_occ_s18,
  s19 = dynamic_occ_s19,
  s20 = dynamic_occ_s20,
  s21 = dynamic_occ_s21,
  s22 = dynamic_occ_s22_full,
  m6 = dynamic_occ_f1,
  m7 = dynamic_occ_f10,
  m8 = dynamic_occ_f103,
  m9 = dynamic_occ_f2,
  m10a = dynamic_occ_f3,
  m10b = dynamic_occ_f4,
  m11 = dynamic_occ_f507,
  m12 = dynamic_occ_f508,
  m13 = dynamic_occ_f509,
  m14 = dynamic_occ_f510,
  m15 = dynamic_occ_f511,
  m16 = dynamic_occ_f512,
  m17 = dynamic_occ_f513,
  m18 = dynamic_occ_f8,
  m19 = dynamic_occ_f9,
  m20 = dynamic_occ_full_100,
  m21 = dynamic_occ_full_101,
  m22 = dynamic_occ_full_f500,
  m23 = dynamic_occ_full_f501,
  m24 = dynamic_occ_full_f502,
  m25 = dynamic_occ_full_f503,
  m26 = dynamic_occ_full_f504,
  m27 = dynamic_occ_full_f505,
  m28 = dynamic_occ_full_f506,
  m29 = dynamic_occ_full_f514,
  m30 = dynamic_occ_r10,
  m31 = dynamic_occ_r11,
  m32 = dynamic_occ_r12,
  m33 = dynamic_occ_r13,
  m34 = dynamic_occ_r14,
  m35 = dynamic_occ_r19,
  m36 = dynamic_occ_r20

)

# ---- Calculate QAIC for model sets ----

AICcmodavg::mb.gof.test(dynamic_occ_s22_full, nsim = 1000, plot.seasons = T, print.table = T, plot.hist = T)

AICcmodavg::aictab(models1, c.hat = 1) # to account for 0.91 models are: m29, m25, m22

unmarked::vif(dynamic_occ_full_f514, type = 'det')
unmarked::vif(dynamic_occ_full_f503, type = 'det')
unmarked::vif(dynamic_occ_full_f500, type = 'det')


list <- fitList(m29 = dynamic_occ_full_f514,
                m25 = dynamic_occ_full_f503,
                m22 = dynamic_occ_full_f500)
                

# --- Simulate Covariate Values for Prediction ---
set.seed(123)




removal_mean <- completed_removals %>%
  mutate(season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            #week > 45 & week <= 53 ~ "wet2",
                            TRUE ~ "wrong")) %>%
  group_by(season) %>%
  summarise(q1 = quantile(total_removals, 0.05),
            q2 = quantile(total_removals, 1))

# sim_removal <- data.frame(
#   removal_num_weekly = runif(100, min = min(completed_removals$total_removals), max = max(completed_removals$total_removals)),
#   rain_weekly = 0,
#   temp_weekly = 0
# )

sim_removal <- data.frame(
  dry = as.integer(runif(100, min = 0, max = 6)),
  early_dry = as.integer(runif(100, min = 0, max = 6)),
  late_dry = as.integer(runif(100, min = 0, max = 6)),
  late_wet = as.integer(runif(100, min = 0, max = 6)),
  wet = as.integer(runif(100, min = 0, max = 6)),
  index = 1:100
  # temp_weekly = 1
) %>%
  pivot_longer(cols = 1:5, names_to = "season_weekly", values_to = "removal_num_weekly")

# sim_removal <- data.frame(
#   dry = runif(100, min = removal_mean$q1[1], max = removal_mean$q2[1]),
#   early_dry = runif(100, min = removal_mean$q1[2], max = removal_mean$q2[2]),
#   late_dry = runif(100, min = removal_mean$q1[3], max = removal_mean$q2[3]),
#   late_wet = runif(100, min = removal_mean$q1[4], max = removal_mean$q2[4]),
#   wet = runif(100, min = removal_mean$q1[5], max = removal_mean$q2[5]),
#   index = 1:100
#   # temp_weekly = 1
# ) %>%
#   pivot_longer(cols = 1:5, names_to = "season_weekly", values_to = "removal_num_weekly")


removal_freq_mean <- completed_removals %>%
  mutate(season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            #week > 45 & week <= 53 ~ "wet2",
                            TRUE ~ "wrong")) %>%
  group_by(season) %>%
  summarise(q1 = quantile(removal_frequency, 0.05),
            q2 = quantile(removal_frequency, 1))

# sim_removal <- data.frame(
#   removal_num_weekly = runif(100, min = min(completed_removals$total_removals), max = max(completed_removals$total_removals)),
#   rain_weekly = 0,
#   temp_weekly = 0
# )

sim_removal_freq <- data.frame(
  dry = as.integer(runif(100, min = 0, max = 1.5)),
  early_dry = as.integer(runif(100, min = 0, max = 1.5)),
  late_dry = as.integer(runif(100, min = 0, max = 1.5)),
  late_wet = as.integer(runif(100, min = 0, max = 1.5)),
  wet = as.integer(runif(100, min = 0, max = 1.5)),
  index = 1:100 
  # temp_weekly = 1
) %>%
  pivot_longer(cols = 1:5, names_to = "season_weekly", values_to = "removal_freq_weekly")

# sim_removal_freq <- data.frame(
#   dry = runif(100, min = removal_freq_mean$q1[1], max = removal_freq_mean$q2[1]),
#   early_dry = runif(100, min = removal_freq_mean$q1[2], max = removal_freq_mean$q2[2]),
#   late_dry = runif(100, min = removal_freq_mean$q1[3], max = removal_freq_mean$q2[3]),
#   late_wet = runif(100, min = removal_freq_mean$q1[4], max = removal_freq_mean$q2[4]),
#   wet = runif(100, min = removal_freq_mean$q1[5], max = removal_freq_mean$q2[5]),
#   index = 1:100 
#   # temp_weekly = 1
# ) %>%
#   pivot_longer(cols = 1:5, names_to = "season_weekly", values_to = "removal_freq_weekly")


temp_mean <- temperature %>%
  mutate(week = week(date), #) %>% 
         # month = month(date),
         season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            #week > 45 & week <= 53 ~ "wet2",
                            TRUE ~ "wrong")) %>%
  dplyr::select(week, composite_temperature_max, site, season) %>% 
  group_by(week, site, season) %>% 
  summarise(mean_composite_max_temperature = round(mean(composite_temperature_max), digits = 2), .groups = "drop") %>% 
  ungroup() %>% 
  group_by(season) %>%
  summarise(q1 = quantile(mean_composite_max_temperature, 0.25),
            q2 = quantile(mean_composite_max_temperature, 0.75))

sim_temp <- data.frame(
  dry = runif(100, min = temp_mean$q1[1], max = temp_mean$q2[1]),
  early_dry = runif(100, min = temp_mean$q1[2], max = temp_mean$q2[2]),
  late_dry = runif(100, min = temp_mean$q1[3], max = temp_mean$q2[3]),
  late_wet = runif(100, min = temp_mean$q1[4], max = temp_mean$q2[4]),
  wet = runif(100, min = temp_mean$q1[5], max = temp_mean$q2[5]),
  index = 1:100
  # removal_num_weekly = 1
) %>%
  pivot_longer(cols = 1:5, names_to = "season_weekly", values_to = "temp_weekly")

#combine all simulations

sim_all <- full_join(sim_removal, sim_removal_freq) %>% 
  full_join(sim_temp)


