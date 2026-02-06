#probability of detection for acoustics using occupancy modelling

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

# --- Convert Date Column to Date Format ---
df$date <- ymd(df$date, tz = "Australia/Brisbane")

# --- Prepare Recogniser Detection Data (Summarised Weekly) ---
df_prep_recog <- df %>%
  filter(source == "recogniser") %>%
  group_by(site, week) %>%
  summarise(count = sum(n_detections_per_site_date_time), .groups = "drop") %>%
  rename(recogniser = count) %>% 
  mutate(recogniser_binary = case_when(recogniser > 0 ~ 1,
                                       TRUE ~ 0))


# --- Load Metadata for Monitoring Sites ---
metadata <- load_csv("data/processed_monitoring_points.csv")

sites <- unique(metadata$site)

# --- Expand Survey Dates Based on Metadata ---
expanded_dates <- metadata %>%
  rowwise() %>%
  mutate(date = list(seq.Date(from = dmy(date.start), to = dmy(date.end), by = "day"))) %>%
  unnest(date) %>%
  mutate(week = week(date)) %>%
  dplyr::select(site, week) %>%
  distinct(site, week)


# --- Merge Expanded Dates with Detection Data ---
completed_survey <- expanded_dates %>%
  left_join(df_prep_recog, by = c("site", "week")) %>%
  mutate(recogniser_binary = ifelse(is.na(recogniser_binary), 0, recogniser_binary))  # Fill NAs with 0 (absence)

# --- Create Weekly Presence/Absence Matrix ---
presence_weekly <- completed_survey %>%
  dplyr::select(site, week, recogniser_binary) %>%
  pivot_wider(names_from = week, values_from = recogniser_binary, id_cols = site, values_fill = NA) %>%
  as.data.frame()

rownames(presence_weekly) <- presence_weekly$site
presence_weekly <- presence_weekly[,-1]

# --- Remove First Week to Standardize to 50 Weeks ---
# This ensures even seasonal splits (10 weeks per season)
presence_weekly <- presence_weekly %>%
  dplyr::select(2:51)


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

# --- Load Yourka Boundary for Plotting ---

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
    total_pig_count <- sum(removals_in_buffer$n_detections_per_site_date_time, na.rm = TRUE)
    total_removal_frequency <- nrow(removals_in_buffer)
  } else {
    total_pig_count <- 0
    total_removal_frequency <- 0
  }
  
  # --- Store Results ---
  results[[i]] <- data.frame(
    trap_id = trap_id,
    date = trap_date,
    total_pig_count = total_pig_count,
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
    total_pig_removal = sum(total_pig_count),
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
                            TRUE ~ "wrong")) %>%
  group_by(week, season, site) %>% 
  summarise(weekly_rain = sum(silo_rain)) %>% 
  ungroup()

# --- Prepare Weekly Rain Matrix for Modeling ---
weekly_rain <- rain %>%
  dplyr::select(site, week, weekly_rain) %>%
  pivot_wider(names_from = week, values_from = weekly_rain) %>%
  dplyr::select(site, 26:52, 2:24)

weekly_rain <- as.data.frame(weekly_rain)
rownames(weekly_rain) <- weekly_rain[, 1]
weekly_rain <- weekly_rain[, -1]

# --- Load and Process Composite Temperature Data ---
temperature <- climatic %>%
  filter(date < "2024-07-30")

weekly_temp <- temperature %>%
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
  summarise(mean_composite_max_temperature = round(mean(composite_temperature_max), digits = 2), .groups = "drop")

# --- Prepare Weekly Temperature Matrix for Modeling ---
weekly_temp <- weekly_temp %>%
  dplyr::select(site, week, mean_composite_max_temperature) %>%
  pivot_wider(names_from = week, values_from = mean_composite_max_temperature) %>%
  dplyr::select(site, 26:52, 2:24)

weekly_temp <- as.data.frame(weekly_temp)
rownames(weekly_temp) <- weekly_temp[, 1]
weekly_temp <- weekly_temp[, -1]


# --- Prepare Weekly Season Labels for Modeling ---
weekly_season <- expanded_dates %>%
  mutate(season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            TRUE ~ "wrong")) %>%
  dplyr::select(site, week, season) %>%
  pivot_wider(names_from = week, values_from = season) 

weekly_season <- as.data.frame(weekly_season)
rownames(weekly_season) <- weekly_season[, 1]
weekly_season <- weekly_season[, -1]

weekly_season <- weekly_season %>% 
  dplyr::select(2:51)

# --- Create Multi-Season Occupancy Model Frame ---
unmarkedMultFrame <- unmarkedMultFrame(
  y = as.matrix(presence_weekly),
  numPrimary = 5,  # Number of primary periods (e.g., seasons)
  obsCovs = list(
    removal_freq_weekly = removals_frequency_weekly,
    removal_num_weekly = removals_number_weekly,
    rain_weekly = weekly_rain,
    temp_weekly = weekly_temp,
    season_weekly = weekly_season
  )
)


# --- Null Model (Intercept-Only) ---
fm0 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~1,
  data = unmarkedMultFrame,
  method = "BFGS"
)

# --- Single Variable Detection Models (Fixed Occupancy, Colonization, and Extinction) ---

# Detection ~ removal frequency
fixed1 <- colext(~1, ~1, ~1, ~removal_freq_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal number
fixed2 <- colext(~1, ~1, ~1, ~removal_num_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ weekly rain
fixed3 <- colext(~1, ~1, ~1, ~rain_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ weekly temperature
fixed4 <- colext(~1, ~1, ~1, ~temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ season
fixed5 <- colext(~1, ~1, ~1, ~season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + removal number
fixed6 <- colext(~1, ~1, ~1, ~removal_freq_weekly + removal_num_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + rain
fixed7 <- colext(~1, ~1, ~1, ~removal_freq_weekly + rain_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + temperature
fixed8 <- colext(~1, ~1, ~1, ~removal_freq_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + season
fixed9 <- colext(~1, ~1, ~1, ~removal_freq_weekly + season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal number + temperature
fixed10 <- colext(~1, ~1, ~1, ~removal_num_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal number + season
fixed11 <- colext(~1, ~1, ~1, ~removal_num_weekly + season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ rain + temperature
fixed12 <- colext(~1, ~1, ~1, ~rain_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ rain + season
fixed13 <- colext(~1, ~1, ~1, ~rain_weekly + season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + rain + temperature
fixed14 <- colext(~1, ~1, ~1, ~removal_freq_weekly + rain_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ temperature + season
fixed15 <- colext(~1, ~1, ~1, ~temp_weekly + season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + rain + removal number
fixed16 <- colext(~1, ~1, ~1, ~removal_freq_weekly + rain_weekly + removal_num_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal number + removal frequency + temperature
fixed17 <- colext(~1, ~1, ~1, ~removal_num_weekly + removal_freq_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ season + removal frequency + removal number
fixed18 <- colext(~1, ~1, ~1, ~season_weekly + removal_freq_weekly + removal_num_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + rain + temperature
fixed19 <- colext(~1, ~1, ~1, ~removal_freq_weekly + rain_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + rain + season
fixed20 <- colext(~1, ~1, ~1, ~removal_freq_weekly + rain_weekly + season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ season + temperature + removal frequency
fixed21 <- colext(~1, ~1, ~1, ~season_weekly + temp_weekly + removal_freq_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal number + rain + temperature
fixed22 <- colext(~1, ~1, ~1, ~removal_num_weekly + rain_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ season + removal number + rain
fixed23 <- colext(~1, ~1, ~1, ~season_weekly + removal_num_weekly + rain_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ season + removal number + temperature
fixed24 <- colext(~1, ~1, ~1, ~season_weekly + removal_num_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ season + rain + temperature
fixed25 <- colext(~1, ~1, ~1, ~season_weekly + rain_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + removal number + rain + temperature
fixed26 <- colext(~1, ~1, ~1, ~removal_freq_weekly + removal_num_weekly + rain_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + removal number + rain + season
fixed27 <- colext(~1, ~1, ~1, ~removal_freq_weekly + removal_num_weekly + rain_weekly + season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + removal number + temperature + season
fixed28 <- colext(~1, ~1, ~1, ~removal_freq_weekly + removal_num_weekly + temp_weekly + season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal frequency + rain + temperature + season
fixed29 <- colext(~1, ~1, ~1, ~removal_freq_weekly + rain_weekly + temp_weekly + season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ removal number + rain + temperature + season
fixed30 <- colext(~1, ~1, ~1, ~rain_weekly + removal_num_weekly + temp_weekly + season_weekly, data = unmarkedMultFrame, method = "BFGS")

# Detection ~ rain + temperature + removal frequency + removal number
fixed31 <- colext(~1, ~1, ~1, ~rain_weekly + removal_freq_weekly + removal_num_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")

# Full model: Detection ~ rain + temperature + season + removal frequency + removal number
fixed_full <- colext(~1, ~1, ~1, ~rain_weekly + season_weekly + removal_num_weekly + temp_weekly, data = unmarkedMultFrame, method = "BFGS")


# --- List of Models to Compare ---
models1 <- list(
  m1 = fm0, f1 = fixed1, 
  f2 = fixed2, 
  f3 = fixed3, 
  f4 = fixed4,
  f5 = fixed5, f6 = fixed6, f7 = fixed7, f8 = fixed8, f9 = fixed9,
  f10 = fixed10,
  f11 = fixed11, f12 = fixed12, f13 = fixed13, f14 = fixed14, 
  f15 = fixed15, f16 = fixed16, f17 = fixed17,
  f18 = fixed18, f19 = fixed19, f20 = fixed20, f21 = fixed21,
  f22 = fixed22, f23 = fixed23,
  f24 = fixed24, f25 = fixed25,
  f26 = fixed26, f27 = fixed27, f28 = fixed28, f29 = fixed29,
  f30 = fixed30, f31 = fixed31,
  ff = fixed_full
)


AICcmodavg::mb.gof.test(fixed_full, nsim = 1000, plot.seasons = T, plot.hist = T)

AICcmodavg::aictab(models1, c.hat = 2, sort = T) #QAIC values suggest model averaging on: f15, f13, f11 and f9

unmarked::vif(fixed15, type = 'det')
unmarked::vif(fixed13, type = 'det')
unmarked::vif(fixed11, type = 'det')
unmarked::vif(fixed9, type = 'det')

list <- fitList(f11 = fixed11, f13 = fixed13, f15 = fixed15, f9 = fixed9)


# --- Simulate Covariate Values for Prediction ---
set.seed(123)


temp_mean <- temperature %>%
  mutate(week = week(date), #) %>% 
         # month = month(date),
         season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
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
) %>%
  pivot_longer(cols = 1:5, names_to = "season_weekly", values_to = "temp_weekly")


rain_mean <- rain %>%
  dplyr::select(week, weekly_rain, site, season) %>% 
  group_by(week, site, season) %>% 
  summarise(weekly_rain = round(mean(weekly_rain), digits = 2), .groups = "drop") %>% 
  ungroup() %>% 
  group_by(season) %>%
  summarise(q1 = quantile(weekly_rain, 0.25),
            q2 = quantile(weekly_rain, 0.75))

sim_rain <- data.frame(
  dry = runif(100, min = rain_mean$q1[1], max = rain_mean$q2[1]),
  early_dry = runif(100, min = rain_mean$q1[2], max = rain_mean$q2[2]),
  late_dry = runif(100, min = rain_mean$q1[3], max = rain_mean$q2[3]),
  late_wet = runif(100, min = rain_mean$q1[4], max = rain_mean$q2[4]),
  wet = runif(100, min = rain_mean$q1[5], max = rain_mean$q2[5]),
  index = 1:100
) %>%
  pivot_longer(cols = 1:5, names_to = "season_weekly", values_to = "rain_weekly")


removal_mean <- completed_removals %>%
  mutate(season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            TRUE ~ "wrong")) %>%
  group_by(season) %>%
  summarise(q1 = quantile(total_removals, 0.05),
            q2 = quantile(total_removals, 1))



sim_removal <- data.frame(
  dry = as.integer(runif(100, min = 0, max = 6)),
  early_dry = as.integer(runif(100, min = 0, max = 6)),
  late_dry = as.integer(runif(100, min = 0, max = 6)),
  late_wet = as.integer(runif(100, min = 0, max = 6)),
  wet = as.integer(runif(100, min = 0, max = 6)),
  index = 1:100
) %>%
  pivot_longer(cols = 1:5, names_to = "season_weekly", values_to = "removal_num_weekly")


removal_freq_mean <- completed_removals %>%
  mutate(season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            TRUE ~ "wrong")) %>%
  group_by(season) %>%
  summarise(q1 = quantile(removal_frequency, 0.05),
            q2 = quantile(removal_frequency, 1))


sim_removal_freq <- data.frame(
  dry = as.integer(runif(100, min = 0, max = 1.5)),
  early_dry = as.integer(runif(100, min = 0, max = 1.5)),
  late_dry = as.integer(runif(100, min = 0, max = 1.5)),
  late_wet = as.integer(runif(100, min = 0, max = 1.5)),
  wet = as.integer(runif(100, min = 0, max = 1.5)),
  index = 1:100 
) %>%
  pivot_longer(cols = 1:5, names_to = "season_weekly", values_to = "removal_freq_weekly")

#combine all simulations

sim_all <- full_join(sim_rain, sim_removal) %>% 
  full_join(sim_removal_freq) %>% 
  full_join(sim_temp)


sim_all$det <- predict(list, type = 'det', newdata = sim_all)

AICcmodavg::modavgPred(list1, parm.type = "psi", newdata = sim_all)

summary(sim_all)

predict(list, type = 'psi', newdata = sim_all)
predict(list, type = 'col', newdata = sim_all)
predict(list, type = 'ext', newdata = sim_all)

save_csv(sim_all, "results/sound_predicted_results.csv")

sim_all %>% 
  ggplot() +
  geom_pointrange(aes(x = season_weekly, y = det$Predicted, ymin = det$lower, ymax = det$upper))
