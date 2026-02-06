# --- Load Required Libraries ---
library(tidyverse)
library(lubridate)
library(ggplot2)
library(report)
library(hms)

source("R/load_data.R")
source("R/save_utils.R")

# --- 1. Load and Prepare Data ---

# Load Yourka datalogger data

yourka_data <- load_csv("data/raw_weather_measured.csv")


# Load and clean SILO climate data
silo <- load_csv("data/processed_silo_all_data.csv") %>%
  # dplyr::select(-c(daily_rain_source, max_temp_source, min_temp_source, rh_tmax_source, rh_tmin_source)) %>%
  mutate(
    silo_average_temperature = (T.Max + T.Min) / 2,
  ) %>%
  rename(silo_max_temperature = T.Max,
         silo_min_temperature = T.Min,
         silo_rain = Rain) %>% 
  group_by(lat, long) %>%
  mutate(groups = cur_group_id())
  # rename(date = YYYY.MM.DD) %>%
  # dplyr::select(#silo_average_humidty, 
    # silo_average_temperature, daily_rain, date,
    #      silo_max_temperature = max_temp,
    #      silo_min_temperature = min_temp)

# Convert SILO date to Date format
silo$Date <- ymd(silo$Date)

yourka_data$date <- ymd(yourka_data$date)
yourka_data$DateTime <-  as.POSIXct(yourka_data$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")

# --- 2. Create Complete Date-Site Grid ---

start_date <- dmy("26/06/2023")
end_date <- dmy("08/06/2024")
sites <- unique(yourka_data$site)

# Create a full grid of dates and sites
complete_times <- expand.grid(date = seq(from = start_date, to = end_date, by = "1 day"),
                              site = sites) %>% 
  mutate(group = case_when(site == "site-a" ~ 10,
                           site == "site-b" ~ 10,
                           site == "site-b2" ~ 10,
                           site == "site-c" ~ 11,
                           site == "site-d" ~ 11,
                           site == "site-e" ~ 1,
                           site == "site-f" ~ 8,
                           site == "site-g" ~ 8,
                           site == "site-h" ~ 8,
                           site == "site-i" ~ 7
  ))

# --- 3. Visualize Raw Data ---

yourka_data %>%
  ggplot(aes(x = DateTime)) +
  geom_line(aes(y = temperature), colour = "orange") +
  # geom_line(aes(y = humidity, colour = "cyan")) +
  facet_wrap(~site)

# --- 4. Summarize Daily Temperature and Humidity ---

available_data <- yourka_data %>%
  group_by(site, date) %>%
  summarise(
    measured_temperature_average = mean(temperature),
    measured_temperature_max = max(temperature),
    measured_temperature_min = min(temperature),
    .groups = "drop"
  ) %>% 
  mutate(group = case_when(site == "site-a" ~ 10,
                           site == "site-b" ~ 10,
                           site == "site-b2" ~ 10,
                           site == "site-c" ~ 11,
                           site == "site-d" ~ 11,
                           site == "site-e" ~ 1,
                           site == "site-f" ~ 8,
                           site == "site-g" ~ 8,
                           site == "site-h" ~ 8,
                           site == "site-i" ~ 7
                           ))

# --- 5. Merge with SILO Data ---

# Merge complete date-site grid with Yourka and SILO data
full_data <- complete_times %>%
  left_join(available_data, by = c("date", "site", "group")) %>%
  left_join(silo, by = c("date" = "Date", "group" = "groups")) %>%
  mutate(month = month(date))

# Prepare aligned data for modeling
aligned_data <- available_data %>%
  left_join(silo, by = c("date" = "Date", "group" = "groups")) %>%
  mutate(month = month(date))

# --- 6. Fit Linear Models for Temperature and Humidity ---

# Temperature models
m1_site_avg_temp <- lm(measured_temperature_average ~ silo_average_temperature + site + month, data = aligned_data)
m1_site_max_temp <- lm(measured_temperature_max ~ silo_max_temperature + site + month, data = aligned_data)
m1_site_min_temp <- lm(measured_temperature_min ~ silo_min_temperature + site + month, data = aligned_data)

report(m1_site_max_temp)

# --- 7. Predict Missing Values ---

# Add predictions to full dataset
full_data <- full_data %>%
  mutate(
    predict_temp_avg = predict(m1_site_avg_temp, newdata = full_data),
    predict_temp_max = predict(m1_site_max_temp, newdata = full_data),
    predict_temp_min = predict(m1_site_min_temp, newdata = full_data),
  )

# --- 8. Evaluate Model Performance ---


cor.test(full_data$measured_temperature_average, full_data$predict_temp_avg)
cor.test(full_data$measured_temperature_min, full_data$predict_temp_min)
cor.test(full_data$measured_temperature_max, full_data$predict_temp_max)

# --- 9. Create Composite Dataset ---

completed_dataset <- full_data %>%
  mutate(
    composite_temperature_max = ifelse(is.na(measured_temperature_max), predict_temp_max, measured_temperature_max),
    composite_temperature_min = ifelse(is.na(measured_temperature_min), predict_temp_min, measured_temperature_min),
    composite_temperature_avg = ifelse(is.na(measured_temperature_average), predict_temp_avg, measured_temperature_average),
  )

# --- 10. Visualize Composite Metrics ---
temp_viz <- completed_dataset %>% 
  pivot_longer(cols = c(silo_max_temperature, composite_temperature_max, measured_temperature_max), names_to = "temperature_metrics", values_to = "temperature_values") %>% 
ggplot() +
  geom_point(aes(x = date, y = temperature_values, colour = temperature_metrics)) +
  facet_wrap(. ~ site)

save_plot(temp_viz, "results/supplementary_information/si2_temperature_modelled_per_site.tiff", width = 10, height = 8)

# --- 11. Save Composite Dataset ---

save_csv(completed_dataset, "data/processed_composite_climatic.csv")
