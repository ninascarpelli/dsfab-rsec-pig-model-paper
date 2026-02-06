library(dplyr)
library(tidyr)

source("pig_model_paper/R/load_data.R")
source("pig_model_paper/R/save_utils.R")


# --- Load and Process Spatial Metadata ---
metadata <- load_csv("pig_model_paper/data/raw_monitoring_points.csv") %>%
  mutate(site = paste0("site-", point),
         lat = round(latitude, 3),
         long = round(longitude, 3)) %>%
  filter(device == "bar") %>%
  dplyr::select(dispname, site, date.start, date.end, latitude, longitude)

metadata$site <- tolower(metadata$site)

save_csv(metadata, "pig_model_paper/data/processed_monitoring_points.csv")
