#testing spatial autocorrelation between sites

set.seed(123)

# --- Load Required Libraries ---
library(tidyverse)    # For data wrangling and visualization
library(ape)          #For Moran's I statistics
library(raster)       #For calculating distances

source("R/load_data.R")
source("R/save_utils.R")


# --- Load Detection Data and Add Week Column ---
df <- load_csv("data/processed_detections.csv") %>%
  mutate(week = week(date)) 


autocorrelation_data <- df %>% 
  group_by(site, latitude, longitude) %>% 
  summarise(count = sum(n_detections_per_site_date_time))


# dist <- as.matrix(dist(cbind(autocorrelation_data$longitude, autocorrelation_data$latitude)))

dist <- pointDistance(cbind(autocorrelation_data$longitude, autocorrelation_data$latitude), lonlat = T)

t <- t(dist)

new <- matrix(NA, nrow = 10, ncol = 10)
new[upper.tri(new)] <- t[upper.tri(t)]
new[lower.tri(new)] <- dist[lower.tri(dist)]
new

dists.inv <- 1/new
diag(dists.inv) <- 0

m <- Moran.I(autocorrelation_data$count, dists.inv)
#p > 0.05 there is no spatial autocorrelation

vector <- as.vector(dist)

vector <- vector[!is.na(vector)]
vector <- vector[vector!=0]

summary(vector)


