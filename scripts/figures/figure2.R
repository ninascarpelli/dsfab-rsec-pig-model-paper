library(ggplot2)
library(tidyverse)
library(lubridate)

source("R/load_data.R")
source("R/save_utils.R")

#rain ----
rain <- load_csv("data/processed_composite_climatic.csv")

rain$date <- ymd(rain$date, tz = "Australia/Brisbane")

data_collection_start <- dmy("26/06/2023", tz = "Australia/Brisbane")
data_collection_end <- dmy("08/06/2024", tz = "Australia/Brisbane")

rain <- rain %>% filter(date >= data_collection_start & date <= data_collection_end)

# Assign week and season, then calculate weekly rainfall

rain <- rain %>% 
  mutate(week = week(date),
         month = month(date),
         season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                            week > 3 & week <= 12 ~ "late_wet",
                            week > 12 & week <= 25 ~ "early_dry",
                            week >= 26 & week <= 35 ~ "dry",
                            week > 35 & week <= 45 ~ "late_dry",
                            TRUE ~ "wrong")) %>%
  group_by(week, season) %>% 
  summarise(weekly_rain = sum(silo_rain)) %>% 
  ungroup()


rain$season <- as.factor(rain$season)

rain$season <- factor(rain$season, levels = c("dry", "late_dry", "wet", "late_wet", "early_dry"))
#creating a seasonal dataframe just to be able to include that in the plot
seasons <- data.frame(
  xmin = as.numeric(c(1,3,13,26,36,46)),
  xmax = as.numeric(c(3,13,23,36,46,53)),
  seasons = c("wet", "late wet", "early dry", "dry", "late dry", "wet"),
  ymin = -Inf,
  ymax = Inf)

seasons <- seasons %>% mutate(date_min = case_when(xmin >= 26 & xmin <= 53 ~ as.Date(paste("2023", xmin, 1, sep = "-"), format = "%Y-%U-%u"),
                                                   xmin >= 1 & xmin <= 23 ~ as.Date(paste("2024", xmin, 1, sep = "-"), format = "%Y-%U-%u")),
                              date_max = case_when(xmax >= 26 & xmax <= 53 ~ as.Date(paste("2023", xmax, 1, sep = "-"), format = "%Y-%U-%u"),
                                                   xmax >= 1 & xmax <= 23 ~ as.Date(paste("2024", xmax, 1, sep = "-"), format = "%Y-%U-%u"))
                              
)

seasons$date_max[6] <- as.Date("2023-12-31")
seasons$date_min[1] <- as.Date("2024-01-01")
seasons$date_max[3] <- as.Date("2024-06-08")

season_colours <- c("dry" = "#f72585", "early dry" = "#7d1362", "late dry" = "#b967ff", "wet" = "#4cc9f0", "late wet" = "#05ffa1")

seasons$seasons <- factor(seasons$seasons, levels = c("dry", "late dry", "wet", "late wet", "early dry"))

#with 5 seasons
c <- rain %>% 
  mutate(date = case_when(week >= 26 & week <= 53 ~ as.Date(paste("2023", week, 1, sep = "-"), format = "%Y-%U-%u"),
                          week >= 1 & week <= 25 ~ as.Date(paste("2024", week, 1, sep = "-"), format = "%Y-%U-%u"))) 
c$date[51] <- as.Date("2023-12-31")

plot <- ggplot() +
  geom_line(aes(x = date, y = weekly_rain), data = c) +
  geom_rect(aes(xmin = date_min, xmax = date_max, ymin = ymin, ymax = ymax, fill = seasons), data = seasons, alpha = 0.2) +
  geom_text(
    data = c,
    aes(x = date, y = 0, label = week),  # Adjust y to position labels
    angle = 90, hjust = 1, size = 3, vjust = 0.5
  ) +
  scale_fill_manual(values = season_colours, labels = c("Dry", "Late dry", "Wet", "Late wet", "Early dry")) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2))) +
  labs(x = "Date", y = "Cumulative weekly rain (mm)", fill = "Seasons") +
  theme_bw() +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    legend.key.size = unit(1.5, "lines")
  )



save_plot(plot, "results/figures/figure2.tiff", width = 10, height = 8)
