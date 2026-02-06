library(tidyverse)
library(dplyr)
library(lubridate)

source("pig_model_paper/R/load_data.R")
source("pig_model_paper/R/save_utils.R")

raw_silo_files <- list.files("pig_model_paper/data/raw_silo", pattern = ".csv", full.names = T)

all_files <- NULL
data <- NULL

for (file in raw_silo_files) {
  
  data <- load_csv(file) %>% 
    dplyr::select(T.Max, T.Min, Rain, Date, Date2, lat, long)
  
  all_files <- rbind(data, all_files)
  
}

all_files$Date <- ymd(all_files$Date)

save_csv(all_files, "pig_model_paper/data/processed_silo_all_data.csv")

#Calculating overall rain for the whole property for the year of data collection + 10 week prior the start of data collection for getting the previous rain

yourka_rain <- all_files %>% 
  filter(Date >= "2023-04-16" & Date <= "2024-06-25") %>% 
  mutate(week = week(Date)) %>% 
  mutate(year = year(Date)) %>% 
  group_by(lat, long) %>% 
  mutate(groups = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(year, week, groups) %>% 
  mutate(cumulative_rain = sum(Rain))

yourka_rain %>% ggplot() +
  geom_line(aes(x = Date, y = cumulative_rain, colour = as.factor(groups))) 

yourka_rain <- yourka_rain %>% 
      mutate(season = case_when(week > 45 & week <= 53 | week >= 1 & week <= 3 ~ "wet",
                          week > 3 & week <= 12 ~ "late_wet",
                          week > 12 & week <= 25 ~ "early_dry",
                          week >= 26 & week <= 35 ~ "dry",
                          week > 35 & week <= 45 ~ "late_dry",
                          #week > 45 & week <= 53 ~ "wet2",
                          TRUE ~ "wrong")) 
  

#creating a seasonal dataframe just to be able to include that in the plot
seasons <- data.frame(
  xmin = as.numeric(c(1,3,13,16,26,36,46)),
  xmax = as.numeric(c(3,13,23,25,36,46,53)),
  seasons = c("wet", "late wet", "early dry", "early dry", "dry", "late dry", "wet"),
  ymin = -Inf,
  ymax = Inf)

seasons <- seasons %>% mutate(date_min = case_when(xmin >= 16 & xmin <= 53 ~ as.Date(paste("2023", xmin, 1, sep = "-"), format = "%Y-%U-%u"),
                                                   xmin >= 1 & xmin <= 23 ~ as.Date(paste("2024", xmin, 1, sep = "-"), format = "%Y-%U-%u"),),
                              date_max = case_when(xmax >= 25 & xmax <= 53 ~ as.Date(paste("2023", xmax, 1, sep = "-"), format = "%Y-%U-%u"),
                                                   xmax >= 1 & xmax <= 23 ~ as.Date(paste("2024", xmax, 1, sep = "-"), format = "%Y-%U-%u"))
                              
)

seasons$date_max[7] <- as.Date("2023-12-31")
seasons$date_min[1] <- as.Date("2024-01-01")
seasons$date_max[4] <- as.Date("2023-06-25")

season_colours <- c("dry" = "#f72585", "early dry" = "#7d1362", "late dry" = "#b967ff", "wet" = "#4cc9f0", "late wet" = "#05ffa1")

#with 5 seasons
# c <- rain %>% 
#   mutate(date = case_when(week >= 26 & week <= 53 ~ as.Date(paste("2023", week, 1, sep = "-"), format = "%Y-%U-%u"),
#                           week >= 1 & week <= 25 ~ as.Date(paste("2024", week, 1, sep = "-"), format = "%Y-%U-%u"))) 
# c$date[53] <- as.Date("2023-12-31")

ggplot()+
  geom_line(aes(x = Date, y = cumulative_rain), data = yourka_rain) +
  geom_rect(aes(xmin = date_min, xmax = date_max,  ymin = ymin, ymax = ymax, fill = seasons), data = seasons, alpha = 0.2) +
  scale_fill_manual(values = season_colours, labels = c("Dry", "Early dry", "Late dry", "Late wet", "Wet")) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2))) +
  theme(
    legend.title = element_text(size = 14),  # Adjust title size
    legend.text = element_text(size = 13),   # Adjust text size
    legend.key.size = unit(1.5, "lines")) +     # Adjust key size
  labs(x = "Date", y = "Cumulative weekly rain (mm)", fill = "Seasons") +
  theme_bw() +
  facet_wrap(.~groups)

save_plot("pig_model_paper/results/supplementary_information/seasons_with_previous_rainfall.tiff")


yourka_rain %>% 
  group_by(week, year) %>% 
  filter(Date == min(Date)) %>% 
  dplyr::select(Date, week, year, cumulative_rain, season, groups)


save_csv(yourka_rain, "pig_model_paper/data/processed_seasonal_rain.csv")
