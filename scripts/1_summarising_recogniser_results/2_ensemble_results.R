library(dplyr)
library(tidyr)
library(ggplot2)

source("R/load_data.R")
source("R/save_utils.R")

all_detections <- load_txt("data/raw_all_recogniser_versions_validated.txt")


#putting all the results together

positive_detection <- all_detections %>% 
  filter(new_recog_version != "v6-9-3") %>%
  filter(eval == 1) %>% 
  # filter(confidence >= 0.5) %>% 
  filter(standardised_recog_version %in% c("v09", "v10", "v11", "v13", "v17", "v18", "v19")) #these were the latest versions in which the validation etc is comparable
  
  
positive_detection %>% filter(standardised_recog_version == "v13" |
                                standardised_recog_version == "v17") %>% 
  ggplot(aes(x = dt)) + 
  geom_bar(aes(fill = standardised_recog_version)) + 
  facet_wrap(.~standardised_recog_version, scales = "free_y")

positive_detection %>% filter(standardised_recog_version == "v13" |
                                standardised_recog_version == "v17") %>% 
  ggplot(aes(x = as.numeric(confidence))) + 
  geom_bar(aes(fill = standardised_recog_version)) + 
  facet_wrap(.~standardised_recog_version, scales = "free_y")


grouped_data <- positive_detection %>%
  group_by(dt, site, begin_time_s) %>%
  summarise(
    detected_by = paste(unique(standardised_recog_version), collapse = ", "),
    .groups = "drop"
  ) %>%
  ungroup() %>% 
  group_by(dt, site) %>% 
  summarise(n_detections_per_site_date_time = n()) %>% 
  mutate(sp = "pig") %>% 
  mutate(source = "recogniser")
  
save_csv(grouped_data, "results/tables/selected_recogniser_positive_detections.csv")

#only vtest----
#best recall for hourly detections

v17 <- all_detections %>% 
  filter(standardised_recog_version == "v17") %>% #these was one of the best fscore
  filter(eval == 1) #%>% 
# filter(confidence >= 0.5) 

v17 %>% 
  ggplot(aes(x = dt)) + 
  geom_bar(aes(fill = standardised_recog_version)) + 
  facet_wrap(.~standardised_recog_version, scales = "free_y")

v17 %>% 
  ggplot(aes(x = as.numeric(confidence))) + 
  geom_bar(aes(fill = standardised_recog_version)) + 
  facet_wrap(.~standardised_recog_version, scales = "free_y")


grouped_data_v17 <- v17 %>%
  group_by(dt, site, begin_time_s) %>%
  summarise(
    detected_by = paste(unique(standardised_recog_version), collapse = ", "),
    .groups = "drop"
  ) %>%
  ungroup() %>% 
  group_by(dt, site) %>% 
  summarise(n_detections_per_site_date_time = n()) %>% 
  mutate(sp = "pig") %>% 
  mutate(source = "recogniser")

v17$confidence <- as.numeric(v17$confidence)

save_csv(grouped_data_v17, "results/tables/v17_recogniser_positive_detections.csv")


#only v13 ----
# #objectively the best version if you consider fscore - also best precision for hourly detections

v13 <- all_detections %>% 
  filter(standardised_recog_version == "v13") %>% #these was one of the best fscore
  filter(eval == 1) #%>% 
  # filter(confidence >= 0.5) 

v13 %>% 
  ggplot(aes(x = dt)) + 
  geom_bar(aes(fill = standardised_recog_version)) + 
  facet_wrap(.~standardised_recog_version, scales = "free_y")

v13 %>% 
  ggplot(aes(x = as.numeric(confidence))) + 
  geom_bar(aes(fill = standardised_recog_version)) + 
  facet_wrap(.~standardised_recog_version, scales = "free_y")

#getting unique detections across versions----

grouped_data_v13 <- v13 %>%
  group_by(dt, site, begin_time_s) %>%
  summarise(
    detected_by = paste(unique(standardised_recog_version), collapse = ", "),
    .groups = "drop"
  ) %>%
  ungroup() %>% 
  group_by(dt, site) %>% 
  summarise(n_detections_per_site_date_time = n()) %>% 
  mutate(sp = "pig") %>% 
  mutate(source = "recogniser")

save_csv(grouped_data_v13, "results/tables/v13_recogniser_positive_detections.csv", row.names = F)

# ensemble ----
grouped_data_v13 <- grouped_data_v13 %>% 
  mutate(standardised_recog_version = "v13") 
  

grouped_data_v17 <- grouped_data_v17 %>% 
  mutate(standardised_recog_version = "v17")


ensemble <- rbind(grouped_data_v13, grouped_data_v17) %>% 
  group_by(dt, site) %>% 
  summarise(n_detections_per_site_date_time = sum(n_detections_per_site_date_time)) %>% 
  mutate(sp = "pig") %>% 
  mutate(source = "recogniser")


save_csv(ensemble, "data/processed_ensemble_recogniser_results.csv")



