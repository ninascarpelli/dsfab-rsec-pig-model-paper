# --- Load Required Libraries ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)

source("R/load_data.R")
source("R/save_utils.R")


# --- Load Detection Data ---
all_detections <- load_txt("data/raw_all_recogniser_versions_validated.txt")

# --- Plot Detection Confidence Distributions (excluding v6-9-3) ---
all_detections_plot <- all_detections %>%
  filter(new_recog_version != "v6-9-3") %>%
  mutate(confidence = round(as.numeric(confidence), 1)) %>%
  filter(confidence >= 0.5) %>%
  filter(standardised_recog_version %in% c("v09", "v10", "v11", "v13", "v17", "v18", "v19")) %>%
  filter(eval != "NA") %>%
  group_by(standardised_recog_version, eval) %>%
  mutate(n_detections = n()) %>%
  ggplot() +
  geom_bar(aes(x = confidence, fill = as.factor(eval))) +
  labs(x = "Confidence score", y = "Number of detections", fill = "Validation", title = "A") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  facet_wrap(standardised_recog_version ~ n_detections, scales = "free_y")

# --- Combine All Positive Detections ---
positive_detection <- all_detections %>%
  separate(dt, into = c("date_time", "timezone"), sep = "[+]", remove = FALSE) %>%
  filter(new_recog_version != "v6-9-3") %>%
  filter(eval == 1, confidence >= 0.5) %>%
  filter(standardised_recog_version %in% c("v09", "v10", "v11", "v13", "v17", "v18", "v19"))

# --- Plot Positive Detections by Site ---
pos_per_site <- positive_detection %>%
  group_by(standardised_recog_version) %>%
  mutate(total_n_detection = n()) %>%
  ggplot() +
  geom_bar(aes(x = site, fill = standardised_recog_version)) +
  labs(x = "Site", y = "Number of detections", fill = "Recogniser version", title = "C") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"), legend.position = "none") +
  facet_wrap(standardised_recog_version ~ total_n_detection)

# --- Plot Positive Detections Over Time ---
positive_detection$date_time <- ymd_hms(positive_detection$date_time)
positive_detection$date <- date(positive_detection$date_time)

pos_per_time <- positive_detection %>%
  group_by(standardised_recog_version) %>%
  mutate(total_n_detection = n()) %>%
  ggplot() +
  geom_bar(aes(x = date, fill = standardised_recog_version)) +
  labs(x = "Time", y = "Number of detections", fill = "Recogniser version", title = "D") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"), legend.position = "none") +
  facet_wrap(standardised_recog_version ~ total_n_detection)

# --- Calculate Recall (True Positives) ---

# Group detections by datetime and site
grouped_data <- positive_detection %>%
  group_by(dt, site) %>%
  summarise(detected_by = paste(unique(standardised_recog_version), collapse = ", "), .groups = "drop")

# Spread recogniser versions into columns
comparison_wide <- grouped_data %>%
  separate_rows(detected_by, sep = ", ") %>%
  pivot_wider(names_from = detected_by, values_from = detected_by, values_fill = NA) %>%
  dplyr::select(dt, site, everything())

# Count true positives per recogniser
detection_counts <- comparison_wide %>%
  dplyr::select(-c(dt, site)) %>%
  summarise(across(everything(), ~ sum(!is.na(.), na.rm = TRUE))) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("recogniser_version") %>%
  rename(tp = V1) %>%
  mutate(overall_positive_detections = nrow(comparison_wide),
         fn = overall_positive_detections - tp,
         recall = tp / (tp + fn))

# --- Calculate False Positives ---

negative_detection <- all_detections %>%
  filter(new_recog_version != "v6-9-3") %>%
  filter(eval == 0, confidence >= 0.5) %>%
  filter(standardised_recog_version %in% c("v09", "v10", "v11", "v13", "v17", "v18", "v19"))

grouped_neg_data <- negative_detection %>%
  group_by(dt, site) %>%
  summarise(detected_by = paste(unique(standardised_recog_version), collapse = ", "), .groups = "drop")

comparison_neg_wide <- grouped_neg_data %>%
  separate_rows(detected_by, sep = ", ") %>%
  pivot_wider(names_from = detected_by, values_from = detected_by, values_fill = NA) %>%
  dplyr::select(dt, site, everything())

fp_counts <- comparison_neg_wide %>%
  dplyr::select(-c(dt, site)) %>%
  summarise(across(everything(), ~ sum(!is.na(.), na.rm = TRUE))) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("recogniser_version") %>%
  rename(fp = V1)

# --- Combine Metrics and Calculate Precision & F1 Score ---

metrics <- left_join(fp_counts, detection_counts) %>%
  mutate(
    precision = tp / (tp + fp),
    f1score = (2 * precision * recall) / (precision + recall)
  )

# --- Plot Recall and Precision per Recogniser ---
metrics_plot <- metrics %>%
  pivot_longer(cols = c(recall, precision), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = recogniser_version)) +
  geom_col(aes(y = value, fill = recogniser_version)) +
  labs(x = "Recogniser version", y = "Metric value", fill = "Recogniser version", title = "B") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"), legend.position = "none") +
  facet_wrap(~metric)

# --- Save Plots and Metrics ---

save_plot(metrics_plot, "results/figures/figure3_recogniser_metrics_plot.tiff", 12, 10)

grid <- gridExtra::grid.arrange(all_detections_plot, metrics_plot, pos_per_site, pos_per_time, nrow = 2)

save_plot(grid, "results/supplementary_information/si4_recogniser_summary_versions.tiff", 18, 10)

save_csv(metrics, "results/supplementary_information/si7_summary_hourly_recall_and_precision_per_version.csv")

# --- Summary of Chosen Recogniser Versions ---

chosen_versions <- all_detections %>%
  separate(dt, into = c("date_time", "timezone"), sep = "[+]", remove = FALSE) %>%
  filter(new_recog_version != "v6-9-3") %>%
  filter(eval == 1) %>%
  filter(standardised_recog_version %in% c("v13", "v17")) %>%
  distinct(standardised_recog_version, date_time) %>%
  group_by(standardised_recog_version) %>%
  summarise(total_num_detections = n())

save_csv(chosen_versions, "results/tables/recogniser_detection_counts_per_hour_per_chosen_version.csv")
