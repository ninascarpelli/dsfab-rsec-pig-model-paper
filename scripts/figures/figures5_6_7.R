#plotting simulated results and predictions
library(ggplot2)
library(tidyverse)

source("R/load_data.R")
source("R/save_utils.R")

sound_and_image <- load_csv("results/sound_and_image_predicted_results.csv")
image <- load_csv("results/image_predicted_results.csv")
sound <- load_csv("results/sound_predicted_results.csv")

sound_and_image$season_weekly <- factor(sound_and_image$season_weekly, levels = c("dry", "late_dry", "wet", "late_wet", "early_dry"))

sound$season_weekly <- factor(sound$season_weekly, levels = c("dry", "late_dry", "wet", "late_wet", "early_dry"))

image$season_weekly <- factor(image$season_weekly, levels = c("dry", "late_dry", "wet", "late_wet", "early_dry"))


# Convert factor to numeric for shifting
sound_and_image <- sound_and_image %>% group_by(season_weekly) %>% 
  mutate(x_num = cur_group_id() - 0.2,
         method = "both") %>% 
  ungroup() %>% 
  group_by(removal_num_weekly) %>% 
  mutate(x_num_removal = cur_group_id() - 0.2) %>% 
  ungroup() %>% 
  group_by(removal_freq_weekly) %>% 
  mutate(x_freq_removal = cur_group_id() - 0.2)


image <- image %>% group_by(season_weekly) %>% 
  mutate(x_num = cur_group_id(),
         method = "image") %>% 
  ungroup() %>% 
  group_by(removal_num_weekly) %>% 
  mutate(x_num_removal = cur_group_id())%>% 
  ungroup() %>% 
  group_by(removal_freq_weekly) %>% 
  mutate(x_freq_removal = cur_group_id())

sound <- sound %>% group_by(season_weekly) %>% 
  mutate(x_num = cur_group_id() + 0.2,
         method = "sound") %>% 
  ungroup() %>% 
  group_by(removal_num_weekly) %>% 
  mutate(x_num_removal = cur_group_id() + 0.2)%>% 
  ungroup() %>% 
  group_by(removal_freq_weekly) %>% 
  mutate(x_freq_removal = cur_group_id() + 0.2)

all_predicted_results <- rbind(sound_and_image, sound, image)

# Plot: Detection by season
(season_plot <- #sim_all %>%
  # distinct(season_weekly, .keep_all = TRUE) %>%
  ggplot() +
  geom_pointrange(aes(y = det.Predicted, x = x_num, ymin = det.lower, ymax = det.upper, colour = method), data = all_predicted_results) +
  # geom_pointrange(aes(y = det.Predicted, x = x_num, ymin = det.lower, ymax = det.upper), colour = "green", data = image) +
  # geom_pointrange(aes(y = det.Predicted, x = x_num, ymin = det.lower, ymax = det.upper), colour = "#6699cc", data = sound_and_image) +
  labs(x = "Seasons", y = "Predicted detection probability", colour = "Detection method") +
  scale_colour_manual(values = c("#6699cc", "green", "magenta"), labels = c("Combined (sound and image)", "Image", "Sound")) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5), # numeric positions
    labels = c("Dry", "Late dry", "Wet", "Late wet", "Early dry") # original factor labels
  ) +
  theme_bw())
save_plot(season_plot, "results/figures/figure5.tiff", width = 10, height = 8)

# Plot: Detection vs Rain
labels <- c(dry = "Dry", late_dry = "Late dry", wet = "Wet", late_wet = "Late wet", early_dry = "Early Dry")

labels1 <- c(both = "Combined", image = "Image", sound = "Sound")
# rain_plot <- sim_rain %>%
#   ggplot() +
#   geom_line(aes(y = det$Predicted, x = rain_weekly, colour = season_weekly)) +
#   geom_ribbon(aes(y = det$Predicted, x = rain_weekly, ymin = det$lower, ymax = det$upper, fill = season_weekly), alpha = 0.2) +
#   labs(x = "Rain (mm) estimates per week", y = "Predicted probability", fill = "Seasons", title = "C") +
#   scale_fill_discrete(labels = c("Dry", "Late dry", "Late wet", "Wet")) +
#   scale_colour_discrete(guide = "none") +
#   theme_bw() +
#   facet_wrap(. ~ season_weekly, scales = "free_x", labeller = labeller(season_weekly = labels))
# ggsave("U:/work/acoustics/00_working/nina/04_pig-recogniser/pig-model-paper/results/ct_detection_rain.tiff")

# Plot: Detection vs Temperature
(removal_num_plot <- all_predicted_results %>%
  ggplot() +
  geom_pointrange(aes(y = det.Predicted, x = x_num_removal, ymin = det.lower, ymax = det.upper, colour = method), position = "jitter")+
  # geom_ribbon(aes(y = det.Predicted, x = removal_num_weekly, ymin = det.lower, ymax = det.upper, colour = method, fill = method, ), alpha = 0.2) +
    labs(x = "Weekly pig removal number (simulated)", y = "Predicted detection probability", colour = "Detection method", title = "A") +
    scale_colour_manual(values = c("#6699cc", "green", "magenta"), labels = c("Combined (sound and image)", "Image", "Sound")) +
    scale_fill_manual(values = c("#6699cc", "green", "magenta"), labels = c("Combined (sound and image)", "Image", "Sound")) +
    scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5, 6)) +
  # geom_ribbon(aes(y = det$Predicted, x = rain_weekly, ymin = det$lower, ymax = det$upper, fill = season_weekly), alpha = 0.2) +
  # labs(x = expression(paste("Rain (mm) estimates per week")), y = "Predicted probability", fill = "Seasons", title = "B") +
  # scale_fill_discrete(labels = c("Dry", "Early Dry", "Late dry", "Late wet", "Wet")) +
  # scale_colour_discrete(guide = "none") +
  theme_bw() +
  facet_wrap(method ~ season_weekly, scales = "free_x", labeller = labeller(season_weekly = labels, method = labels1), nrow = 3))

(removal_freq_plot <- all_predicted_results %>%
    ggplot() +
    geom_pointrange(aes(y = det.Predicted, x = x_freq_removal, ymin = det.lower, ymax = det.upper, colour = method))+
    # geom_ribbon(aes(y = det.Predicted, x = removal_freq_weekly, ymin = det.lower, ymax = det.upper, colour = method, fill = method, ), alpha = 0.2) +
    labs(x = "Weekly pig control frequency (simulated)", y = "Predicted detection probability", colour = "Detection method", title = "B") +
    scale_colour_manual(values = c("#6699cc", "green", "magenta"), labels = c("Combined (sound and image)", "Image", "Sound")) +
    scale_fill_manual(values = c("#6699cc", "green", "magenta"), labels = c("Combined (sound and image)", "Image", "Sound")) +
    scale_x_continuous(
    breaks = c(1, 2), # numeric positions
  labels = c("0", "1") # original factor labels
) +
    # geom_ribbon(aes(y = det$Predicted, x = rain_weekly, ymin = det$lower, ymax = det$upper, fill = season_weekly), alpha = 0.2) +
    # labs(x = expression(paste("Rain (mm) estimates per week")), y = "Predicted probability", fill = "Seasons", title = "B") +
    # scale_fill_discrete(labels = c("Dry", "Early Dry", "Late dry", "Late wet", "Wet")) +
    # scale_colour_manual(guide = "none") +
    # scale_colour_manual(guide = "none") +
    theme_bw() +
    facet_wrap(season_weekly ~ ., scales = "free_x", labeller = labeller(season_weekly = labels)))

grid <- gridExtra::grid.arrange(removal_num_plot, removal_freq_plot, ncol = 2)
save_plot(grid, "results/figures/figure6.tiff", width = 17, height = 10)


(temp_plot <- all_predicted_results %>%
  filter(temp_weekly != is.na(temp_weekly)) %>% 
  ggplot() +
  geom_smooth(aes(y = det.Predicted, x = temp_weekly, colour = method), se = F) +
  geom_ribbon(aes(y = det.Predicted, x = temp_weekly, ymin = det.lower, ymax = det.upper, fill = method), alpha = 0.2) +
  labs(x = expression(paste("Maximum weekly temperature (", degree, "C) (simulated)")), y = "Predicted detection probability", colour = "Detection method", title = "A") +
    scale_colour_manual(values = c("#6699cc", "green", "magenta"), labels = c("Combined (sound and image)", "Image", "Sound")) +
    scale_fill_manual(values = c("#6699cc", "green", "magenta"), labels = c("Combined (sound and image)", "Image", "Sound")) +
  # geom_ribbon(aes(y = det$Predicted, x = rain_weekly, ymin = det$lower, ymax = det$upper, fill = season_weekly), alpha = 0.2) +
  # labs(x = expression(paste("Rain (mm) estimates per week")), y = "Predicted probability", fill = "Seasons", title = "B") +
  # scale_fill_discrete(labels = c("Dry", "Early Dry", "Late dry", "Late wet", "Wet")) +
  # scale_colour_discrete(guide = "none") +
  theme_bw() +
  facet_wrap(. ~ method, scales = "free_x", labeller = labeller(method = labels1)))

(rain_plot <- all_predicted_results %>%
    filter(rain_weekly != is.na(rain_weekly)) %>% 
    ggplot() +
    geom_smooth(aes(y = det.Predicted, x = rain_weekly, colour = method), se = F) +
    geom_ribbon(aes(y = det.Predicted, x = rain_weekly, ymin = det.lower, ymax = det.upper, fill = method), alpha = 0.2) +
    labs(x = "Weekly rainfall (mm) (simulated)", y = "Predicted detection probability", colour = "Detection method", title = "B") +
    scale_colour_manual(values = c("green", "magenta"), labels = c("Image", "Sound")) +
    scale_fill_manual(values = c("green", "magenta"), labels = c("Image", "Sound")) +
    # geom_ribbon(aes(y = det$Predicted, x = rain_weekly, ymin = det$lower, ymax = det$upper, fill = season_weekly), alpha = 0.2) +
    # labs(x = expression(paste("Rain (mm) estimates per week")), y = "Predicted probability", fill = "Seasons", title = "B") +
    # scale_fill_discrete(labels = c("Dry", "Early Dry", "Late dry", "Late wet", "Wet")) +
    # scale_colour_discrete(guide = "none") +
    theme_bw() +
    facet_wrap(. ~ method, scales = "free_x", labeller = labeller(method = labels1)))




# Combine all plots
grid <- gridExtra::grid.arrange(temp_plot, rain_plot, nrow = 2)
save_plot(grid, "results/figures/figure7.tiff", width = 15, height = 9)



