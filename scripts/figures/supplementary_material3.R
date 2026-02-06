veg <- read.csv("data/processed_vegetation.csv") %>% 
  mutate(site = gsub("bar", "site", point)) %>% 
  dplyr::select(site, everything(), -point)


# site_variables <- left_join(other_vars, veg, by = "site")

# sites <- unique(site_variables$site)

# site_variables <- as.data.frame(site_variables)

# rownames(site_variables) <- site_variables[,1]
# 
# site_variables <- site_variables[,-1]

labels <- as_labeller(c(shrub_cover = "Shrub cover", shrub_height = "Shrub height"))

veg %>% 
  pivot_longer(cols = c("shrub_height", "shrub_cover"), names_to = "variable", values_to = "value") %>% 
  ggplot() +
  geom_col(aes(x = site, y = value, fill = variable)) +
  scale_fill_manual(values = c("#7FFFD4", "#088F8F"), labels = c("Shrub cover", "Shrub height")) +
  labs(y = "Value", fill = "Metric", x = "Site") +
  theme_bw() +
  facet_wrap(.~ variable, scales = "free_y", labeller = labels) +
  theme(legend.position = "none") 
ggsave("results/supplementary_information/si3_shrub_summary.tiff")
