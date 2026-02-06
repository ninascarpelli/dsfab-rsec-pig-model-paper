#calculating veg variables

library(dplyr)
library(ggplot2)

source("pig_model_paper/R/load_data.R")
source("pig_model_paper/R/save_utils.R")

new_sites <- read.csv("pig_model_paper/data/raw_vegetation.csv") %>% 
  group_by(point) %>% 
  summarise(tree_height = mean(c_across(c(tree.height.north, tree.height.east, tree.height.south, tree.height.west)), na.rm = T),
            tree_cover = mean(c_across(c(tree.cover.north, tree.cover.east, tree.cover.south, tree.cover.west)), na.rm = T),
            shrub_height = mean(c_across(c(shrub.height.north, shrub.height.east, shrub.height.south, shrub.height.west)), na.rm = T),
            shrub_cover = mean(c_across(c(shrub.cover.north, shrub.cover.east, shrub.cover.south, shrub.cover.west)), na.rm = T))
new_sites

save_csv(new_sites, "pig_model_paper/data/processed_vegetation.csv")
