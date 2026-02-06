library(tidyverse)

files <- list.files("U:/work/acoustics/00_working/nina/04_pig-recogniser/birdnet-analyser/pig-recog/classifier-output", pattern = "*.csv", full.names = TRUE, recursive = F)

r <- NULL
re <- NULL

for (f in files) {
  
  r <- load_csv(f) %>% 
    mutate(model_name = gsub(x = basename(f), pattern = "_Params.csv", replacement = "")) %>% 
    mutate(model_name = gsub(x = model_name, pattern = "CustomClassifier", replacement = ""))
  
  re <- rbind(r, re)
  
}

save_csv(re, "U:/work/acoustics/00_working/nina/04_pig-recogniser/pig-model-paper/results/training_config_appendix.csv")
