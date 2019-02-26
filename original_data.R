#### Original Dataset ####
# Packages
library(tidyverse)
library(GGally)

# Data
epilepsy <- read.csv(file = "data/epilepsy.csv", sep = ";") %>% 
  rename(
    seizures_baseline = number.of.seizures.at.baseline, 
    seizures_treatment = number.of.seizures.under.treatment, 
    time_study = time.in.study..days.,
    drop_out = drop.out,
    time_baseline = time.to.baseline.number
  ) %>% 
  mutate(
    treatment = factor(treatment, labels = c("placebo", "new")), 
    drop_out = factor(drop_out, labels = c("No", "Yes")), 
    censor = factor(censor, labels = c("No", "Yes")), 
    response = factor(if_else(
      condition = seizures_treatment <= seizures_baseline & drop_out == "No", 
      true = 1, 
      false = 0
    ), labels = c("No", "Yes")), 
    seizures_baseline_log = log(seizures_baseline), 
    time_study_log = log(time_study)
  )
summary(object = epilepsy)

# Visualization
# pdf(file = "plots/original_data.pdf", width = 13)
ggpairs(
  data = epilepsy, 
  mapping = aes(colour = 1), 
  title = "Original data",
  upper = list(continuous = "points", combo = "box", discrete = "facetbar"),
  lower = list(continuous = "points", combo = "box", discrete = "facetbar")
)
# dev.off()