#### Simulation ####
# Packages
library(tidyverse)
library(GGally)

# Original data
epilepsy <- read.csv(file = "epilepsy.csv", sep = ";")
epilepsy <- epilepsy %>% 
  rename(
    seizures_baseline = number.of.seizures.at.baseline, 
    seizures_treatment = number.of.seizures.under.treatment, 
    time_study = time.in.study..days.,
    drop_out = drop.out,
    time_baseline = time.to.baseline.number
  ) %>% 
  mutate(
    treatment = factor(treatment, levels = c(0, 1), 
                       labels = c("placebo", "new")),
    drop_out = factor(drop_out, levels = c(0, 1), labels = c("No", "Yes")), 
    censor = factor(censor, levels = c(0, 1), labels = c("No", "Yes")),
    response = factor(if_else(
      condition = seizures_treatment <= seizures_baseline & drop_out == "No", 
      true = 1, 
      false = 0
    ), levels = c(0, 1), labels = c("No", "Yes"))
  )
summary(object = epilepsy)

# Visualization
# pdf(file = "epilepsy.pdf", width = 13)
# ggpairs(data = epilepsy, mapping = aes(colour = 1), title = "Original data",
#         upper = list(continuous = "points",
#                      combo = "box",
#                      discrete = "facetbar"),
#         lower = list(continuous = "points",
#                      combo = "box",
#                      discrete = "facetbar"))
# dev.off()

#### 1. Data Frame ####
set.seed(seed = 42)

# Creating the data frame.
# subject
simulation <- data.frame(subject = 1:200) %>% 
  mutate(

# treatment    
    treatment = factor(rbinom(n = length(x = subject), size = 1, prob = 0.5), 
                       levels = c(0, 1), labels = c("placebo", "new")), 

# seizures_baseline with individual lambda and more than three seizures.
    lambda_baseline = rgamma(n = 200, shape = 5, scale = 2), 
    seizures_baseline = sapply(X = lambda_baseline, FUN = rpois, n = 1)
  ) %>% 
  filter(seizures_baseline > 3) %>% 

# seizures_treatment
  mutate(
    seizures_treatment = sapply(X = exp(
      log(lambda_baseline * 2) - ((as.integer(treatment) - 1) * 0.13 + 0.2)
    ), FUN = rpois, n = 1)
  ) %>% 
  
# deleting coloumn of lambdas
  select(-lambda_baseline) %>% 
  mutate(
    
# time_study
    time_study = round(
      pmin(rexp(n = length(subject), rate = -log(0.8) / 56), 56)
    ), 
    
# dropout
    drop_out = factor(
      if_else(condition = time_study == 56, true = 0, false = 1), 
      levels = c(0, 1), 
      labels = c("No", "Yes")
    ), 

# time_baseline
    time_baseline = round(unlist(lapply(X = mapply(
      FUN = rexp, 
      n = seizures_baseline, 
      rate = seizures_treatment / time_study
    ), FUN = sum))),
    time_baseline = if_else(
      condition = time_baseline <= time_study & seizures_treatment != 0, 
      true = time_baseline, 
      false = time_study
    ), 
    time_baseline = if_else(condition = time_baseline > 0, 
                            true = time_baseline, 
                            false = 1),
# censor
    censor = factor(if_else(
      condition = seizures_treatment < seizures_baseline, 
      true = 1, 
      false = 0
    ), levels = c(0, 1), labels = c("No", "Yes")),

# response
    response = factor(if_else(
      condition = seizures_treatment <= seizures_baseline & drop_out == "No", 
      true = 1, 
      false = 0
    ), levels = c(0, 1), labels = c("No", "Yes"))
  )
summary(object = simulation)

# # Visualization
# pdf(file = "simulation.pdf", width = 13)
# ggpairs(data = simulation, mapping = aes(colour = 1), title = "Simulation",
#         upper = list(continuous = wrap(funcVal = "points"), 
#                      combo = wrap(funcVal = "box"), 
#                      discrete = wrap(funcVal = "facetbar")), 
#         lower = list(continuous = wrap(funcVal = "points"), 
#                      combo = wrap(funcVal = "box"), 
#                      discrete = wrap(funcVal = "facetbar")))
# dev.off()