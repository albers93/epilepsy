#### Original Data ####
# Packages
library(tidyverse)
library(GGally)
library(microbenchmark)

# Data
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

#### Simulation ####
# Function for simulating the data.
sim <- function(n = 200, times = 10000) {
  
# Creation of the data.
  simulation <- function(n) {
    tibble(
      
# subject
      subject = 1:n, 
      
# treatment
      treatment = factor(rbinom(n = n, size = 1, prob = 0.5), 
                         levels = c(0, 1), labels = c("placebo", "new")), 

# seizures_baseline with individual lambda and more than three seizures.
      lambda_baseline = rgamma(n = n, shape = 5, scale = 2), 
      seizures_baseline = sapply(X = lambda_baseline, FUN = rpois, n = 1)
    ) %>% 
      filter(seizures_baseline > 3) %>% 
      
# seizures_treatment
      mutate(
        seizures_treatment = sapply(X = exp(
          log(lambda_baseline * 2) - ((as.integer(treatment) - 1) * 0.13 + 0.2)
        ), FUN = rpois, n = 1),

# time_study
        time_study = round(pmin(rexp(n = length(subject),
                                     rate = -log(0.8) / 56), 56)),
        time_study = if_else(condition = time_study != 0, true = time_study, 
                             false = 1), 

# drop_out
        drop_out = factor(
          if_else(condition = time_study == 56, true = 0, false = 1), 
          levels = c(0, 1), 
          labels = c("No", "Yes")
        ), 

# time_baseline with individual lambda.
        lambda_time = if_else(condition = seizures_treatment != 0, 
                              true = seizures_treatment / time_study, 
                              false = 1e-10), 
        time_baseline = round(unlist(lapply(X = mapply(FUN = rexp, 
                                                       n = seizures_baseline, 
                                                       rate = lambda_time), 
                                            FUN = sum))),
        time_baseline = if_else(condition = time_baseline <= time_study, 
                                true = time_baseline, false = time_study),
        time_baseline = if_else(condition = time_baseline > 0, 
                                true = time_baseline, false = 1),

# censor
        censor = factor(if_else(
          condition = seizures_treatment < seizures_baseline, 
          true = 1, 
          false = 0
        ), levels = c(0, 1), labels = c("No", "Yes")), 

# response
        response = factor(if_else(
          condition = seizures_treatment <= seizures_baseline & 
            drop_out == "No",
          true = 1,
          false = 0
        ), levels = c(0, 1), labels = c("No", "Yes"))
      ) %>% 
      
# Deleting columns of lambdas
      select(-lambda_baseline, -lambda_time)
  }
  
# Replication of the data.
  lapply(X = as.list(rep(n, times = times)), FUN = simulation)
}

set.seed(seed = 42)
simulation <- sim(times = 1)
summary(object = simulation[[1]])

microbenchmark(simulation <- sim(times = 100), times = 1)

# # Visualization
# pdf(file = "simulation.pdf", width = 13)
# ggpairs(data = simulation[[1]], mapping = aes(colour = 1), 
#         title = "Simulation",
#         upper = list(continuous = wrap(funcVal = "points"),
#                      combo = wrap(funcVal = "box"),
#                      discrete = wrap(funcVal = "facetbar")),
#         lower = list(continuous = wrap(funcVal = "points"),
#                      combo = wrap(funcVal = "box"),
#                      discrete = wrap(funcVal = "facetbar")))
# dev.off()