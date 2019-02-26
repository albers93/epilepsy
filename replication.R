#### Replication ####
# Packages
library(tidyverse)
library(GGally)

# Replication
replicate_data <- function(n = 200, delta = 0.13){
  tibble(
    
    # subject
    subject = 1:n, 
    
    # treatment
    treatment = as.factor(rbinom(n = n, size = 1, prob = 0.5)), 
    
    # seizures_baseline with individual lambda and more than three seizures.
    lambda_baseline = rgamma(n = n, shape = 5, scale = 2), 
    seizures_baseline = sapply(X = lambda_baseline, FUN = rpois, n = 1)
  ) %>% 
    filter(seizures_baseline > 3) %>% 
    mutate(
      
      # time_study
      time_study = round(pmin(rexp(n = length(subject),
                                   rate = -log(0.8) / 56), 56)),
      time_study = if_else(condition = time_study != 0, true = time_study, 
                           false = 1), 
      
      # seizures_treatment
      seizures_treatment = sapply(X = exp(
        log(lambda_baseline * time_study / 28) - 
          ((as.numeric(treatment) - 1) * delta + 0.2)
      ), FUN = rpois, n = 1),
      
      # drop_out
      drop_out = as.factor(if_else(condition = time_study == 56, true = 0, 
                                   false = 1)), 
      
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
      censor = as.factor(if_else(
        condition = seizures_treatment < seizures_baseline, 
        true = 0, 
        false = 1
      )), 
      
      # response
      response = as.factor(if_else(
        condition = seizures_treatment <= seizures_baseline & 
          drop_out == 0,
        true = 1,
        false = 0
      )),
      
      # Log-transformations
      seizures_baseline_log = log(seizures_baseline),
      time_study_log = log(time_study)
    ) %>% 
    
    # Deleting columns of lambdas
    select(-lambda_baseline, -lambda_time)
}

set.seed(seed = 42)
dataset <- replicate_data(n = 200, delta = 0.13)
summary(object = dataset)

# Visualization
# pdf(file = "plots/replicated_data.pdf", width = 13)
ggpairs(
  data = dataset, 
  mapping = aes(colour = 1), 
  title = "Replicated data",
  upper = list(continuous = "points", combo = "box", discrete = "facetbar"),
  lower = list(continuous = "points", combo = "box", discrete = "facetbar")
)
# dev.off()