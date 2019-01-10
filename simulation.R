#### Original Data ####
# Packages
library(MASS)
library(survival)
library(tidyverse)
library(GGally)

# Data
epilepsy <- read.csv(file = "epilepsy.csv", sep = ";") %>% 
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
    ), labels = c("No", "Yes"))
  )
summary(object = epilepsy)

# # Visualization
# pdf(file = "epilepsy.pdf", width = 13)
# ggpairs(
#   data = epilepsy,
#   mapping = aes(colour = 1), title = "Original data",
#   upper = list(continuous = "points", combo = "box", discrete = "facetbar"),
#   lower = list(continuous = "points", combo = "box", discrete = "facetbar")
# )
# dev.off()

#### Simulation ####
# Function for simulating the data.
sim <- function(n = 200, times = 10000, delta = 0.13) {
  
# Creation of the data.
  simulation <- function(n) {
    tibble(
      
# subject
      subject = 1:n, 
      
# treatment
      treatment = rbinom(n = n, size = 1, prob = 0.5), 

# seizures_baseline with individual lambda and more than three seizures.
      lambda_baseline = rgamma(n = n, shape = 5, scale = 2), 
      seizures_baseline = sapply(X = lambda_baseline, FUN = rpois, n = 1)
    ) %>% 
      filter(seizures_baseline > 3) %>% 
      
# seizures_treatment
      mutate(
        seizures_treatment = sapply(X = exp(
          log(lambda_baseline * 2) - (treatment * delta + 0.2)
        ), FUN = rpois, n = 1),

# time_study
        time_study = round(pmin(rexp(n = length(subject),
                                     rate = -log(0.8) / 56), 56)),
        time_study = if_else(condition = time_study != 0, true = time_study, 
                             false = 1), 

# drop_out
        drop_out = if_else(condition = time_study == 56, true = 0, false = 1), 

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
        censor = if_else(condition = seizures_treatment < seizures_baseline,
                         true = 0, false = 1),

# response
        response = if_else(
          condition = seizures_treatment <= seizures_baseline & 
            drop_out == 0,
          true = 1,
          false = 0
        ),

# seizures_baseline_log 
        seizures_baseline_log = log(seizures_baseline), 

# time_study_log
        time_study_log = log(time_study)
      ) %>% 
      
# Deleting columns of lambdas
      select(-lambda_baseline, -lambda_time)
  }
  
# Replication of the data.
  lapply(X = as.list(rep(n, times = times)), FUN = simulation)
}

#### Chi square ####
p_value <- function(data = data) {
  
  # logit
  logit <- summary(object = glm(
    formula = data$response ~ data$treatment + data$seizures_baseline_log,
    family = binomial(link = "logit")
  ))
  
  # negative binomial
  neg_bin_summary <- summary(object = glm.nb(
    formula = data$seizures_treatment ~ data$treatment + 
      data$seizures_baseline_log + offset(data$time_study_log)
  ))
  
  # log_rank and surv
  surv <- Surv(time = data$time_baseline, event = data$censor)
  log_rank <- survdiff(formula = surv ~ data$treatment)
  
  # cox
  cox_summary <- summary(object = coxph(
    formula = surv ~ data$treatment + data$seizures_baseline
  ))
  
  # cox2
  cox2_summary <- summary(object = coxph(formula = surv ~ data$treatment))
  
  # data frame
  tibble(
    chi_square = chisq.test(x = data$treatment, y = data$response)$p.value,
    logit_treatment = logit$coefficients[2, 4],
    logit_seizures_baseline_log = logit$coefficients[3, 4],
    neg_bin_treatment = neg_bin_summary$coefficients[2, 4],
    neg_bin_seizures_baseline = neg_bin_summary$coefficients[3, 4],
    log_rank = 1 - pchisq(q = log_rank$chisq, df = 1), 
    cox_treatment = cox_summary$coefficients[1, 5], 
    cox_seizures_baseline = cox_summary$coefficients[2,5], 
    cox2_treatment = cox_summary$coefficients[1, 5]
  )
}


set.seed(seed = 42)
simulation <- sim(times = 10, delta = 0.13)
summary(object = simulation[[1]])

# # Visualization
# pdf(file = "simulation_1.pdf", width = 13)
# ggpairs(data = simulation[[1]], mapping = aes(colour = 1),
#         title = "Delta = 0",
#         upper = list(continuous = wrap(funcVal = "points"),
#                      combo = wrap(funcVal = "box"),
#                      discrete = wrap(funcVal = "facetbar")),
#         lower = list(continuous = wrap(funcVal = "points"),
#                      combo = wrap(funcVal = "box"),
#                      discrete = wrap(funcVal = "facetbar")))
# dev.off()

# Je größer delta, desto weniger Anfälle in der treatment periode.
# Je größer delta, desto länger die time_baseline
# Ist der treatment effect hoch gehen Anzahl der Anfälle zurück. 
#   -> Die Wirkung des treatments wird erhöht.

set.seed(seed = 42)
p <- bind_rows(lapply(X = sim(times = 10, delta = 0), FUN = p_value))
p
# Treatment effect with the example of the Chi square test:
# Treatment effect of zero: p-value near zero
# Treatment effect of one: p-value between 0.3 and 1
# 0.15: half of the p-values are below 0.05