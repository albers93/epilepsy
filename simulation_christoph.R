#### Simulation ####
# Packages
library(MASS)
library(tidyverse)
library(survival)
library(parallel)

# generating datasets
sampling <- function(x) {
  
  # negativ binomial model
  negative_binomial <- function(data) {
    neg_bin_summary <- summary(object = neg_bin <- glm.nb(
      formula = data$seizures_treatment ~ data$treatment + 
        data$seizures_baseline + offset(log(data$time_study))
      ))
    tibble(
      neg_bin_coefficient_treatment = neg_bin$coefficients[2],
      neg_bin_coefficient_seizures_baseline = neg_bin$coefficients[3],
      neg_bin_p_value_treatment = neg_bin_summary$coefficients[2, 4],
      neg_bin_p_value_seizures_baseline = neg_bin_summary$coefficients[3, 4]
    )
  }
  
  # Log-Rank und Cox
  log_rank <- function(data) {
    surv_diff <- survdiff(
      formula = Surv(time = data$time_baseline, 
                     event = data$censor) ~ data$treatment
    )
    tibble(logrank_p_value = 1 - pchisq(surv_diff$chisq, df = 1))
  }
  
  cox_reg <- function(data) {
    cox_summary <- summary(object = cox <- coxph(
      formula = Surv(time = data$time_baseline, 
                     event = data$censor, type = "right") ~ 
        data$treatment + data$seizures_baseline
    ))
    tibble(
      cox_p_value_treatment = cox_summary$coefficients[1, 5], 
      cox_p_value_seizures_baseline = cox_summary$coefficients[2,5], 
      cox_coefficient_seizures_baseline = cox_summary$coefficients[2, 1],
      cox_wald = cox$wald.test
    )
  }
  
  cox_reg2 <- function(data) { 
    # only treatment, surv nur einmal berecnen
    cox_summary <- summary(object = cox <- coxph(
      formula = Surv(time = data$time_baseline, 
                     event = data$censor, type = "right") ~ 
        data$treatment
    ))
    tibble(
      cox_p_value_treatment2 = cox_summary$coefficients[1, 5],
      cox_coefficient_treatment = cox_summary$coefficients[1, 1],
      cox_wald = cox$wald.test
    )
    # 2 p-values für treatment?
  }
  
  # Logit und Chi^2
  logit_reg <- function(data) {
    logit_summary <- summary(object = logit <- glm(
      formula = data$response ~ data$treatment + data$seizures_baseline,
      family = binomial(link = "logit")
    ))
    # hier noch gucken, ob log(seizures)
    tibble(
      logit_coefficient_treatment = logit$coefficients[2],
      logit_coefficient_seizures_baseline = logit$coefficients[3],
      logit_p_value_treatment = logit_summary$coefficients[2, 4],
      logit_p_value_seizures_baseline = logit_summary$coefficients[3, 4],
      oddsratio_treatment = exp(logit$coefficients[2]),
      oddsratio_seizures_baseline = exp(logit$coefficients[3])
    )
  }
  
  chisq <- function(data) {
    chisq <- chisq.test(x = data$treatment, y = data$response)
    tibble(
      chi_square_p_value = chisq$p.value
    )
  }
  
  # function which generates a dataset 
  # and extracts all relevant information in form of a dataframe
  gen_dat <- function(n){
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
          log(lambda_baseline * 2) - (treatment * 0.13 + 0.2)
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
        censor = if_else(
          condition = seizures_treatment < seizures_baseline, 
          true = 0, 
          false = 1
        ), 
        
        # response
        response = if_else(
          condition = seizures_treatment <= seizures_baseline & 
            drop_out == 0,
          true = 1,
          false = 0
        ),
      ) %>% 
      
      # Deleting columns of lambdas
      select(-lambda_baseline, -lambda_time)
  }
  
  data <- gen_dat(n = 200)
  bind_cols(negative_binomial(data = data), log_rank(data = data), 
            cox_reg(data = data), cox_reg2(data = data), 
            logit_reg(data = data), chisq(data = data))
}

# without parralel computation for testing
set.seed(seed = 42)
result <- bind_rows(lapply(X = 1:10, FUN = sampling))

# # parallel computing for faster computation
# cl <- makeCluster(spec = detectCores() - 1)
# clusterEvalQ(cl = cl, expr = lapply(X = c("MASS", "tidyverse", "survival"), 
#                                     FUN = require, character.only = TRUE))
# system.time(results <- parLapply(cl = cl, X = 1:10, fun = sampling))  
# # bei 4Kernen 862 sec -> 14min
# stopCluster(cl = cl)
# system.time(result_df <- do.call(what = "bind_rows", args = results))
