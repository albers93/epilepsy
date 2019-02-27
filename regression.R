#### Regression ####
# Packages
library(MASS)
library(tidyverse)
library(survival)
library(parallel)


#### Function for simulation and regression ####
sampling <- function(x, n, delta) {
  
  # negativ binomial model
  negative_binomial <- function(data) {
    neg_bin_summary <- summary(object = neg_bin <- glm.nb(
      formula = data$seizures_treatment ~ data$treatment + 
        data$seizures_baseline + offset(log(data$time_study))
      ))
    
    neg_bin_summary2 <- summary(object = neg_bin2 <- glm.nb(
      formula = data$seizures_treatment ~ data$treatment + 
        log(data$seizures_baseline) + offset(log(data$time_study))
    ))
    
    tibble(
      neg_bin_coefficient_treatment = exp(neg_bin$coefficients[2]),
      neg_bin_coefficient_seizures_baseline = exp(neg_bin$coefficients[3]),
      neg_bin_p_value_treatment = neg_bin_summary$coefficients[2, 4],
      neg_bin_p_value_seizures_baseline = neg_bin_summary$coefficients[3, 4],
      neg_bin_conf_IRR_low = NA,
      neg_bin_conf_IRR_up = NA,
      neg_bin_se_treatment = neg_bin_summary$coefficients[2,2],
      
      
      neg_bin_log_coefficient_treatment = exp(neg_bin2$coefficients[2]),
      neg_bin_log_coefficient_seizures_baseline = exp(neg_bin2$coefficients[3]),
      neg_bin_log_p_value_treatment = neg_bin_summary2$coefficients[2, 4],
      neg_bin_log_p_value_seizures_baseline = 
        neg_bin_summary2$coefficients[3, 4],
      neg_bin_log_conf_IRR_low = NA,
      neg_bin_log_conf_IRR_up = NA,
      neg_bin_log_se_treatment = neg_bin_summary2$coefficients[2,2]
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
    survtime <- Surv(time = data$time_baseline, 
                        event = data$censor, type = "right")
    cox_summary <- summary(object = cox <- coxph(
      formula =  survtime~ 
        data$treatment + data$seizures_baseline
    ))
    

     cox_summary2 <- summary(object = cox2 <- coxph(
      formula = survtime ~ data$treatment + log(data$seizures_baseline)
    ))
    
    # only treatment, surv nur einmal berecnen
    cox_summary3 <- summary(object = cox3 <- coxph(
      formula = survtime ~ data$treatment
    ))
    
    tibble(
      cox_p_value_treatment = cox_summary$coefficients[1, 5], 
      cox_p_value_seizures_baseline = cox_summary$coefficients[2,5], 
      cox_HR_treatment = cox_summary$coefficients[1,2],  #hazard ratio
      cox_HR_seizures_baseline = cox_summary$coefficients[2, 2],
      cox_conf_treatment_low = NA,
      cox_conf_treatment_up = NA,
      cox_se_treatment = cox_summary$coefficients[1,3],
      cox_wald = cox$wald.test,
      
      cox_p_value_log_treatment = cox_summary2$coefficients[1, 5], 
      cox_p_value_log_seizures_baseline = cox_summary2$coefficients[2,5], 
      cox_HR_log_treatment = cox_summary2$coefficients[1,2],       
      cox_HR_log_seizures_baseline = cox_summary2$coefficients[2, 2],
      cox_log_conf_treatment_low = NA,
      cox_log_conf_treatment_up = NA,
      cox_log_se_treatment = cox_summary2$coefficients[1,3],
      cox_log_wald = cox2$wald.test,
      
      cox3_p_value_treatment = cox_summary3$coefficients[1, 5],
      cox3_HR_treatment = cox_summary3$coefficients[1, 2],
      cox3_conf_treatment_low = NA,
      cox3_conf_treatment_up = NA,
      cox3_se_treatment = cox_summary3$coefficients[1,3],
      cox3_wald = cox3$wald.test
      
    )
    
    
  }
  
  
  # Logit und Chi^2
  logit_reg <- function(data) {
    logit_summary <- summary(object = logit <- glm(
      formula = data$response ~ data$treatment + data$seizures_baseline,
      family = binomial(link = "logit")
    ))
    
    logit_summary2 <- summary(object = logit2 <- glm(
      formula = data$response ~ data$treatment + log(data$seizures_baseline),
      family = binomial(link = "logit")
    ))
 
    tibble(
      
      logit_p_value_treatment = logit_summary$coefficients[2, 4],
      logit_p_value_seizures_baseline = logit_summary$coefficients[3, 4],
      oddsratio_treatment = exp(logit$coefficients[2]),
      oddsratio_seizures_baseline = exp(logit$coefficients[3]),
      conf_treatment_low = NA,
      conf_treatment_up = NA,
      logit_se_treatment = logit_summary$coefficients[2, 2],
      
      logit_p_value_log_treatment = logit_summary2$coefficients[2, 4],
      logit_p_value_log_seizures_baseline = logit_summary2$coefficients[3, 4],
      oddsratio_log_treatment = exp(logit2$coefficients[2]),
      oddsratio_log_seizures_baseline = exp(logit2$coefficients[3]),
      conf_log_treatment_low = NA,
      conf_log_treatment_up = NA,
      logit_log_se_treatment = logit_summary2$coefficients[2, 2]
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
  gen_dat <- function(n, delta = delta){
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
      
      mutate(
        
        # time_study
        time_study = round(pmin(rexp(n = length(subject),
                                     rate = -log(0.8) / 56), 56)),
        time_study = if_else(condition = time_study != 0, true = time_study, 
                             false = 1),
        
        # seizures_treatment
        seizures_treatment = sapply(X = exp(
          log(lambda_baseline * time_study / 28) - (treatment * delta + 0.2) 
        ), FUN = rpois, n = 1),
        
        # drop_out
        drop_out = if_else(condition = time_study == 56, true = 0, false = 1), 
        
        # time_baseline with individual lambda.
        lambda_time = if_else(condition = seizures_treatment != 0, 
                              true = seizures_treatment / time_study, 
                              false = 1e-10), 
        time_baseline = round(unlist(lapply(X = mapply(FUN = rexp, 
                                                       n = seizures_baseline, 
                                                       rate = time_study), 
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
        ) 
      )
  }
  
  data <- gen_dat(n = n, delta = delta)
  bind_cols(tibble(delta = delta), tibble(n = n), 
            negative_binomial(data = data), log_rank(data = data), 
            cox_reg(data = data), logit_reg(data = data), chisq(data = data))
}

#### Execution ####
# without parralel computation for testing
set.seed(42)
result <- bind_rows(lapply(X = 1:100, FUN = sampling, n = 200, delta = 0.13))
result

# parallel computing for faster computation
set.seed(seed = 42)
cl <- makeCluster(spec = detectCores())
clusterEvalQ(cl = cl, expr = lapply(X = c("MASS", "tidyverse", "survival"),
                                    FUN = require, character.only = TRUE))

system.time(results <- parLapply(cl = cl, X = 1:2000, fun = sampling, n=200, 
                                 delta = 1))
system.time(result_df <- do.call(what = "bind_rows", args = results))

#### Different Deltas ####
# simulate for different values of delta

resultlist <- list()
x <- seq(0,0.3,0.05)
system.time(
for(i in 1:length(x)){
  resultlist[[i]] <- parLapply(cl = cl, X = 1:500, fun = sampling, n = 100, 
                               delta = x[i])
}
)

results <- lapply( resultlist, bind_rows ) # each list element is one 
                                           # result dataframe

for (i in 1:length(x)){  #save each data.frame with an individual name
  nam <- results[[i]]
  
  assign(paste0("Testn100_delta",nam$delta[1],sep=""), nam,envir=globalenv())
  
}

rm(results, resultlist,nam,i,x)
save(list = ls(all.names = TRUE), file = "TestN_400.RData", envir = .GlobalEnv) 

load("N_200.RData")
setwd("C:/Users/Chris_2/Documents/Uni/Master/Stat Praktikum/Ergebnisse")
save(result_df, file= "n_200_delta_050.RData" )

rm(result_df, results)
resultdf <- load("delta_030.RData")
load("delta0.13_n.RData")
stopCluster(cl = cl)

rm(list=ls(pattern="n100"))

#### Different Ns ####
# simulate for different values of n

resultlist <- list()
x <- seq(from = 90, to = 900, length.out = 10)
system.time(
  for(i in 1:length(x)){
    resultlist[[i]] <- parLapply(cl = cl, X = 1:500, fun = sampling, n = x[i], 
                                 delta = 0.13)
  }
)

# a <- results[[3]]

results <- lapply(resultlist, bind_rows) # each list element is one result 
                                         # dataframe
for (i in 1:length(x)){
  nam <- results[[i]]
  
  if (nam$n[1] < 100) {
    assign(paste0("delta0.13_n0",nam$n[1],sep=""), nam,envir=globalenv())
  } else {
  assign( paste0("delta0.13_n",nam$n[1],sep=""), nam,envir=globalenv())
  } # dataframes are not in correct order, therefore use "n" column
}   # entry for naming.

rm(results, resultlist, nam, i)

save(list = ls(all.names = TRUE), file = "delta0.13_n_2.RData", 
     envir = .GlobalEnv)
