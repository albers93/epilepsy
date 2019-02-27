#### Power ####
# Packages
library(tidyverse)
library(gridExtra)

# Data
load("Data/delta_000.RData")
delta_000 <- result_df
load("Data/delta_010.RData")
delta_010 <- result_df
load("../Stat Praktikum/delta_013.RData")
delta_013 <- result_df
load("../Stat Praktikum/delta_015.RData")
delta_015 <- result_df
load("../Stat Praktikum/delta_020.RData")
delta_020 <- result_df
load("../Stat Praktikum/delta_025.RData")
delta_025 <- result_df
load("../Stat Praktikum/delta_100.RData")
delta_100 <- result_df
rm(result_df)
load("Data/N_100.RData")
load("Data/N_200.RData")
load("Data/N_400.RData")

power.t.test(n = NULL, delta = 0.13, sig.level = 0.05, power = 0.8)

#### Power calculation ####
calculate_power <- function(data = Null) {
  
  # A list with all Data Frames of the global environment
  global <- ls(envir = .GlobalEnv)
  data_list <- lapply(
    X = global[sapply(
      X = global,
      FUN = function(object) {is.data.frame(get(object))})],
    FUN = get
  )
  
  # Only treatment p values
  p_treatment_list <- lapply(
    X = data_list, 
    FUN = select, 
    neg_bin_p_value_treatment, 
    neg_bin_log_p_value_treatment, 
    logrank_p_value, 
    cox_p_value_treatment, 
    cox_p_value_log_treatment, 
    cox3_p_value_treatment, 
    logit_p_value_treatment, 
    logit_p_value_log_treatment, 
    chi_square_p_value
  )
  
  # Data frame with power.
  # Each column one Dataset.
  # Each row one test
  power <- as_tibble(t(bind_cols(lapply(
    X = p_treatment_list, 
    FUN = function(data) {
      apply(
        X = data,
        MARGIN = 2,
        FUN = function(column) {
          nrow(filter(.data = data, column <= 0.05)) / nrow(data)
        }
      )
    }
  ))))
  rename(
    .data = power, 
    neg_bin_p_value_treatment = V1, 
    neg_bin_log_p_value_treatment = V2, 
    logrank_p_value = V3, 
    cox_p_value_treatment = V4, 
    cox_p_value_log_treatment = V5, 
    cox3_p_value_treatment = V6, 
    logit_p_value_treatment = V7, 
    logit_p_value_log_treatment = V8, 
    chi_square_p_value = V9
  )
}

#### Execution ####
power <- calculate_power(data = NULL)
# save(list = "power_100", file = "Data/power_100.RData", envir = .GlobalEnv)
power

#### Plots ####
x <- seq(from = 90, to = 900, length.out = 10)

# jpeg(filename = "plots/line_400.jpeg")
ggplot(data = power) + 
  geom_line(mapping = aes(x = x, 
                          y = neg_bin_p_value_treatment, 
                          color = "Negative Binomial")) + 
  geom_line(mapping = aes(x = x, 
                          y = logrank_p_value, color = "Log Rank")) + 
  geom_line(mapping = aes(x = x, 
                          y = cox_p_value_treatment, color = "Cox (full)")) + 
  geom_line(mapping = aes(x = x, 
                          y = cox3_p_value_treatment, 
                          color = "Cox (only treatment)")) + 
  geom_line(mapping = aes(x = x, 
                          y = logit_p_value_treatment, color = "Logit")) + 
  geom_line(mapping = aes(x = x, 
                          y = chi_square_p_value, color = "Chi Square")) + 
  geom_line(mapping = aes(x = x, y = 0.05, color = "0.05")) + 
  geom_line(mapping = aes(x = x, y = 0.8, color = "0.8")) + 
  geom_line(mapping = aes(x = x, y = 0.95, color = "0.95")) + 
  scale_color_viridis_d(name = "Method") + 
  xlab(label = "n") + 
  ylab(label = "Power") + 
  ggtitle(label = "delta = 0.13") + 
  theme_classic()
# dev.off()

epi_seizures <- ggplot(data = epilepsy) + 
  geom_density(mapping = aes(x = seizures_treatment, fill = 1)) + 
  guides(fill = FALSE) + 
  theme_classic()
dat_seizures <- ggplot(data = dataset[[11]]) + 
  geom_density(mapping = aes(x = seizures_treatment, fill = 1)) + 
  guides(fill = FALSE) +
  theme_classic()
epi_time <- ggplot(data = epilepsy) + 
  geom_density(mapping = aes(x = time_baseline, fill = 1)) + 
  guides(fill = FALSE) + 
  theme_classic()
dat_time <- ggplot(data = dataset[[11]]) + 
  geom_density(mapping = aes(x = time_baseline, fill = 1)) + 
  guides(fill = FALSE) + 
  theme_classic()
epi_point <- ggplot(data = epilepsy) + 
  geom_point(mapping = aes(x = time_study, y = seizures_treatment, color = 1)) + 
  guides(color = FALSE) + 
  theme_classic()
dat_point <- ggplot(data = dataset[[11]]) + 
  geom_point(mapping = aes(x = time_study, y = seizures_treatment, color = 1)) + 
  guides(color = FALSE) + 
  theme_classic()

# jpeg(filename = "plots/plot.jpg")
grid.arrange(epi_seizures, dat_seizures, epi_time, dat_time, epi_point, 
             dat_point)
# dev.off()

