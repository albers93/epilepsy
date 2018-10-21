#### Epilepsy ####
# Packages
library(tidyverse)
library(gridExtra)

# Data
epilepsy <- read.csv(file = "epilepsy.csv", sep = ";")
epilepsy <- rename(
  .data = epilepsy, 
  seizures_baseline = number.of.seizures.at.baseline, 
  seizures_treatment = number.of.seizures.under.treatment, 
  time_study = time.in.study..days., 
  dropout = drop.out, 
  time_baseline = time.to.baseline.number
)
summary(object = epilepsy)

# Boxplots
## Subject
boxplot_subject <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(y = subject, colour = 1, fill = 2)) + 
  scale_colour_viridis_c() + 
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Subject")
## Treatment
boxplot_treatment <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(y = treatment, colour = 1, fill = 2)) + 
  scale_colour_viridis_c() + 
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Treatment")
## Number of seizures at baseline
boxplot_seizures_baseline <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(y = seizures_baseline, colour = 1, fill = 2)) + 
  scale_colour_viridis_c() + 
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Number of seizures at baseline")
## Number of seizures under treatment
boxplot_seizures_treatment <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(y = seizures_treatment, colour = 1, fill = 2)) + 
  scale_colour_viridis_c() + 
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Number of seizures under treatment")
## Time in study (days)
boxplot_time_study <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(y = time_study, colour = 1, fill = 2)) + 
  scale_colour_viridis_c() + 
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Time in study (days)")
## Drop-out
boxplot_dropout <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(y = dropout, colour = 1, fill = 2)) + 
  scale_colour_viridis_c() + 
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Drop-out")
## Time to baseline number
boxplot_time_baseline <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(y = time_baseline, colour = 1, fill = 2)) + 
  scale_colour_viridis_c() + 
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Time to baseline number")
## Censor
boxplot_censor <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(y = censor, colour = 1, fill = 2)) + 
  scale_colour_viridis_c() + 
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Censor")
## Execution
grid.arrange(boxplot_subject, boxplot_treatment, boxplot_seizures_baseline, 
             boxplot_seizures_treatment, boxplot_time_study, boxplot_dropout, 
             boxplot_time_baseline, boxplot_censor)

# Dummy variables as factors
epilepsy$treatment <- as.factor(epilepsy$treatment)
epilepsy$dropout <- as.factor(epilepsy$dropout)
epilepsy$censor <- as.factor(epilepsy$censor)
summary(object = epilepsy)

# Boxplots grouped by treatment
## Subject
boxplot_treatment_subject <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = treatment, y = subject, colour = 1, 
                             fill = treatment)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Subject") + 
  xlab(label = "Treatment")
## Number of seizures at baseline
boxplot_treatment_seizures_baseline <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = treatment, y = seizures_baseline, colour = 1, 
                             fill = treatment)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Number of seizures at baseline") + 
  xlab(label = "Treatment")
## Number of seizures under treatment
boxplot_treatment_seizures_treatment <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = treatment, y = seizures_treatment, colour = 1, 
                             fill = treatment)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Number of seizures under treatment") + 
  xlab(label = "Treatment")
## Time in study (days)
boxplot_treatment_time_study <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = treatment, y = time_study, colour = 1, 
                             fill = treatment)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Time in study (days)") + 
  xlab(label = "Treatment")
## Time to baseline number
boxplot_treatment_time_baseline <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = treatment, y = time_baseline, colour = 1, 
                             fill = treatment)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Time to baseline number") + 
  xlab(label = "Treatment")
## Execution
grid.arrange(boxplot_treatment_subject, boxplot_treatment_seizures_baseline, 
             boxplot_treatment_seizures_treatment, boxplot_treatment_time_study, 
             boxplot_treatment_time_baseline)

# Boxplots grouped by drop-out
## Subject
boxplot_dropout_subject <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = dropout, y = subject, colour = 1, 
                             fill = dropout)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Subject") + 
  xlab(label = "Drop-out")
## Number of seizures at baseline
boxplot_dropout_seizures_baseline <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = dropout, y = seizures_baseline, colour = 1, 
                             fill = dropout)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Number of seizures at baseline") + 
  xlab(label = "Drop-out")
## Number of seizures under treatment
boxplot_dropout_seizures_treatment <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = dropout, y = seizures_treatment, colour = 1, 
                             fill = dropout)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Number of seizures under treatment") + 
  xlab(label = "Drop-out")
## Time in study (days)
boxplot_dropout_time_study <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = dropout, y = time_study, colour = 1, 
                             fill = dropout)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Time in study (days)") + 
  xlab(label = "Drop-out")
## Time to baseline number
boxplot_dropout_time_baseline <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = dropout, y = time_baseline, colour = 1, 
                             fill = dropout)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Time to baseline number") + 
  xlab(label = "Drop-out")
## Execution
grid.arrange(boxplot_dropout_subject, boxplot_dropout_seizures_baseline, 
             boxplot_dropout_seizures_treatment, boxplot_dropout_time_study, 
             boxplot_dropout_time_baseline)

# Boxplots grouped by censor
## Subject
boxplot_censor_subject <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = censor, y = subject, colour = 1, 
                             fill = censor)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Subject") + 
  xlab(label = "Censor")
## Number of seizures at baseline
boxplot_censor_seizures_baseline <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = censor, y = seizures_baseline, colour = 1, 
                             fill = censor)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Number of seizures at baseline") + 
  xlab(label = "Censor")
## Number of seizures under treatment
boxplot_censor_seizures_treatment <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = censor, y = seizures_treatment, colour = 1, 
                             fill = censor)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Number of seizures under treatment") + 
  xlab(label = "Censor")
## Time in study (days)
boxplot_censor_time_study <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = censor, y = time_study, colour = 1, 
                             fill = censor)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Time in study (days)") + 
  xlab(label = "Censor")
## Time to baseline number
boxplot_censor_time_baseline <- ggplot(data = epilepsy) + 
  geom_boxplot(mapping = aes(x = censor, y = time_baseline, colour = 1, 
                             fill = censor)) + 
  scale_colour_viridis_d(aesthetics = "fill") + 
  scale_colour_viridis_c(aesthetics = "colour") +
  guides(colour = FALSE, fill = FALSE) + 
  ylab(label = "Time to baseline number") + 
  xlab(label = "Censor")
## Execution
grid.arrange(boxplot_censor_subject, boxplot_censor_seizures_baseline, 
             boxplot_censor_seizures_treatment, boxplot_censor_time_study, 
             boxplot_censor_time_baseline)

