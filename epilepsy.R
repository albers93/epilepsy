#### Epilepsy ####
# Packages
library(MASS)
library(tidyverse)
library(GGally)
library(lmtest)

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
    ), labels = c("No", "Yes")), 
    seizures_baseline_log = log(seizures_baseline), 
    time_study_log = log(time_study)
  )
summary(object = epilepsy)

# Visualization
ggpairs(
  data = epilepsy, 
  mapping = aes(colour = 1), title = "Original data",
  upper = list(continuous = "points", combo = "box", discrete = "facetbar"),
  lower = list(continuous = "points", combo = "box", discrete = "facetbar")
)

#### Binary Regression ####
# Visualization
ggpairs(data = epilepsy, 
        columns = c("response", "treatment", "seizures_baseline"), 
        mapping = aes(colour = 1), title = "Binary Regression",
        upper = list(continuous = "points",
                     combo = "box",
                     discrete = "facetbar"),
        lower = list(continuous = "points",
                     combo = "box",
                     discrete = "facetbar"))

# Chi-Quadrat Tests
table(epilepsy$response, epilepsy$treatment, dnn = c("Response", "Treatment"))
chisq.test(x = epilepsy$response, y = epilepsy$treatment)
## P-value > 0.05. -> No significant correlation.

## Odds-ratio and confidence interval
oddsratio(x = epilepsy$response, y = epilepsy$treatment)

# Logistische Regression
logit <- glm(formula = response ~ treatment + seizures_baseline, 
             family = binomial, data = epilepsy)
summary(object = logit)
## Only the Intercept is significant.

## Number of seizures at baseline is logarithmized
logit_log <- glm(formula = response ~ treatment + seizures_baseline_log, 
                 family = binomial, data = epilepsy)
summary(object = logit_log)
### Only the Intercept is significant.

## AIC
AIC(logit, logit_log)
AIC(logit_log) < AIC(logit)
### logit_log is the better model

lrtest(logit, logit_log)
### log transformation is significant

ggplot(data = epilepsy) + 
  geom_point(mapping = aes(x = seizures_baseline_log, 
                           y = as.numeric(response) - 1, colour = 1)) + 
  geom_smooth(
    formula = epilepsy$response ~ epilepsy$treatment + 
      epilepsy$seizures_baseline, 
    mapping = aes(
      x = seizures_baseline_log[order(seizures_baseline_log)], 
      y = as.vector(logit_log$fitted.values[order(logit_log$fitted.values)]), 
      colour = 2)
  ) + 
  scale_colour_viridis_c(guide = FALSE) + 
  xlab(label = "Logarithmic number of seizures at baseline") + 
  ylab(label = "Response")

ggplot(data = epilepsy) +
  geom_point(mapping = aes(x = as.numeric(treatment) - 1,
                           y = as.numeric(response) - 1, colour = 1)) +
  geom_line(mapping = aes(
    x = (as.numeric(epilepsy$treatment) - 1)[order(
      (as.numeric(epilepsy$treatment) - 1)
    )],
    y = as.vector(logit_log$fitted.values[order(logit_log$fitted.values)]),
    colour = 2)
  ) +
  scale_colour_viridis_c(guide = FALSE) +
  xlab(label = "Treatment") +
  ylab(label = "Response")

## Confidence interval
confint(object = logit_log)
### Only the intercept significantly different from zero

## Plot of the Fitted Values
ggplot(data = epilepsy) + 
  geom_point(mapping = aes(x = seizures_baseline_log, 
                           y = as.numeric(response) - 1, colour = 1)) + 
  geom_line(mapping = aes(
    x = seizures_baseline_log[order(seizures_baseline_log)], 
    y = as.vector(logit_log$fitted.values[order(logit_log$fitted.values)]), 
    colour = 2)
  ) + 
  scale_colour_viridis_c(guide = FALSE) + 
  xlab(label = "Logarithmic number of seizures at baseline") + 
  ylab(label = "Response")

# ggplot(data = epilepsy) + 
#   geom_point(mapping = aes(x = as.numeric(treatment) - 1, 
#                            y = as.numeric(response) - 1, colour = 1)) + 
#   geom_line(mapping = aes(
#     x = (as.numeric(epilepsy$treatment) - 1)[order(
#       (as.numeric(epilepsy$treatment) - 1)
#     )], 
#     y = as.vector(logit_log$fitted.values[order(logit_log$fitted.values)]), 
#     colour = 2)
#   ) + 
#   scale_colour_viridis_c(guide = FALSE) + 
#   xlab(label = "Treatment") + 
#   ylab(label = "Response")

ggplot(data = epilepsy) + 
  geom_density(mapping = aes(x = seizures_baseline, colour = "Kernel density", 
                             fill = 1)) + 
  geom_line(mapping = aes(
    x = seizures_treatment,
    y = dnbinom(seizures_treatment, size = 3.77,
                mu = mean(epilepsy$seizures_treatment)),
    colour = "Theoretical density"
  )) +
  scale_colour_viridis_d(direction = -1, name = "Legend") + 
  guides(fill = FALSE) +
  xlab(label = "Number of seizures under treatment") + 
  ylab(label = "Density")

shapiro.test(residuals.glm(logit))
shapiro.test(residuals(logit))
shapiro.test(logit$residuals)
ggplot(data = epilepsy) + 
  geom_point(mapping = aes(x = 1:length(subject), y = logit$residuals, 
                           colour = 0)) + 
  geom_hline(mapping = aes(yintercept = 0, colour = 1)) +
  scale_colour_viridis_c(guide = FALSE) + 
  xlab(label = "Index") + 
  ylab(label = "Residuen")

#### Count data regression ####
# Distribution of number of seizures under treatment and maximum likelihood 
# estimates for size and mu
summary(object = select(.data = epilepsy, seizures_treatment, treatment, 
                        seizures_baseline, time_study, seizures_baseline_log, 
                        time_study_log))
ggpairs(data = select(.data = epilepsy, seizures_treatment, treatment, 
                      seizures_baseline, time_study, seizures_baseline_log, 
                      time_study_log), mapping = aes(colour = 1),
        upper = list(continuous = wrap(funcVal = "points"), 
                     combo = wrap(funcVal = "box"), 
                     discrete = wrap(funcVal = "facetbar")), 
        lower = list(continuous = wrap(funcVal = "points"), 
                     combo = wrap(funcVal = "box"), 
                     discrete = wrap(funcVal = "facetbar")),
        diag = list(continuous = wrap(funcVal = "densityDiag"), 
                    discrete = wrap(funcVal = "barDiag")))
ggplot(data = epilepsy) + 
  geom_density(mapping = aes(x = seizures_treatment, 
                             colour = "Kerndichteschätzer", fill = 1)) + 
  geom_line(mapping = aes(
    x = seizures_treatment, 
    y = dnbinom(seizures_treatment, size = 3.77, 
                mu = mean(epilepsy$seizures_treatment)),
    colour = "Negativ-binomial-Verteilung"
  )) + 
  scale_colour_viridis_d(direction = -1) + 
  guides(fill = FALSE, colour = FALSE) +
  xlab(label = "Anzahl der Anfälle während der Behandlung") + 
  ylab(label = "Dichte")

# Regression with negative binomial distribution and log-link
count <- glm.nb(
  formula = seizures_treatment ~ treatment + seizures_baseline +
    offset(object = time_study_log),
  data = epilepsy 
)
summary(object = count)
## All parameters are significant at 0.05 signifcance level.

## Log
count_log <- glm.nb(
  formula = seizures_treatment ~ treatment + seizures_baseline_log +
    offset(object = time_study_log),
  data = epilepsy
)
summary(object = count_log)
### All parameters are significant at 0.05 signifcance level.
### Treatment has a negative effect on the number of seizures

## AIC
AIC(count, count_log)
AIC(count_log) < AIC(count)
### count_log is the better model

lrtest(count, count_log)
# significant difference

anova(count, count_log)

## Confidence Interval
confint(count_log)
### Everything significantly different from zero.

ggplot(data = epilepsy) + 
  geom_point(mapping = aes(x = seizures_baseline_log, 
                           y = seizures_treatment, colour = 1)) + 
  geom_line(mapping = aes(
    x = seizures_baseline_log[order(seizures_baseline_log)], 
    y = as.vector(count_log$fitted.values[order(count_log$fitted.values)]), 
    colour = 2)
  ) + 
  scale_colour_viridis_c(guide = FALSE) + 
  xlab(label = "Logarithmic number of seizures at baseline") + 
  ylab(label = "Number of seizures under treatment")

count$residuals
shapiro.test(count$residuals)
shapiro.test(residuals(count))
shapiro.test(residuals.glm(count_log))
shapiro.test(residuals.)
shapiro.test(residuals.lm(count))
count$coefficients
ggplot(data = epilepsy) + 
  geom_point(mapping = aes(x = 1:length(subject), 
                           y = residuals.glm(count_log), 
                           colour = 0)) + 
  geom_hline(mapping = aes(yintercept = 0, colour = 1)) +
  scale_colour_viridis_c(guide = FALSE) + 
  xlab(label = "Index") + 
  ylab(label = "Residuen")

#### Simulation ####
lambda <- rgamma(n = 10000, shape = 1, rate = 1)
poisson <- c()
for (i in 1:10000) {
  poisson[i] <- rpois(n = 1, lambda = lambda[i])  
}
poisson
ggplot() + 
  geom_density(mapping = aes(x = poisson, fill = 1)) + 
  scale_colour_viridis_d() + 
  guides(fill = FALSE)

ggplot() + 
  geom_histogram(mapping = aes(x = poisson, fill = 1))

ggplot() + 
  geom_histogram(mapping = aes(x = poisson, y = ..density.., fill = 1), 
                 bins = 10)
