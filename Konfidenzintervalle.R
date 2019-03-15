# Statistisches Praktikum

# Konfidenzintervalle


library(gridExtra)
library(ggplot2)

load("Ergebnisse/N_delta_0.30.RData")   #load any simulation dataframe and save as "data"
data <- delta_0.1_n_0750

# Funktion, die als Input einen simulierten Datensatz nimmt um dann KI's zu machen
conf.int <- function(data){   
  data$neg_bin_conf_IRR_low <- exp(log(data$neg_bin_coefficient_treatment) - 1.96*data$neg_bin_se_treatment)
  data$neg_bin_conf_IRR_up <- exp(log(data$neg_bin_coefficient_treatment) + 1.96*data$neg_bin_se_treatment)
  
  #data$neg_bin_log_conf_IRR_low <- exp(log(data$neg_bin_log_coefficient_treatment) - 1.96*data$neg_bin_log_se_treatment)
  #data$neg_bin_log_conf_IRR_up <- exp(log(data$neg_bin_log_coefficient_treatment) + 1.96*data$neg_bin_log_se_treatment)
  
  
  data$cox_conf_treatment_low <- exp(log(data$cox_HR_treatment) - 1.96 * data$cox_se_treatment)
  data$cox_conf_treatment_up <- exp(log(data$cox_HR_treatment) + 1.96 * data$cox_se_treatment)
  
  #data$cox_log_conf_treatment_low <- exp(log(data$cox_HR_log_treatment) - 1.96 * data$cox_log_se_treatment)
  #data$cox_log_conf_treatment_up <- exp(log(data$cox_HR_log_treatment) + 1.96 * data$cox_log_se_treatment)
  
  #data$cox3_conf_treatment_low <- exp( log( data$cox3_HR_treatment)) - 1.96 * data$cox3_se_treatment
  #data$cox3_conf_treatment_up <- exp( log( data$cox3_HR_treatment)) + 1.96 * data$cox3_se_treatment
  
  
  data$conf_treatment_low <- exp( log( data$oddsratio_treatment)) - 1.96 * data$logit_se_treatment
  data$conf_treatment_up <- exp( log( data$oddsratio_treatment)) + 1.96 * data$logit_se_treatment
  
  #data$conf_log_treatment_low <- exp( log( data$oddsratio_log_treatment)) - 1.96 * data$logit_log_se_treatment
  #data$conf_log_treatment_up <- exp( log( data$oddsratio_log_treatment)) + 1.96 * data$logit_log_se_treatment
  
  # change column names to make it more consistent
  colnames(data)[which(colnames(data)=="conf_treatment_low")] <- "logit_conf_oddsratio_low"
  colnames(data)[which(colnames(data)=="conf_treatment_up")] <- "logit_conf_oddsratio_up"
  #colnames(data)[which(colnames(data)=="conf_log_treatment_low")] <- "logit_log_conf_oddsratio_low"
  #colnames(data)[which(colnames(data)=="conf_log_treatment_up")] <- "logit_log_conf_oddsratio_up"
  
  return(data)
  
}


#jetzt einfach auf Simulationsdatensatz anwenden und die leeren Spalten werden mit KI's gefüllt
data <- conf.int(delta_0.2_n_0200)






# Koeffizientenbetrachtung

#   Histogramme fuer die Koeffizienten


# negative binomial 
breaks <- c(round(quantile(delta_0.2_n_0200$neg_bin_coefficient_treatment, 0.025),2),round(mean(delta_0.2_n_0200$neg_bin_coefficient_treatment),2),
            round(quantile(delta_0.2_n_0200$neg_bin_coefficient_treatment, 0.975),2),1)
P1 <- ggplot(data=delta_0.2_n_0200, aes(delta_0.2_n_0200$neg_bin_coefficient_treatment)) + 
  geom_histogram(col="yellow", 
                 fill="blue", 
                 alpha = .2)  +
  theme(text = element_text(size=18)) +
  labs(x="IRR", y="Frequency", title="Negative Binomial Model")+
  # annotate("text", label = "n = 200", x = 0.96, y = 500, color = "black", size=6, alpha=0.7) +
  #  annotate("text", label = "delta = 0.2", x = 0.96, y = 440, color = "black", size=6, alpha=0.7)+
  geom_vline(xintercept = mean(delta_0.2_n_0200$neg_bin_coefficient_treatment), linetype="dashed", size=0.5, color="red")+
  geom_vline(xintercept = quantile(delta_0.2_n_0200$neg_bin_coefficient_treatment, 0.025), linetype="dashed", size=0.5)+
  geom_vline(xintercept = quantile(delta_0.2_n_0200$neg_bin_coefficient_treatment, 0.975), linetype="dashed", size=0.5)+
  scale_x_continuous(name="IRR", breaks=breaks,labels=breaks)

ggsave(filename = "n_200_delta_0.2_negbin.png", path = "Ergebnisse", device="png", dpi = "retina", width = 20, height = 12, units="cm")

# Cox 

breaks <- c(round(quantile(delta_0.2_n_0200$cox_HR_treatment, 0.025),2),round(mean(delta_0.2_n_0200$cox_HR_treatment),2),
            round(quantile(delta_0.2_n_0200$cox_HR_treatment, 0.975),2),1)
P2 <- ggplot(data=delta_0.1_n_0750, aes(delta_0.2_n_0200$cox_HR_treatment)) + 
  geom_histogram(col="yellow", 
                 fill="blue", 
                 alpha = .2)  +
  theme(text = element_text(size=18)) +
  labs(x="HR", y="Frequency", title="Cox Model")+
  # annotate("text", label = "n = 200", x = 0.96, y = 500, color = "black", size=6, alpha=0.7) +
  #  annotate("text", label = "delta = 0.2", x = 0.96, y = 440, color = "black", size=6, alpha=0.7)+
  geom_vline(xintercept = mean(delta_0.2_n_0200$cox_HR_treatment), linetype="dashed", size=0.5, color="red")+
  geom_vline(xintercept = quantile(delta_0.2_n_0200$cox_HR_treatment, 0.025), linetype="dashed", size=0.5)+
  geom_vline(xintercept = quantile(delta_0.2_n_0200$cox_HR_treatment, 0.975), linetype="dashed", size=0.5)+
  scale_x_continuous(name="HR", breaks=breaks,labels=breaks)

ggsave(filename = "n_200_delta_0.2_cox.png", path = "Ergebnisse", device="png", dpi = "retina", width = 20, height = 12, units="cm")


# Logit 

breaks <- c(round(quantile(delta_0.2_n_0200$oddsratio_treatment, 0.025),2),round(mean(delta_0.2_n_0200$oddsratio_treatment),2),
            round(quantile(delta_0.2_n_0200$oddsratio_treatment, 0.975),2))

P3 <- ggplot(data=delta_0.2_n_0200, aes(delta_0.2_n_0200$oddsratio_treatment)) + 
  geom_histogram(col="yellow", 
                 fill="blue", 
                 alpha = .2)  +
  theme(text = element_text(size=18)) +
  labs(x="Odds Ratio", y="Frequency", title="Logit Model")+
  # annotate("text", label = "n = 200", x = 9, y = 1000, color = "black", size=6, alpha=0.7) +
  #  annotate("text", label = "delta = 0.2", x = 9, y = 820, color = "black", size=6, alpha=0.7)+
  
  geom_vline(xintercept = mean(delta_0.2_n_0200$oddsratio_treatment), linetype="dashed", size=0.5, color="red")+
  geom_vline(xintercept = quantile(delta_0.2_n_0200$oddsratio_treatment, 0.025), linetype="dashed", size=0.5)+
  geom_vline(xintercept = quantile(delta_0.2_n_0200$oddsratio_treatment, 0.975), linetype="dashed", size=0.5)+
  
  scale_x_continuous(name="Odds Ratio", breaks=breaks,labels=breaks)

ggsave(filename = "n_200_delta_0.2_logit.png", path = "Ergebnisse", device="png", dpi = "retina", width = 20, height = 12, units="cm")






# N = 750 und delta = 0.1

# negative binomial 
breaks <- c(round(quantile(delta_0.1_n_0750$neg_bin_coefficient_treatment, 0.025),2),round(mean(delta_0.1_n_0750$neg_bin_coefficient_treatment),2),
            round(quantile(delta_0.1_n_0750$neg_bin_coefficient_treatment, 0.975),2),1)
P4 <- ggplot(data=delta_0.1_n_0750, aes(delta_0.1_n_0750$neg_bin_coefficient_treatment)) + 
  geom_histogram(col="yellow", 
                 fill="blue", 
                 alpha = .2)  +
  theme(text = element_text(size=18)) +
  labs(x="IRR", y="Frequency", title="Negative Binomial Model")+
  # annotate("text", label = "n = 400", x = 0.96, y = 500, color = "black", size=6, alpha=0.7) +
  #  annotate("text", label = "delta = 0.15", x = 0.96, y = 440, color = "black", size=6, alpha=0.7)+
  geom_vline(xintercept = mean(delta_0.1_n_0750$neg_bin_coefficient_treatment), linetype="dashed", size=0.5, color="red")+
  geom_vline(xintercept = quantile(delta_0.1_n_0750$neg_bin_coefficient_treatment, 0.025), linetype="dashed", size=0.5)+
  geom_vline(xintercept = quantile(delta_0.1_n_0750$neg_bin_coefficient_treatment, 0.975), linetype="dashed", size=0.5)+
  scale_x_continuous(name="IRR", breaks=breaks,labels=breaks)

ggsave(filename = "n_750_delta_0.1_negbin.png", path = "Ergebnisse", device="png", dpi = "retina", width = 20, height = 12, units="cm")

# Cox 

breaks <- c(round(quantile(delta_0.1_n_0750$cox_HR_treatment, 0.025),2),round(mean(delta_0.1_n_0750$cox_HR_treatment),2),
            round(quantile(delta_0.1_n_0750$cox_HR_treatment, 0.975),2),1)
P5 <- ggplot(data=delta_0.1_n_0750, aes(delta_0.1_n_0750$cox_HR_treatment)) + 
  geom_histogram(col="yellow", 
                 fill="blue", 
                 alpha = .2)  +
  theme(text = element_text(size=18)) +
  labs(x="HR", y="Frequency", title="Cox Model")+
  # annotate("text", label = "n = 400", x = 0.96, y = 500, color = "black", size=6, alpha=0.7) +
  #  annotate("text", label = "delta = 0.15", x = 0.96, y = 440, color = "black", size=6, alpha=0.7)+
  geom_vline(xintercept = mean(delta_0.1_n_0750$cox_HR_treatment), linetype="dashed", size=0.5, color="red")+
  geom_vline(xintercept = quantile(delta_0.1_n_0750$cox_HR_treatment, 0.025), linetype="dashed", size=0.5)+
  geom_vline(xintercept = quantile(delta_0.1_n_0750$cox_HR_treatment, 0.975), linetype="dashed", size=0.5)+
  scale_x_continuous(name="HR", breaks=breaks,labels=breaks)

ggsave(filename = "n_750_delta_0.1_cox.png", path = "Ergebnisse", device="png", dpi = "retina", width = 20, height = 12, units="cm")


# Logit 

breaks <- c(round(quantile(delta_0.1_n_0750$oddsratio_treatment, 0.025),2),round(mean(delta_0.1_n_0750$oddsratio_treatment),2),
            round(quantile(delta_0.1_n_0750$oddsratio_treatment, 0.975),2),4,5)

P6 <- ggplot(data=delta_0.1_n_0750, aes(delta_0.1_n_0750$oddsratio_treatment)) + 
  geom_histogram(col="yellow", 
                 fill="blue", 
                 alpha = .2)  +
  theme(text = element_text(size=18)) +
  labs(x="Odds Ratio", y="Frequency", title="Logit Model")+
  # annotate("text", label = "n = 400", x = 4.5, y = 750, color = "black", size=6, alpha=0.7) +
  #  annotate("text", label = "delta = 0.15", x = 4.5, y = 630, color = "black", size=6, alpha=0.7)+
  
  geom_vline(xintercept = mean(delta_0.1_n_0750$oddsratio_treatment), linetype="dashed", size=0.5, color="red")+
  geom_vline(xintercept = quantile(delta_0.1_n_0750$oddsratio_treatment, 0.025), linetype="dashed", size=0.5)+
  geom_vline(xintercept = quantile(delta_0.1_n_0750$oddsratio_treatment, 0.975), linetype="dashed", size=0.5)+
  
  scale_x_continuous(name="Odds Ratio", breaks=breaks,labels=breaks)


ggsave(filename = "n_750_delta_0.1_logit.png", path = "Ergebnisse", device="png", dpi = "retina", width = 20, height = 12, units="cm")

grid.arrange(P1,P4,P2,P5,P3,P6, ncol=2) 