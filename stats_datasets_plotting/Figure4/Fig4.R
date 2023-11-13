# stats for figure 4

Sys.setenv(LANG = "en")
rm(list  =  ls())

library('emmeans')
library('openxlsx')
library('sjPlot')
library('lme4')
library('lmerTest')
library('ggplot2')

##################### LOAD DATA for STTC and firing rate #####################
data <- read.xlsx('load df_firing.xlsx here')
data <- data[!is.na(data$medSTTC) > 0,]
mouse <- factor(data$mouse)
age <- data$age
firing <- log10(data$f_rate)
avgSTTC <- log10(data$medSTTC - min(data$medSTTC) + 0.001)
brain_area <- factor(data$b_area)
levels(brain_area) <- c('OB', 'PFC')

# run a model for firing rate over age and brain area
model <- lmer(avgSTTC ~ age + firing*brain_area + (1 | mouse))
summary(model)
summ_model <- summary(model)
summ_model$coefficients
confint(model)

plot_model(model, type='pred', terms=c('firing', 'brain_area'))

##################### LOAD DATA for hubness and firing rate #####################
rm(list  =  ls())
data <- read.xlsx('load hub_f_rate.xlsx here')
age <- data$age
mouse <- factor(data$mouse)
f_rate <- data$f_rate
hub_score <- factor(data$hub_score)

# run a model for hubness score without age (let the random effect take care of it)
model <- lmer(f_rate ~ hub_score + (1 | mouse) + (1 | age))
anova(model) # can't use "summary" in a straightforward manner here, hence the use of anova
anova(model)$'Pr(>F)'
emmeans(model, pairwise ~ hub_score)
