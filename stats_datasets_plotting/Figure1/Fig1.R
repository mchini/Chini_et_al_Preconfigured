# stats for Figure 1

Sys.setenv(LANG = "en")
rm(list  =  ls())

library('emmeans')
library('openxlsx')
library('sjPlot')
library('lme4')
library('lmerTest')
library('ggplot2')

##################### LOAD DATA for firing rate #####################
data <- read.xlsx('load df_firing.xlsx here')
data <- data[data$f_rate > 0,]
mouse <- factor(data$mouse)
age <- data$age
firing <- data$f_rate
brain_area <- factor(data$b_area)
levels(brain_area) <- c('OB', 'PFC')

# run a model for firing rate over age and brain area
model <- glmer(firing ~ age * brain_area + (1 | mouse), family = Gamma(link = 'log'),
               control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0)
summary(model)
emmeans(model, pairwise ~ brain_area | age, at = list(age=8))

graph <- plot_model(model, type='int', terms=c('age', 'brain_area'))
graph + ylim(0, 1.25)


##################### LOAD DATA for STTC #####################
data <- read.xlsx('load df_sttc.xlsx here')
data <- data[!is.na(data$sttc),]
mouse <- factor(data$mouse)
age <- data$age
sttc <- data$sttc - min(data$sttc) + 0.0001
brain_area <- factor(data$b_area)
levels(brain_area) <- c('OB', 'PFC')

# run a model for sttc over age and brain area
model <- glmer(sttc ~ age * brain_area + (1 | mouse), family = Gamma(link = 'log'),
               control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0)
summary(model)
emmeans(model, pairwise ~ brain_area | age, at = list(age=8))

plot_model(model, type='int', terms=c('age', 'brain_area'))
save_plot(paste(folder4plot, 'sttc_age_gamma.svg'))
