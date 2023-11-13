# stats for figure 6 and supp. figure 7

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
mouse <- factor(data$mouse)
age <- data$age
firing <- data$f_rate
rec <- factor(data$rec)

# run a model for firing rate over age and brain area
model <- glmer(firing ~ age + (1 | mouse) + (1 | rec), family = Gamma(link = 'log'),
               control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0)
summary(model)

##################### LOAD DATA for STTC #####################
rm(list  =  ls())
data <- read.xlsx('load df_sttc.xlsx here')
data <- data[!is.na(data$sttc),]
mouse <- factor(data$mouse)
rec <- factor(data$rec)
age <- data$age
sttc <- data$sttc - min(data$sttc) + 0.0001

# run a model for sttc over age and brain area
model <- glmer(sttc ~ age + (1 | mouse) + (1 | rec), family = Gamma(link = 'log'),
               control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE), nAGQ = 0)
summary(model)

##################### LOAD Gini f_rate DATA #####################
rm(list  =  ls())
data <- read.xlsx('load df_gini.xlsx here')
age <- data$age
b_area <- factor(data$b_area)
mouse <- factor(data$mouse)
gini_f <- data$gini_f
gini_s <- data$gini_s
corr_f_s <- data$corr_f_s
kurt_f <- log10(data$kurt_f_rate)
kurt_s <- log10(data$kurt_sttc)
skew_f <- data$skew_f_rate
skew_s <- data$skew_sttc

#### fit model for skewness of firing rate in all brain areas and age
model <- lmer(skew_f ~ age + (1 | mouse)) # fit the model
summary(model)
confint(model)

#### fit model for skewness of sttc in all brain areas and age
model <- lmer(skew_s ~ age + (1 | mouse)) # fit the model
summary(model)
confint(model)

#### fit model for kurtosis of firing rate in all brain areas and age
model <- lmer(kurt_f ~ age + (1 | mouse)) # fit the model
summary(model)
confint(model)

#### fit model for kurtosis of sttc in all brain areas and age
model <- lmer(kurt_s ~ age + (1 | mouse)) # fit the model
summary(model)
confint(model)

#### fit model for g coeff of firing rate in all brain areas and age
model <- lmer(gini_f ~ age + (1 | mouse)) # fit the model
summary(model)
confint(model)

#### fit model for g coeff of sttc in all brain areas and age
model <- lmer(gini_s ~ age + (1 | mouse)) # fit the model
summary(model)
confint(model)

#### fit model for corr of firing and sttc in all brain areas and age
model <- lmer(corr_f_s ~ age + (1 | mouse)) # fit the model
summary(model)
confint(model)

##################### LOAD DATA for firing rate #####################
rm(list  =  ls())
data <- read.xlsx('load df_firing.xlsx here')
data <- data[!is.na(data$meanSTTC), ]
mouse <- factor(data$mouse)
age <- data$age
firing <- log10(data$f_rate)
avgSTTC <- log10(data$meanSTTC - min(data$meanSTTC) + 0.001)

# run a model for firing rate over age and brain area
model <- lmer(firing ~ avgSTTC + (1 | mouse))
summary(model)
confint(model)

