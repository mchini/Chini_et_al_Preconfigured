# stats for figure 3 and supp. figure 5

Sys.setenv(LANG = "en")
rm(list  =  ls())

library('emmeans')
library('openxlsx')
library('sjPlot')
library('lme4')
library('lmerTest')
library('ggplot2')

#################### LOAD DATA for neuropixels probes #####################
data <- read.xlsx('load graph_stuff.xlsx here')
age <- data$age
density <- data$density
clust_coeff <- data$clust_coeff
char_path <- data$char_path
small_w <- data$small_w
small_w_alt <- data$small_w_alt
transit <- data$transit
b_area <- data$b_area

# run a model for clust_coeff over age
model <- lm(density ~ age)
summary(model)
confint(model)

# run a model for char_path over age
model <- lm(char_path ~ age)
summary(model)
confint(model)

# run a model for clust_coeff over age
model <- lm(clust_coeff ~ age)
summary(model)
confint(model)

# run a model for small_w over age
model <- lm(small_w ~ age)
summary(model)
confint(model)

# run a model for transit over age
model <- lm(transit ~ age)
summary(model)
confint(model)




