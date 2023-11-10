# stats for figure 2 and supp. figure 2

Sys.setenv(LANG = "en")
rm(list  =  ls())

library('openxlsx')
library('sjPlot')
library('ggplot2')

##################### LOAD skewness, kurtosis and Gini DATA #####################
data <- read.xlsx('load df_sua_stats_shape here')
data <- data[!is.na(data$gini_f), ]
data <- data[data$num_units > 19, ]
age <- data$age
b_area <- factor(data$b_area)
mouse <- factor(data$mouse)
skew_f <- data$skew_f_rate
skew_s <- data$skew_sttc
gini_f <- data$gini_f
gini_s <- data$gini_s
kurt_f <- log10(data$kurt_f_rate)
kurt_s <- log10(data$kurt_sttc)
levels(b_area) <- c('OB', 'PFC')


#### fit model for the skewness of firing in all brain areas and age
model <- lm(skew_f ~ b_area * age) # fit the model
summary(model)
confint(model)
# plot across brain areas
graph <- plot_model(model, type='pred', terms=c('age', 'b_area'))
graph + ylim(-.5, 6)


#### fit model for the kurtosis of firing in all brain areas and age
model <- lm(kurt_f ~ b_area * age) # fit the model
summary(model)
confint(model)
# plot across brain areas
graph <- plot_model(model, type='pred', terms=c('age', 'b_area'))
graph + ylim(0, 2)

#### fit model for the skewness of sttc in all brain areas and age
model <- lm(skew_s ~ b_area * age) # fit the model
summary(model)
confint(model)
# plot across brain areas
graph <- plot_model(model, type='pred', terms=c('age', 'b_area'))
graph + ylim(-.5, 15)

#### fit model for the kurtosis of sttc in all brain areas and age
model <- lm(kurt_s ~ b_area * age) # fit the model
summary(model)
confint(model)
# plot across brain areas
graph <- plot_model(model, type='pred', terms=c('age', 'b_area'))
graph + ylim(0, 3)

#### fit model for g coeff of firing rate in all brain areas and age
model <- lm(gini_f ~ b_area * age) # fit the model
summary(model)
confint(model)
# plot across brain areas
graph <- plot_model(model, type='pred', terms=c('age', 'b_area'))
graph + ylim(.2, .9)

#### fit model for g coeff of sttc in all brain areas and age
model <- lm(gini_s ~ b_area * age) # fit the model
summary(model)
confint(model)
# plot across brain areas
graph <- plot_model(model, type='pred', terms=c('age', 'b_area'))
graph + ylim(0, .7)



# re-load the data and compute pvalues for the heatmap of supp. figure 2D
data <- read.xlsx('load df_sua_stats_shape here')
data <- data[!is.na(data$gini_f), ]

pvals <- matrix(0, 21, 6)

idx = 1
for(min_units in 9:29)
{
  data <- data[data$num_units > min_units, ]
  age <- data$age
  b_area <- factor(data$b_area)
  mouse <- factor(data$mouse)
  skew_f <- data$skew_f_rate
  skew_s <- data$skew_sttc
  gini_f <- data$gini_f
  gini_s <- data$gini_s
  kurt_f <- log10(data$kurt_f_rate)
  kurt_s <- log10(data$kurt_sttc)
  levels(b_area) <- c('OB', 'PFC', 'PFC')
  
  model <- lm(skew_f ~ b_area * age) # fit the model
  pvals[idx, 1] = summary(model)$coefficients[3, 4]
  
  model <- lm(kurt_f ~ b_area * age) # fit the model
  pvals[idx, 2] = summary(model)$coefficients[3, 4]
  
  model <- lm(skew_s ~ b_area * age) # fit the model
  pvals[idx, 3] = summary(model)$coefficients[3, 4]
  
  model <- lm(kurt_s ~ b_area * age) # fit the model
  pvals[idx, 4] = summary(model)$coefficients[3, 4]
  
  model <- lm(gini_f ~ b_area * age) # fit the model
  pvals[idx, 5] = summary(model)$coefficients[3, 4]
  
  model <- lm(gini_s ~ b_area * age) # fit the model
  pvals[idx, 6] = summary(model)$coefficients[3, 4]
  
  idx = idx + 1
}
