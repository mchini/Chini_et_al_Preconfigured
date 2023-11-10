# stats for figure 5 and supp. figure 6
Sys.setenv(LANG = "en")
rm(list  =  ls())

library('emmeans')
library('openxlsx')
library('sjPlot')
library('lme4')
library('lmerTest')
library('ggplot2')


##################### LOAD simulated DATA with random parameters #####################
data <- read.xlsx('load df_NN_syms.xlsx here')
distance <- data$dist7
skewed_params <- factor(data$extreme_params)
ampa <- scale(data$AMPA_mod)
gaba <- scale(data$GABA_mod)
connectivity <- scale(data$connectivity)
in_sigma <- scale(data$INs_sigma)
pyr_sigma <- scale(data$PYRs_sigma)
log_syn_w <- factor(data$log_syn_w)
log_syn_n <- factor(data$log_syn_n)
prop_syn <- factor(data$prop_syn_n)
skew_f <- scale(data$skew_f)
skew_s <- scale(data$skew_s)
kurt_f <- scale(data$kurt_f)
kurt_s <- scale(data$kurt_s)
gini_f <- scale(data$gini_f)
gini_s <- scale(data$gini_s)
corr_f_s <- scale(data$f_s_corr)

#### fit model for distance with all the parameters and individual extrem params
model <- lm(distance ~ log_syn_n + log_syn_w + prop_syn + ampa + gaba +
              connectivity + in_sigma + pyr_sigma) # fit the model
plot_model(model, sort.est = TRUE, rm.terms = c('gaba', 'ampa', 'in_sigma',
                                                'connectivity', 'pyr_sigma'))

##################### LOAD simulated DATA with random parameters #####################
rm(list  =  ls())
data <- read.xlsx('load df_NN_syms_ind_dim_dist.xlsx here')
log_syn_n <- factor(data$log_syn_n)
log_syn_w <- factor(data$log_syn_w)
prop_syn <- factor(data$prop_syn_n)
ampa <- scale(data$AMPA_mod)
gaba <- scale(data$GABA_mod)
connectivity <- scale(data$connectivity)
in_sigma <- scale(data$INs_sigma)
input_rate <- scale(data$input_factor)
pyr_sigma <- scale(data$PYRs_sigma)
skew_f <- scale(data$skew_f)
skew_s <- scale(data$skew_s)
kurt_f <- scale(data$kurt_f)
kurt_s <- scale(data$kurt_s)
gini_f <- scale(data$gini_f)
gini_s <- scale(data$gini_s)
corr_f_s <- scale(data$f_s_corr)

#### fit model for skew_f with all the parameters and individual extrem params
model <- lm(skew_f ~ log_syn_n + log_syn_w + prop_syn + ampa + gaba +
              connectivity + in_sigma + pyr_sigma) # fit the model
plot_model(model, sort.est = TRUE, rm.terms = c('gaba', 'ampa', 'in_sigma',
                                                'connectivity', 'pyr_sigma'))

#### fit model for skew_f with all the parameters and individual extrem params
model <- lm(skew_s ~ log_syn_n + log_syn_w + prop_syn + ampa + gaba +
              connectivity + in_sigma + pyr_sigma) # fit the model
plot_model(model, sort.est = TRUE, rm.terms = c('gaba', 'ampa', 'in_sigma',
                                                'connectivity', 'pyr_sigma'))

#### fit model for skew_f with all the parameters and individual extrem params
model <- lm(kurt_f ~ log_syn_n + log_syn_w + prop_syn + ampa + gaba +
              connectivity + in_sigma + pyr_sigma) # fit the model
plot_model(model, sort.est = TRUE, rm.terms = c('gaba', 'ampa', 'in_sigma',
                                                'connectivity', 'pyr_sigma'))

#### fit model for skew_f with all the parameters and individual extrem params
model <- lm(kurt_s ~ log_syn_n + log_syn_w + prop_syn + ampa + gaba +
              connectivity + in_sigma + pyr_sigma) # fit the model
plot_model(model, sort.est = TRUE, rm.terms = c('gaba', 'ampa', 'in_sigma',
                                                'connectivity', 'pyr_sigma'))

#### fit model for skew_f with all the parameters and individual extrem params
model <- lm(gini_f ~ log_syn_n + log_syn_w + prop_syn + ampa + gaba +
              connectivity + in_sigma + pyr_sigma) # fit the model
plot_model(model, sort.est = TRUE, rm.terms = c('gaba', 'ampa', 'in_sigma',
                                                'connectivity', 'pyr_sigma'))

#### fit model for skew_f with all the parameters and individual extrem params
model <- lm(gini_s ~ log_syn_n + log_syn_w + prop_syn + ampa + gaba +
              connectivity + in_sigma + pyr_sigma) # fit the model
plot_model(model, sort.est = TRUE, rm.terms = c('gaba', 'ampa', 'in_sigma',
                                                'connectivity', 'pyr_sigma'))

#### fit model for skew_f with all the parameters and individual extrem params
model <- lm(corr_f_s ~ log_syn_n + log_syn_w + prop_syn + ampa + gaba +
              connectivity + in_sigma + pyr_sigma) # fit the model
plot_model(model, sort.est = TRUE, rm.terms = c('gaba', 'ampa', 'in_sigma',
                                                'connectivity', 'pyr_sigma'))


