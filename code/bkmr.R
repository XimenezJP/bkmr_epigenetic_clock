library(tidyverse)
library(bkmr)

## Dataset Complete ----------------------------------------------------------------------
df_full <- read.csv("dataset_v2.csv", sep = ',')

model_ob <- df_full %>% filter(Obese == 1)

model_ct <- df_full %>% filter(Obese == 0)

lny_z <- scale(log(model_ct$AgeAccelerationResidual)) #or DNAmAge either AgeAccelerationDiff

mixture     <- with(model_ct, cbind(Hg, As, Cd, Mn,  Ni))
lnmixture   <- apply(mixture, 2, log)
lnmixture_z <- scale(lnmixture)

covariates <- with(model_ct, cbind(Hipertensao, Diabete))

## Fit Models ----------------------------------------------------------------------------
set.seed(200)

fit_model <-  kmbayes(y=lny_z, Z=lnmixture_z, X=covariates, iter=10000000, verbose=TRUE, varsel=TRUE)

summary(fit_model)

## Plots ---------------------------------------------------------------------------------
ggplot(fit_model, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") +
  theme_bw(base_size = 20) +
  facet_wrap(~ variable) +
  labs(x = "ln-transformed concentration",y = "estimates")

ggplot(fit_model, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) +  
  geom_hline(yintercept=00, linetype="dashed", color="darkred")+ 
  geom_pointrange() + 
  theme_bw(base_size = 20) +
  labs(x = "cumulative metal mixture quantile",y = "estimates") 

ggplot(fit_model, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  theme_bw(base_size = 20) +
  theme(legend.position="none") +
  labs(x = "ln-transformed concentration",y = "estimates")

ggplot(fit_model, aes(variable, est, ymin = est - 1.96*sd,  ymax = est + 1.96*sd, col = q.fixed)) +  
  geom_hline(aes(yintercept=0), linetype="dashed", color="gray")+ 
  geom_pointrange(position = position_dodge(width = 0.75)) +  
  coord_flip() +  
  theme_bw(base_size = 20) +
  theme(legend.position="right") +
  labs(x = "metals", y = "estimate", col = "quantiles")
