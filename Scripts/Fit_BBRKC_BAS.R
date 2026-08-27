#Purpose: To evaluate linkages between recruitment and a standard set of ecosystem indicators

#Creator: Curry Cunningham
#With additions from E. Fedewa

#8/23/26 run: pcod density/benthic invert only updated thru 2025

#load
library(tidyverse)
library(corrplot)
library(ggplot2)
library(viridis)
library(ggthemes)
library(BAS)
library(readxl)
library(gbm)
library(stats)

## Read in setup for crab data
source("./Scripts/get_crab_data.R")

#Read in indicator data
indicators <- read.csv("./Output/BBRKC_esp_indicator_timeseries.csv")

# Set years
current_year <- 2026
years <- 1982:current_year

#create folder for model output
BAS_date <- "Aug2026"

fig_dir <- file.path("Figs",
  paste0("BAS_", format(BAS_date)))

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

############################################################
# Calculate abundance of immature male RKC (95-120 mm) as response for BAS analysis

recruit_abun <- calc_bioabund(crab_data = dat,
                         species = "RKC",
                         region = "EBS",
                         district = "BB",
                         years = years,
                         sex = "male",
                         size_min = 95,
                         size_max = 120) %>%
                  select(YEAR, ABUNDANCE) %>%
                  right_join(., expand.grid(YEAR = years)) %>%
                  arrange(YEAR) %>%
                  mutate(ABUNDANCE = as.numeric(ABUNDANCE/1e6)) %>%
                  rename_with(tolower)

#Plot
recruit_abun %>%
  ggplot(aes(x = year, y = abundance)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#Write output 
write.csv(recruit_abun, "./Output/BAS_recruit_abundance.csv", row.names = F)

#join indicator and response
model_dat <- recruit_abun %>%
  right_join(indicators) %>%
  arrange(year) 

############################################################
# MODEL RUN 1: Using design-based BT survey estimate for male recruitment as response

#Assess collinearity b/w indicators 
model_dat %>% 
  select(-year) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(method="color")
#Some highly correlated covariates- but we'll wait to reassess until we lag since
#some indicators are representing different mechanisms

#Look at temporal coverage of indicators 
model_dat %>%
  select(-abundance) %>%
  pivot_longer(c(2:(ncol(model_dat)-1)), names_to="indicator", values_to="value") %>%
  ggplot(aes(year, indicator, size=value)) +
  geom_point() +
  theme_bw()

#Assign Lags for indicators - see metadata file in repo for rationales for lags
dat_lagged <- model_dat %>%
  #dropping indicators that don't have a mechanistic link w/ recruitment
  select(-proportion_closure, -mean_distance_km, -mature_female_d95, 
         -mature_male_d95, -ratio) %>%
  mutate(temp_lag = lag(date_corrected_temp, n=2, order_by = year),
         clutch_lag = lag(prop_empty, n=7, order_by = year),
         pcod_lag = lag(pcod_density, n=1, order_by = year), 
         ao_lag = lag(mean_ao, n=6, order_by = year),
         salmon_lag = lag(inshore_run, n=6, order_by = year), 
         invert_lag = lag(benthic_invert, n=1, order_by = year),
         wind_lag = lag(wind_stress, n=6, order_by = year),
         chla_lag = lag(chla, n=6, order_by = year),
         ph_lag = lag(ph, n=4, order_by = year))%>%
  select(-c(date_corrected_temp, prop_empty, pcod_density, mean_ao, inshore_run, 
            benthic_invert, wind_stress, chla, ph)) 

#plot timeseries with lagged covariates 
dat_lagged %>%
  pivot_longer(c(2:(ncol(dat_lagged))), names_to="indicator", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~indicator, scales = "free") +
  theme_bw()

#Temporal coverage with lags incorporated 
dat_lagged %>%
  select(-abundance) %>%
  pivot_longer(c(2:(ncol(dat_lagged)-1)), names_to="indicator", values_to="value") %>%
  ggplot(aes(year, indicator, size=value)) +
  geom_point(na.rm=T) +
  theme_bw()

#Assess collinearity b/w lagged indicators 
dat_lagged %>% 
  select(-year) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(method="number")
#looks okay to proceed with full suite of indicators

#Lets also look at distributions of potentially problematic covariates
hist(dat_lagged$abundance)
hist(dat_lagged$salmon_lag)
hist(dat_lagged$invert_lag)
hist(dat_lagged$pcod_lag)

#log transform recruitment predictor
dat_bas <- dat_lagged %>% 
  mutate(ln_rec=log(abundance)) %>%
  select(-abundance)

hist(dat_bas$ln_rec)

#check whether log recruitment is autocorrelated
acf(dat_bas$ln_rec, na.action = na.pass)

#Define covariates
covars <- names(dat_bas %>% select(-year, -ln_rec))

# Standardize Predictors 
dat_zscore <- dat_bas %>%
  mutate(across(-c(year, ln_rec), ~ as.numeric(scale(.x)),  .names = "z_{.col}")) %>%
  select(-temp_lag, -clutch_lag,-pcod_lag,-ao_lag,-salmon_lag,
         -invert_lag, -wind_lag, -ph_lag, -chla_lag)
#When predictors are z-scored, the regression coefficients represent the change in the outcome variable
#(in standard deviations) for a one-standard-deviation change in the predictor. 
#This allows for direct comparison of the strength/importance of different predictors.

# final plot with lagged/z-scored indicators and log recruitment response
plot <- dat_zscore %>%
  rename("Bottom Temperature" = z_temp_lag, "% Empty Clutches" = z_clutch_lag, 
         "Pacific Cod Density" = z_pcod_lag, "Arctic Oscillation" = z_ao_lag, 
         "Sockeye Run Size" = z_salmon_lag, "Benthic Prey Density" = z_invert_lag,
         "Wind Stress" = z_wind_lag, "pH" = z_ph_lag, "Chl-a" = z_chla_lag) %>%
  pivot_longer(cols= -c(year, ln_rec), names_to = "indicator", values_to = "value") 

plot %>%
ggplot() +
  geom_point(aes(year, value), color="blue") + 
  geom_line(aes(year, value), color="blue") + 
  geom_line(data = plot %>%
              select(year, ln_rec), 
            aes(year, ln_rec), color = "grey50", linetype = 6) +
 labs(y = "", x = "") +
  facet_wrap(~ indicator, scales = "free_x") + 
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA, color = "white"),
        strip.background = element_blank())

ggsave(filename = paste0("./Figs/BAS_", BAS_date, "/covariates.png"))

#Fit Models ====================================

#MODEL #1: longer timeseries by kicking out chla:

model1_predictors <- c("z_temp_lag", "z_clutch_lag", "z_pcod_lag",
                      "z_ao_lag", "z_salmon_lag", "z_invert_lag",
                       "z_wind_lag", "z_ph_lag")

# Bayesian Model Selection
bas_fit1 <-  bas.lm(ln_rec ~ z_temp_lag  + z_clutch_lag + z_pcod_lag +
                      z_ao_lag + z_salmon_lag + z_invert_lag +
                      z_wind_lag + z_ph_lag,
                    data = dat_zscore,
                    modelprior=uniform(), initprobs="Uniform",
                    method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas_fit1)
bas_fit1

#Diagnostic Plots
plot(bas_fit1)
plot(coef(bas_fit1))
plot(bas_fit1, which = 4)
coef.mod <- coef(bas_fit1)
plot(confint(coef.mod))

#Results plots

covariate_labels <- c(
  z_temp_lag   = "Bottom Temperature",
  z_clutch_lag = "% Empty Clutches",
  z_pcod_lag   = "Pacific Cod Density",
  z_ao_lag     = "Arctic Oscillation",
  z_salmon_lag = "Sockeye Run Size",
  z_invert_lag = "Benthic Prey Density",
  z_wind_lag   = "Wind Stress",
  z_ph_lag     = "pH")

#model space
image(bas_fit1, rotate = FALSE,
  drop.always.included = TRUE)

#Posterior prediction probabilities
bas_coef <- coef(bas_fit1)
ci <- confint(bas_coef)

plot.df <- data.frame(
  bas.names = bas_coef$namesx,
  inc.probs = bas_coef$probne0,
  post.mean = bas_coef$postmean,
  post.sd = bas_coef$postsd,
  low.95 = ci[, 1], up.95 = ci[, 2]) %>%
  filter(bas.names != "Intercept") %>%
  mutate(indicator = dplyr::recode(
      bas.names, !!!covariate_labels))

#Posterior coefficient effects
p_effect <- ggplot(plot.df, aes(x = indicator, y = post.mean)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, alpha = 0.6) +
  geom_errorbar(aes(ymin = low.95, ymax = up.95),
    width = 0.25, linewidth = 0.7) +
  geom_point(size = 3, shape = 21, fill = "royalblue4",
    color = "black") +
  coord_flip() +
  labs(x = NULL, y = "Effect") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA,
      linewidth = 0.5),
    axis.text.y = element_text(color = "black"))

#Posterior inclusion probabilities
p_prob <- ggplot(plot.df,
  aes(x = indicator, y = inc.probs, fill = inc.probs)) +
  geom_col(color = "black", linewidth = 0.4, width = 0.9) +
  geom_hline(yintercept = 0.5, color = "black", linetype = 2,
    linewidth = 0.6) +
  scale_y_continuous(limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0.02))) +
  scale_fill_gradient(
    low = "lightblue",
    high = "royalblue4") +
  coord_flip() +
  labs(x = NULL, y = "Inclusion\nProbability") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank())


#Observed vs Predicted
pred.bas <- predict(bas_fit1, estimator = "BMA")

fit_dat <- dat_zscore %>%
  select(year, ln_rec, all_of(model1_predictors)) %>%
  drop_na() %>%
  mutate(predicted = pred.bas$Ybma)

#qq plot
p_obs_pred <- ggplot(fit_dat, aes(x = ln_rec, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1.2,
    color = "royalblue3", alpha = 0.7) +
  geom_point(shape = 21, size = 3, fill = "indianred2",
    color = "black", alpha = 0.8) +
  labs(x = "Observed ln(Recruitment)", y = "Predicted ln(Recruitment)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

#time series plot
ts_dat <- fit_dat %>%
  select(year, Observed = ln_rec, Predicted = predicted) %>%
  pivot_longer(cols = c(Observed, Predicted),
    names_to = "type", values_to = "ln_rec")

p_timeseries <- ggplot(ts_dat, aes(x = year, y = ln_rec,
    color = type)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(data = ts_dat %>%
      filter(type == "Observed"),
    shape = 21, size = 2.5, fill = "indianred2", color = "black") +
  scale_color_manual(values = c(Observed = "indianred2",
      Predicted = "royalblue3")) +
  labs(x = "Year", y = "ln(Recruitment)",color = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.15, 0.1),
    legend.background = element_rect(fill = "white", color = "grey40"),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.margin = margin(2, 2, 2, 2))

#combine plots and save
final_BAS_plot <-
  (p_effect | p_prob) /
  (p_obs_pred | p_timeseries) + plot_annotation(tag_levels = "A")

ggsave(final_BAS_plot, filename = paste0("./Figs/BAS_", BAS_date, "/model_results.png"),
       width = 9, height = 6.5, units = "in", dpi = 300)

#=============================================================
#Sensitivity Analysis: Model with recruitment autoregression term 
  #i.e. do ecosystem indicators explain recruitment after accounting for recruitment persistence?

dat_fit <- dat_zscore %>%
  mutate(rec_lag1 = lag(ln_rec)) %>%
  select(-year) 

#Holding off on this given that we're moving to DSEM!

#=============================================================
#### Leave-one-out cross validation on short timeseries model 2 run
  #Script developed by Krista Oke (sablefish ESP)

#Because we don't have response data for 2020, we'll drop it from the analysis
dat.mod2 %>%
  filter(YEAR != 2020) -> dat.loocv

covars <- names(dat.loocv)
n.cov <- length(dat.loocv)

#STEP 1 - Loop through training sets and fit models-------
  #Using model averaging (BMA estimator) to predict which produces a range of predictions
  #instead of the highest prob model (HPM estimator) b/c HPM results in some loops selecting 
  #different best models 

scaled_loop_dat <- dat.loocv

yrs <- unique(scaled_loop_dat$YEAR)
output_df <- data.frame(matrix(ncol=3, nrow = length(yrs)))
colnames(output_df) <- c("YEAR", "observed_ln_recruit", "predicted_ln_recruit")

i<-1
for(i in 1:length(scaled_loop_dat$YEAR)){
  print(i)
  temp_dat <- scaled_loop_dat[-i,]
  
  temp_dat <- temp_dat[-which(names(temp_dat) %in% c("YEAR"))]
  temp_dat <- temp_dat[which(names(temp_dat) %in% covars)]
  
  dropped_yr <- scaled_loop_dat[i,]
  output_df$observed_ln_recruit[i] <- dropped_yr$ln_rec
  dropped_yr <- dropped_yr[,names(dropped_yr) %in% covars]
  dropped_yr <- dropped_yr[,!names(dropped_yr) %in% "ln_rec"]
  print(dropped_yr$YEAR)
  #fit model
  bas.loop <-  bas.lm(ln_rec ~ ., data=temp_dat,
                      # prior="ZS-null",
                      modelprior=uniform(), initprobs="Uniform",
                      method='BAS', MCMC.iterations=1e5, thin=10)
  
  #have model predict to missing year
  temp_predict <- predict(bas.loop, newdata=dropped_yr, estimator="BMA")
  print(temp_predict$bestmodel)
  #write to output object so we can compare predicted vs obs
  output_df$YEAR[i] <- dropped_yr$YEAR
  output_df$predicted_ln_recruit[i] <- temp_predict$fit
}

output_df$predicted_ln_recruit <- as.numeric(as.character(output_df$predicted_ln_recruit))

#plot observed vrs. predicted 
ggplot(output_df, aes(observed_ln_recruit, predicted_ln_recruit)) + 
  # geom_point() + 
  geom_smooth(method="lm") + geom_abline(intercept = 0, slope = 1) + 
  geom_text(aes(observed_ln_recruit, predicted_ln_recruit, label=YEAR))+
  ylim(c(0,5)) + xlim(c(0,5)) + theme_bw()

#get MSE & MAE------

#these need to be double checked!
#BAS_MSE <- ((sum((output_df$observed_ln_recruit - output_df$predicted_ln_recruit)^2, na.rm = TRUE)))/length(output_df$observed_ln_recruit)


obs_pred_mod <- lm(predicted_ln_recruit ~ observed_ln_recruit, data=output_df)
summary(obs_pred_mod)

output_df$diff <- output_df$predicted_ln_recruit - output_df$observed_ln_recruit

ggplot(output_df, aes(YEAR, diff, col=as.numeric(YEAR))) + 
  geom_point() + geom_smooth(method="lm") +
  theme_bw() +
  xlim(2004,2024) +
  theme(legend.title=element_blank())
  

BAS_long_rmse <- rmse(output_df, truth=observed_ln_recruit, 
                      estimate=predicted_ln_recruit, na.rm=TRUE)

BAS_long_mae <- mae(output_df, truth=observed_ln_recruit, 
                    estimate=predicted_ln_recruit, na.rm=TRUE)

#write.csv(output_df, file=paste(wd,"/data/BAS_obsvpreds_long.csv", sep=""))
#output_df_long <- read.csv(file=paste(wd,"/data/BAS_obsvpreds_long.csv", sep=""))

#=============================================================
#### Leave-one-out cross validation on long timeseries model 1 run
#Script developed by Krista Oke (sablefish ESP)

#Because we don't have response data for 2020, we'll drop it from the analysis
dat.mod1 %>%
  filter(YEAR != 2020) -> dat.loocv

covars <- names(dat.loocv)
n.cov <- length(dat.loocv)

#STEP 1 - Loop through training sets and fit models-------
#Using model averaging (BMA estimator) to predict which produces a range of predictions
#instead of the highest prob model (HPM estimator) b/c HPM results in some loops selecting 
#different best models 

scaled_loop_dat <- dat.loocv

yrs <- unique(scaled_loop_dat$YEAR)
output_df <- data.frame(matrix(ncol=3, nrow = length(yrs)))
colnames(output_df) <- c("YEAR", "observed_ln_recruit", "predicted_ln_recruit")

i<-1
for(i in 1:length(scaled_loop_dat$YEAR)){
  print(i)
  temp_dat <- scaled_loop_dat[-i,]
  
  temp_dat <- temp_dat[-which(names(temp_dat) %in% c("YEAR"))]
  temp_dat <- temp_dat[which(names(temp_dat) %in% covars)]
  
  dropped_yr <- scaled_loop_dat[i,]
  output_df$observed_ln_recruit[i] <- dropped_yr$ln_rec
  dropped_yr <- dropped_yr[,names(dropped_yr) %in% covars]
  dropped_yr <- dropped_yr[,!names(dropped_yr) %in% "ln_rec"]
  print(dropped_yr$YEAR)
  #fit model
  bas.loop <-  bas.lm(ln_rec ~ ., data=temp_dat,
                      # prior="ZS-null",
                      modelprior=uniform(), initprobs="Uniform",
                      method='BAS', MCMC.iterations=1e5, thin=10)
  
  #have model predict to missing year
  temp_predict <- predict(bas.loop, newdata=dropped_yr, estimator="BMA")
  print(temp_predict$bestmodel)
  #write to output object so we can compare predicted vs obs
  output_df$YEAR[i] <- dropped_yr$YEAR
  output_df$predicted_ln_recruit[i] <- temp_predict$fit
}

output_df$predicted_ln_recruit <- as.numeric(as.character(output_df$predicted_ln_recruit))

#plot observed vrs. predicted 
ggplot(output_df, aes(observed_ln_recruit, predicted_ln_recruit)) + 
  # geom_point() + 
  geom_smooth(method="lm") + geom_abline(intercept = 0, slope = 1) + 
  geom_text(aes(observed_ln_recruit, predicted_ln_recruit, label=YEAR))+
  ylim(c(0,5)) + xlim(c(0,5)) + theme_bw()

#get MSE & MAE------

#these need to be double checked!
#BAS_MSE <- ((sum((output_df$observed_ln_recruit - output_df$predicted_ln_recruit)^2, na.rm = TRUE)))/length(output_df$observed_ln_recruit)


obs_pred_mod <- lm(predicted_ln_recruit ~ observed_ln_recruit, data=output_df)
summary(obs_pred_mod)

output_df$diff <- output_df$predicted_ln_recruit - output_df$observed_ln_recruit

ggplot(output_df, aes(YEAR, diff, col=as.numeric(YEAR))) + 
  geom_point() + geom_smooth(method="lm") +
  theme_bw() +
  xlim(1986,2024) +
  theme(legend.title=element_blank())

BAS_long_rmse <- rmse(output_df, truth=observed_ln_recruit, 
                      estimate=predicted_ln_recruit, na.rm=TRUE)

BAS_long_mae <- mae(output_df, truth=observed_ln_recruit, 
                    estimate=predicted_ln_recruit, na.rm=TRUE)

#=============================================================


#Results do not appear robust to shifting reference period- how to provide 
  #management advice under non-stationarity and/or over-fitting?! 

#BAS cannot handle missing data! 2020 is problematic, and additional years are dropped
  #once lags are applied 

#Good resource for diagnostics: https://cran.r-project.org/web/packages/BAS/vignettes/BAS-vignette.html
