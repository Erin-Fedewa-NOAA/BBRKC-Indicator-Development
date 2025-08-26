#Purpose: To evaluate linkages between recruitment and a standard set of ecosystem indicators

#Creator: Curry Cunningham
#With additions from E. Fedewa

#Recent run: 8/19 doesn't include pH, and 
  #chla/pcod density/sockeye/benthic invert only updated thru 2024

#NOTES for 2026: Use MMB or time varying mortality as response instead of recruitment?
#Explore a model run with recruitment model output- these are 25-40mm though, and 
#some lags would be tough 

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
current_year <- 2025
years <- 1982:current_year

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
recruit_abun %>%
  right_join(indicators) %>%
  arrange(year) -> model_dat

############################################################
# MODEL RUN 1: Using design-based BT survey estimate for male recruitment as response

#Assess collinearity b/w indicators 
model_dat %>% 
  select(-year) %>%
  cor(use = "complete.obs") %>%
  corrplot(method="color")
#Some highly correlated covariates- but we'll wait to reassess until we lag since
#some indicators are representing different mechanisms

#Quick look at cross-correlations, though we'll stick to mechanistic understanding/
  #hypotheses to assign lags below
ccf(model_dat$summer_bt, model_dat$abundance, na.action = na.pass, lag.max = 7)
ccf(model_dat$prop_empty, model_dat$abundance, na.action = na.pass, lag.max = 7)
ccf(model_dat$pcod_density, model_dat$abundance, na.action = na.pass, lag.max = 7)
ccf(model_dat$Mean_AO, model_dat$abundance, na.action = na.pass, lag.max = 7)
ccf(model_dat$inshore_run_size, model_dat$abundance, na.action = na.pass, lag.max = 7)
ccf(model_dat$benthic_invert_density, model_dat$abundance, na.action = na.pass, lag.max = 7)
ccf(model_dat$wind_stress, model_dat$abundance, na.action = na.pass, lag.max = 7)
ccf(model_dat$chla, model_dat$abundance, na.action = na.pass, lag.max = 7)

#Look at temporal coverage of indicators 
model_dat %>%
  select(-abundance) %>%
  pivot_longer(c(2:(ncol(model_dat)-1)), names_to="indicator", values_to="value") %>%
  ggplot(aes(year, indicator, size=value)) +
  geom_point() +
  theme_bw()

#Assign Lags for indicators - see metadata file in repo for rationales for lags
model_dat %>%
  #dropping indicators that don't have a mechanistic link w/ recruitment
  select(-prop_closed_area, -mean_distance_shore_km, -mature_female_d95, 
         -mature_male_d95, -bbrkc_northern_ratio) %>%
  mutate(temp_lag = lag(summer_bt, n=2, order_by = year),
         clutch_lag = lag(prop_empty, n=7, order_by = year),
         pcod_lag = lag(pcod_density, n=1, order_by = year), 
         ao_lag = lag(Mean_AO, n=6, order_by = year),
         salmon_lag = lag(inshore_run_size, n=6, order_by = year), 
         invert_lag = lag(benthic_invert_density, n=1, order_by = year),
         wind_lag = lag(wind_stress, n=6, order_by = year),
         chla_lag = lag(chla, n=6, order_by = year))%>%
  select(-c(summer_bt, prop_empty, pcod_density, Mean_AO, inshore_run_size, 
            benthic_invert_density, wind_stress, chla)) -> dat_lagged

#plot timeseries with lagged covariates 
dat_lagged %>%
  pivot_longer(c(2:(ncol(dat_lagged))), names_to="indicator", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~indicator, scales = "free_y") +
  theme_bw()

#Temporal coverage with lags incorporated 
dat_lagged %>%
  select(-abundance) %>%
  pivot_longer(c(2:(ncol(dat_lagged)-1)), names_to="indicator", values_to="value") %>%
  ggplot(aes(year, indicator, size=value)) +
  geom_point(na.rm=T) +
  theme_bw()
#Including wind means we start at 94 instead of 89
#chla limits our timeseries to 2004+ and only includes n=19 years

#Assess collinearity b/w lagged indicators 
dat_lagged %>% 
  select(-year) %>%
  cor(use = "complete.obs") %>%
  corrplot(method="number")
#looks okay to proceed with full suite of indicators

#Lets also look at distributions of potentially problematic covariates
hist(dat_lagged$abundance)
hist(dat_lagged$salmon_lag)
hist(dat_lagged$invert_lag)
hist(dat_lagged$pcod_lag)

# Final data wrangling 
dat_bas <- dat_lagged %>% 
  mutate(ln_rec=log(abundance)) 

hist(dat_bas$ln_rec)

#Define covariates
covars <- names(dat_bas %>% select(-year, -abundance, -ln_rec))

# Plot Covariates Z scored 
covar.list <- dat_bas %>% 
  select(-abundance, -ln_rec) %>% 
  gather(key=type, value=value, -year) 

ggplot(covar.list, aes(x=value, fill=type)) +
  theme_linedraw() +
  geom_histogram() +
  geom_density(alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~type, scales='free') +
  theme(legend.position = "NA")

# Z-score Predictors that are bounded at zero 
dat_bas %>%
  select(-chla_lag) %>%
  mutate(across(c(3:9), ~ (.-mean(.,na.rm=T))/sd(.,na.rm=T), .names = "z_{.col}")) %>%
  select(-temp_lag, -clutch_lag,-pcod_lag,-ao_lag,-salmon_lag,
         -invert_lag, -wind_lag, -abundance) %>%
  rename("Bottom Temperature" = z_temp_lag, "% Empty Clutches" = z_clutch_lag, 
         "Pacific Cod Density" = z_pcod_lag, "Arctic Oscillation" = z_ao_lag, 
         "Sockeye Run Size" = z_salmon_lag, "Benthic Prey Density" = z_invert_lag,
         "Wind Stress" = z_wind_lag) -> dat_zscore
#When predictors are z-scored, the regression coefficients represent the change in the outcome variable
#(in standard deviations) for a one-standard-deviation change in the predictor. 
#This allows for direct comparison of the strength/importance of different predictors.

# final plot with lagged/z-scored indicators and log recruitment response
z.ts.plot <- dat_zscore %>%
  select(-ln_rec) %>%
  pivot_longer(c(2:(ncol(dat_zscore)-1)), names_to = "indicator", values_to = "value") %>%
  mutate(indicator = factor(indicator, 
                            levels = c("Bottom Temperature", "% Empty Clutches",
                                       "Pacific Cod Density","Arctic Oscillation",
                                       "Sockeye Run Size","Benthic Prey Density", 
                                       "Wind Stress")))


ggplot() +
  geom_point(data = z.ts.plot, aes(year, value), color="blue") + 
  geom_line(data = z.ts.plot, aes(year, value), color="blue") + 
  geom_line(data = dat_zscore %>%
              select(year, ln_rec), 
            aes(year, ln_rec), color = "grey50", linetype = 6) +
 labs(y = "", x = "") +
  facet_wrap(~ indicator, scales = "free_x") + 
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA, color = "white"),
        strip.background = element_blank())

ggsave("./Figs/BAS_Aug_2025/covariates.png")

#Fit Models ====================================

#MODEL #1: longer timeseries by kicking out chla:

#remove year from dataset
dat_zscore %>%
  select(-year) -> dat_fit

# Bayesian Model Selection
bas.lm <-  bas.lm(ln_rec ~ ., data = dat_fit,
                  modelprior=uniform(), initprobs="Uniform",
                  method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas.lm)
bas.lm

#Diagnostic Plots
plot(bas.lm)
plot(coef(bas.lm))
image(bas.lm, rotate = F, drop.always.included = TRUE)
plot(bas.lm, which = 4)
coef.mod <- coef(bas.lm)
plot(confint(coef.mod))


# Plot Model Predictions vs. Observed ==============================

pdf(file.path("Figs/BAS_Aug_2025","Model 1 Fit.pdf"), height=5, width=10)
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
pred.bas <- predict(bas.lm, estimator="BMA")

# Omit NAs
dat.temp.na.omit <- na.omit(dat_zscore)

plot(x=dat.temp.na.omit$ln_rec, y=pred.bas$Ybma,
     xlab="Observed ln(Recruitment)", ylab="Predicted ln(Recruitment)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
# Title
mtext(paste("Snow Crab Aug 19"), side=3, outer=TRUE, font=2)
# plot(x=pred.bas$fit, y=pred.bas$Ybma) 
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3) 

# Timeseries
plot(x=dat.temp.na.omit$year, y=dat.temp.na.omit$ln_rec,
     xlab="year", ylab="ln(Recruitment)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.temp.na.omit$year, y=dat.temp.na.omit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.temp.na.omit$year, y=pred.bas$Ybma, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('bottomleft', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
                                                                rgb(0,0,1, alpha=0.5)),
       bg="white") 
 
dev.off()


## Plot inclusion probabilities ------------------------------------------------
inc.probs <- summary(bas.lm)[2:ncol(dat_fit), 1]

bas.names <- coef(bas.lm)$namesx
inc.probs <- coef(bas.lm)$probne0
post.mean <- coef(bas.lm)$postmean
post.sd <- coef(bas.lm)$postsd
low.95 <- confint(coef(bas.lm))[,1]
up.95 <- confint(coef(bas.lm))[,2]

# Make final plot of covariates
plot.df <- data.frame(bas.names, inc.probs, post.mean, post.sd, low.95, up.95)

# Parameter estimates/CI
plot.df %>%
  filter(bas.names != 'Intercept') %>%
  ggplot(aes(x = bas.names, post.mean, fill = 'royalblue4')) +
  theme_bw() +
  geom_errorbar(aes(ymin = low.95, ymax = up.95), width = 0.25) +
  geom_point(pch = 21, fill = 'royalblue4', size = 3) +
  geom_hline(yintercept = 0, col = 'red', alpha = 0.5) +
  ylab('Effect') +
  xlab('Covariate') +
  coord_flip() +
  theme(legend.position = 'none') -> effect

# Inclusion probability
plot.df %>%
  filter(bas.names != 'Intercept') %>%
  ggplot(aes(x = bas.names, y = inc.probs, fill = inc.probs)) +
  theme_bw() +
  geom_bar(stat = 'identity', color = 'black') +
  ylab('Inclusion\nProbability') +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = c(0, 1)) +
  geom_hline(yintercept = 0.5, col = 'black', linetype = 5, alpha = 0.5) +
  theme(legend.position = 'none', axis.text.y = element_blank(), 
        axis.title.y = element_blank()) +
  coord_flip() +
  scale_fill_continuous_tableau() -> prob

# Bring figures together and save
effect + prob 
ggsave("./Figs/BAS_Aug_2025/covariate_effects.png")

#############################################################

#MODEL #2: short timeseries with chla:

#remove year from dataset
dat_zscore %>%
  select(-year) -> dat_fit

# Bayesian Model Selection
bas.lm.2 <-  bas.lm(ln_rec ~ ., data = dat_fit,
                  modelprior=uniform(), initprobs="Uniform",
                  method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas.lm.2)

#So the shorter run is telling us the same thing, let's stick with results 
  #from the longer run! 


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
