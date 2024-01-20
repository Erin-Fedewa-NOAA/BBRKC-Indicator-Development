#==================================================================================================
#Project Name: ECOSYSTEM AND SOCIOECONOMIC PROFILES - BBRKC
#
#Creator: Dr. Curry James Cunningham, UAF, CFOS
#Additions by Erin Fedewa
#Date: 8/27/23
#
#Purpose: To evaluate linkages between recruitment and a standard set of atmospheric, oceanographic, 
# and biological indicators of ecosystem status for the Ecosystem and Socioeconomic Profiles (ESPs).
#
#==================================================================================================
#NOTES:
#2023 BBRKC Indicator dataset is raw data from the webservice, so requires some wrangling
#Response variable #1, male survey abundance output is produced via separate script
  #This output is then merged with indicator timeseries for the BAS analysis 

#2024 Follow ups- run a second BAS model with RKC recruitment output from assmt model 
#Extend available timeseries like temperature/AO etc. to 1980 so that we don't lose data when 
  #lagging 7 yrs 
#Assess different techniques to address non-stationarity 
#Additional models with other response variables and BB sockeye inshore run timeseries 
#Clean up BAS script and figures, create Fig. 4 in script
#==================================================================================================
#TIMING:
#Initial run May 2019, model run by Curry
#Follow up runs with new indicators/modeled rec response in Sept 2023, model run by Erin
##==================================================================================================
#LOAD:
require(tidyverse)
require(corrplot)
require(cowplot)
require(ggplot2)
require(viridis)
require(ggthemes)
require(BAS)
require(readxl)
require(dplyr)
require(gbm)
#=============================================================
#DEFINE DIRECTORY STRUCTURE:
wd <- getwd()

dir.data <- file.path(wd,"Data")
dir.output <- file.path(wd,"Output")
dir.figs <- file.path(wd, "Figs")
#=============================================================
#READ IN DATA:
dat <- read.csv("./Data/bbrkc_2023_indicators_replace_sockeye.csv") #csv was created post Sept 2023 
  #BBRKC ESP run and BASIS salmon abundance was replaced with BB sockeye inshore run size indicator 
r_survey <- read.csv("./Output/BAS_survey_recruit.csv")
#=============================================================
#CONTROL SECTION:

fit <- TRUE

offset <- 0

#Define Model Name
model <- "BAS_Mod_Dev" #Cleaning up script and model exploration, 1/25

#Update location references for figs and outputs
dir.output <- file.path(dir.output, model)
dir.create(dir.output, recursive=TRUE)
dir.figs <- file.path(dir.figs, model)
dir.create(dir.figs, recursive=TRUE)

#For Data
if(model=="BAS_Mod_Dev") {
  years <- unique(dat$YEAR[dat$INDICATOR_TYPE=="Ecosystem"])
  n.years <- length(years)
}

if(model!="BAS_Mod_Dev") {
  years <- NULL
  n.years <- NULL
  stop(paste("WRONG model:", model))
}
#Whether to do initial exploratory plots
do.initPlot <- TRUE

#Remove Correlated Covariates:
# rem.cor.cov <- FALSE

# Plotting Fxns ========================
q.50 <- function(x) { return(quantile(x, probs=c(0.25,0.75))) }
q.95 <- function(x) { return(quantile(x, probs=c(0.025,0.975))) }

q_0.025 <- function(x) { return(quantile(x, probs=0.025)) }
q_0.975 <- function(x) { return(quantile(x, probs=0.975)) }

#=============================================================
#CLEAN UP DATA AND ASSIGN LAGS:

#Data wrangling of webservice indicator data 
dat %>%
  select(YEAR, INDICATOR_NAME, DATA_VALUE, INDICATOR_TYPE) %>%
  filter(INDICATOR_TYPE != "Socioeconomic") %>%
  select(-INDICATOR_TYPE) %>%
  pivot_wider(names_from="INDICATOR_NAME", values_from="DATA_VALUE") %>%
  left_join((r_survey %>% rename("YEAR"="SURVEY_YEAR")), by="YEAR") %>%
  rename("imm_survey_abun"="ABUNDANCE_MIL") -> rkcindic

#Look at temporal coverage of indicators 
rkcindic %>%
  select(!imm_survey_abun) %>%
  pivot_longer(c(2:15), names_to="indicator", values_to="value") %>%
  ggplot(aes(YEAR, indicator, size=value)) +
  geom_point() +
  theme_bw()
#pH and chla are the shortest timeseries

#And plot timeseries with all covariates 
rkcindic %>%
  pivot_longer(c(2:15), names_to="indicator", values_to="value") %>%
  ggplot(aes(YEAR, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~indicator, scales = "free_y") +
  theme_bw()

#Assign Lags for indicators (see metadata file in repo for rationales for lags)
rkcindic %>%
  select(-c(Annual_Red_King_Crab_Catch_Distance_Shore_BBRKC_Fishery, #not driver of recruitment, dropping
            Annual_Red_King_Crab_Recruit_Abundance_BBRKC_Survey, #model response
            Summer_Red_King_Crab_Female_Area_Occupied_BBRKC_Model, #not driver of recruitment, dropping
            Summer_Red_King_Crab_Male_Area_Occupied_BBRKC_Model, #not driver of recruitment, dropping
            Spring_Corrosivity_Index_BBRKC_Model)) %>% #using pH instead of corrosivity
  mutate(ao_lag = lag(Winter_Spring_Arctic_Oscillation_Index_Model, n=7, order_by = YEAR),
         wind_lag = lag(Summer_Wind_Stress_BBRKC_Satellite, n=7, order_by = YEAR),
         temp_lag = lag(Summer_Temperature_Bottom_BBRKC_Survey, n=6, order_by = YEAR),
         salmon_lag = lag(Sockeye_Inshore_Run_Size, n=7, order_by = YEAR),
         chla_lag = lag(Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite, n=7, order_by = YEAR),
         ph_lag = lag(Spring_pH_BBRKC_Model, n=6, order_by = YEAR),
         cod_lag = lag(Summer_Pacific_Cod_Density_BBRKC_Survey, n=1, order_by = YEAR),
         invert_lag = lag(Summer_Benthic_Invertebrate_Density_BBRKC_Survey, n=1, order_by = YEAR),
         cp_lag = lag(Summer_Cold_Pool_SEBS_BBRKC_Survey, n=2, order_by = YEAR)) %>%
  select(-c(Winter_Spring_Arctic_Oscillation_Index_Model, Summer_Wind_Stress_BBRKC_Satellite, 
         Summer_Temperature_Bottom_BBRKC_Survey, Sockeye_Inshore_Run_Size,
         Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite, Spring_pH_BBRKC_Model,
         Summer_Pacific_Cod_Density_BBRKC_Survey, Summer_Benthic_Invertebrate_Density_BBRKC_Survey,
         Summer_Cold_Pool_SEBS_BBRKC_Survey)) -> dat1

#Lets plot again and look at temporal coverage with lags incorporated 
dat1 %>%
  select(!imm_survey_abun) %>%
  pivot_longer(c(2:10), names_to="indicator", values_to="value") %>%
  ggplot(aes(YEAR, indicator, size=value)) +
  geom_point(na.rm=T) +
  theme_bw()
#with so many large ELH lags, we're losing early years in the timeseries 

#And plot timeseries with lagged covariates 
dat1 %>%
  pivot_longer(c(2:10), names_to="indicator", values_to="value") %>%
  ggplot(aes(YEAR, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~indicator, scales = "free_y") +
  theme_bw()

#Lets also look at distributions of potentially problematic covariates
hist(dat1$imm_survey_abun)
hist(dat1$salmon_lag) #looks okay, yeah?
hist(dat1$invert_lag) #also looks okay

#Determine Covariates
if(model=="BAS_Mod_Dev") {
  covars <- names(dat1)[-which(names(rkcindic) %in% c("YEAR", "imm_survey_abun"))]
}
n.cov <- length(covars) #9 covariates 

#=============================================================
#CALCULATE LOG RECRUITMENT

dat.2 <- dat1 %>% 
  mutate("ln_rec"=log(imm_survey_abun))

# Log transform biomass predictors if needed

#hist(log(dat.2$salmon_lag))
#hist(log(dat.2$invert_lag))

#if(model=="BAS_Mod_Dev") {
  #dat.2$salmon_lag_log <- log(dat.2$salmon_lag)
  #dat.2$invert_lag_log <- log(dat.2$invert_lag)
#}

#=============================================================
#STANDARDIZE COVARIATES AND ASSESS COLLINIERITY 

#Limit years
#dat.3 <- dat.2 %>% 
  #filter(year %in% years)

#Plot Covariates
covar.list <- dat.2 %>% 
  dplyr::select(-c("imm_survey_abun","ln_rec")) %>% 
  gather(key=type, value=value, -YEAR) 
head(covar.list)

explore.hist <- ggplot(covar.list, aes(x=value, fill=type)) +
  theme_bw() +
  geom_histogram() +
  geom_density(alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~type, scales='free') +
  theme(legend.position = "NA")

ggsave(file.path(dir.figs,"Covar Histogram.png"), plot=explore.hist, 
       height=8, width=12, units='in')

# Z-score Predictors that are bounded at zero 
dat.4 <- dat.2
c <- 1
for(c in 1:n.cov) {
  dat.4[[covars[c]]] <- (dat.4[[covars[c]]] - mean(dat.4[[covars[c]]], na.rm=TRUE)) / sd(dat.4[[covars[c]]], na.rm=TRUE)
}

# Checkup - make sure all predictors are correctly z-scored
apply(dat.4, 2, mean, na.rm=TRUE)
apply(dat.4, 2, sd, na.rm=TRUE)
# }

# Subset Data for Fitting 
if(model=="BAS_Mod_Dev") {
  dat.fit <- dat.4 %>% dplyr::select(-c("imm_survey_abun"))
  dat.fit.list <- dat.fit %>% gather(key='var', value='value', -YEAR)
}  

#Plot Standardized Indicator Timeseries
if(do.initPlot==TRUE) {
  g <- dat.fit.list %>% filter(var!="ln_rec") %>% 
    ggplot(aes(x=YEAR, y=var, fill=value)) +
    theme_linedraw() +
    # geom_point()
    geom_point(aes(cex=value), alpha=0.5, pch=21, color='black') +
    scale_fill_viridis_c() +
    ggtitle("Standardized Covariate Values")
  g
  ggsave(file.path(dir.figs,"Standardized Covariates.png"), plot=g, height=6, width=10, units='in', dpi=500)
  
#Correlation Plot
  covar.mtx <- dat.fit %>% 
    dplyr::select(-c("YEAR"))
  
corr.mtx <- cor(covar.mtx, use="na.or.complete")
corrplot(corr.mtx, method = 'number')
  ggsave(file.path(dir.figs,"Correlation Covariates.png"))
corrplot(corr.mtx, method = 'color', order = 'alphabet')
}

#Fairly high correlation (0.71) b/w cod biomass and sockeye salmon but no clear mechanism?
  #We'll need to drop one of the two for model inference

#=============================================================
#FIT MODELS (all models use survey-derived male recruit abundance as a response)

#======
#MODEL RUN 1: Long timeseries (1987+, 6 covariates) 
  #dropping wind/chla (only 1995+), and pcod (correlated w/ salmon)

# Remove Year and highly correlated covariates
dat.mod1 <- dat.fit %>% 
  dplyr::select(-c(wind_lag, cod_lag, chla_lag)) %>%
  arrange(by_group=YEAR) %>%
  rename(`Cold pool`="cp_lag", `Arctic Oscillation`="ao_lag", `Bottom Temperature`=temp_lag,
  `Sockeye Run Size`="salmon_lag", `Benthic Invert Density`="invert_lag", `pH`="ph_lag")

#Trial LM
temp.lm <- lm(ln_rec ~ ., data=dat.mod1)
summary(temp.lm)
  #Plot
coefplot::coefplot(temp.lm)

# Bayesian Model Selection
bas.m1 <-  bas.lm(ln_rec ~ ., data=dat.mod1[,-c(1)],
                  # prior="ZS-null",
                  modelprior=uniform(), initprobs="Uniform",
                  method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas.m1)

plot(bas.m1, which = 4, ask=FALSE, caption="", sub.caption="")
plot(coef(bas.m1),  ask=FALSE)
plot.bas(bas.m1)

###MODEL 1 PLOTS ==========

# Plot Model Predictions vs. Observed 
pdf(file.path(dir.figs,"Model 1 Fit.pdf"), height=5, width=10)
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
pred.bas <- predict(bas.m1, estimator="BMA")

# Omit NAs
dat.mod1.na.omit <- na.omit(dat.mod1)

plot(x=dat.mod1.na.omit$ln_rec, y=pred.bas$Ybma,
     xlab="Observed ln(Recruitment)", ylab="Predicted ln(Recruitment)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
# Title
mtext(paste("BBRKC Model 1", model), side=3, outer=TRUE, font=2)
# plot(x=pred.bas$fit, y=pred.bas$Ybma) 
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3)

# Timeseries
plot(x=dat.mod1.na.omit$YEAR, y=dat.mod1.na.omit$ln_rec,
     xlab="Year", ylab="ln(Recruitment)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.mod1.na.omit$YEAR, y=dat.mod1.na.omit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.mod1.na.omit$YEAR, y=pred.bas$Ybma, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('bottomleft', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
                                                                  rgb(0,0,1, alpha=0.5)),
       bg="white")

dev.off()

#Plot Inclusion Probabilities 

inc.probs <- summary(bas.m1)[2:ncol(dat.mod1),1]
bas.names <- coef(bas.m1)$namesx
inc.probs <- coef(bas.m1)$probne0
post.mean <- coef(bas.m1)$postmean
post.sd <- coef(bas.m1)$postsd

#Calculate lower and upper 95% CI
low.95 <- post.mean - 1.96*post.sd
up.95 <- post.mean + 1.96*post.sd

cond.mean <- coef(bas.lm)$conditionalmean[,2]
cond.sd <- coef(bas.lm)$conditionalsd

names(coef(bas.lm))

#Plot Effect Sizes
plot.df <- data.frame(bas.names, inc.probs, post.mean, post.sd, low.95, up.95)

g <- ggplot(filter(plot.df, bas.names!='Intercept'),
            aes(x=bas.names, post.mean, fill='blue')) +
  theme_bw() +
  geom_errorbar(aes(ymin=post.mean-post.sd, ymax=post.mean+post.sd), width=0.25) +
  geom_point(pch=21, fill='blue', size=3) +
  geom_hline(yintercept = 0, col='red', alpha=0.5) +
  ylab('Effect') +
  xlab('Covariate') +
  coord_flip() +
  theme(legend.position='none')
g

#Plot Inclusion Probabilities
g2 <-  ggplot(filter(plot.df, bas.names!='Intercept'),
              aes(x=bas.names, y=inc.probs, fill=inc.probs)) +
  theme_bw() +
  geom_bar(stat='identity', color='black') +
  ylab('Inclusion\nProbability') +
  # coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  geom_hline(yintercept=c(0,1)) +
  theme(legend.position='none', axis.text.y = element_blank(), 
        axis.title.y=element_blank()) +
  coord_flip() +
  scale_fill_continuous_tableau()
g2

# Bring Figs Together ========
g3 <- plot_grid(g,g2, nrow=1, ncol=2, rel_widths=c(3,1), align='h')
ggsave(file=file.path(dir.figs,"Model 1 Inclusion Prob.png"), plot=g3, height=5, width=8, units='in',
       dpi=500)

#=============================================================
#### MODEL RUN 2: Short timeseries, all covariates (2005+, 8 covariates) 
  #but dropping pcod (correlated w/ salmon)

# Remove pcod
dat.mod2 <- dat.fit %>% 
  dplyr::select(-c(cod_lag)) %>%
  arrange(by_group=YEAR) %>%
  rename(`Cold pool`="cp_lag", `Arctic Oscillation`="ao_lag", `Bottom Temperature`=temp_lag,
         `Sockeye Run Size`="salmon_lag", `Benthic Invert Density`="invert_lag", `pH`="ph_lag",
         `Wind Stress`="wind_lag",`Chl-a`="chla_lag")

# Bayesian Model Selection
bas.m2 <-  bas.lm(ln_rec ~ ., data=dat.mod2[,-c(1)],
                  # prior="ZS-null",
                  modelprior=uniform(), initprobs="Uniform",
                  method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas.m2)

plot(bas.m2, which = 4, ask=FALSE, caption="", sub.caption="")
plot(coef(bas.m2),  ask=FALSE)

###MODEL 2 PLOTS ==========

# Plot Model Predictions vs. Observed 
pdf(file.path(dir.figs,"Model 2 Fit.pdf"), height=5, width=10)
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
pred.bas <- predict(bas.m2, estimator="BMA")

# Omit NAs
dat.mod2.na.omit <- na.omit(dat.mod2)

plot(x=dat.mod2.na.omit$ln_rec, y=pred.bas$Ybma,
     xlab="Observed ln(Recruitment)", ylab="Predicted ln(Recruitment)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
# Title
mtext(paste("BBRKC Model 2", model), side=3, outer=TRUE, font=2)
# plot(x=pred.bas$fit, y=pred.bas$Ybma) 
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3)

# Timeseries
plot(x=dat.mod2.na.omit$YEAR, y=dat.mod2.na.omit$ln_rec,
     xlab="Year", ylab="ln(Recruitment)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.mod2.na.omit$YEAR, y=dat.mod2.na.omit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.mod2.na.omit$YEAR, y=pred.bas$Ybma, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('bottomleft', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
                                                                    rgb(0,0,1, alpha=0.5)),
       bg="white")

dev.off()

#Plot Inclusion Probabilities 

inc.probs <- summary(bas.m2)[2:ncol(dat.mod1),1]
bas.names <- coef(bas.m2)$namesx
inc.probs <- coef(bas.m2)$probne0
post.mean <- coef(bas.m2)$postmean
post.sd <- coef(bas.m2)$postsd

#Calculate lower and upper 95% CI
low.95 <- post.mean - 1.96*post.sd
up.95 <- post.mean + 1.96*post.sd

cond.mean <- coef(bas.m2)$conditionalmean[,2]
cond.sd <- coef(bas.m2)$conditionalsd

names(coef(bas.m2))

#Plot Effect Sizes
plot.df <- data.frame(bas.names, inc.probs, post.mean, post.sd, low.95, up.95)

g <- ggplot(filter(plot.df, bas.names!='Intercept'),
            aes(x=bas.names, post.mean, fill='blue')) +
  theme_bw() +
  geom_errorbar(aes(ymin=post.mean-post.sd, ymax=post.mean+post.sd), width=0.25) +
  geom_point(pch=21, fill='blue', size=3) +
  geom_hline(yintercept = 0, col='red', alpha=0.5) +
  ylab('Effect') +
  xlab('Covariate') +
  coord_flip() +
  theme(legend.position='none')
g

#Plot Inclusion Probabilities
g2 <-  ggplot(filter(plot.df, bas.names!='Intercept'),
              aes(x=bas.names, y=inc.probs, fill=inc.probs)) +
  theme_bw() +
  geom_bar(stat='identity', color='black') +
  ylab('Inclusion\nProbability') +
  # coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  geom_hline(yintercept=c(0,1)) +
  theme(legend.position='none', axis.text.y = element_blank(), 
        axis.title.y=element_blank()) +
  coord_flip() +
  scale_fill_continuous_tableau()
g2

# Bring Figs Together ========
g3 <- plot_grid(g,g2, nrow=1, ncol=2, rel_widths=c(3,1), align='h')
ggsave(file=file.path(dir.figs,"Model 2 Inclusion Prob.png"), plot=g3, height=5, width=8, units='in',
       dpi=500)

#=============================================================
#### MODEL RUN 3: Short timeseries, model exploration (2005+, 6 covariates) 
  #are chla and wind only driving good fit w/ short timeseries, or are results robust?
  #dropping chla, wind, and pcod (correlated w/ salmon)

# Remove pcod, chla and wind, subset years
dat.mod3 <- dat.fit %>% 
  dplyr::select(-c(cod_lag, wind_lag, chla_lag)) %>%
  filter(YEAR > 2004) %>%
  arrange(by_group=YEAR) %>%
  rename(`Cold pool`="cp_lag", `Arctic Oscillation`="ao_lag", `Bottom Temperature`=temp_lag,
         `Sockeye Run Size`="salmon_lag", `Benthic Invert Density`="invert_lag", `pH`="ph_lag")

# Bayesian Model Selection
bas.m3 <-  bas.lm(ln_rec ~ ., data=dat.mod3[,-c(1)],
                  # prior="ZS-null",
                  modelprior=uniform(), initprobs="Uniform",
                  method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas.m3)

plot(bas.m3, which = 4, ask=FALSE, caption="", sub.caption="")
plot(coef(bas.m3),  ask=FALSE)

###MODEL 3 PLOTS ==========

# Plot Model Predictions vs. Observed 
pdf(file.path(dir.figs,"Model 3 Fit.pdf"), height=5, width=10)
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
pred.bas <- predict(bas.m3, estimator="BMA")

# Omit NAs
dat.mod3.na.omit <- na.omit(dat.mod3)

plot(x=dat.mod3.na.omit$ln_rec, y=pred.bas$Ybma,
     xlab="Observed ln(Recruitment)", ylab="Predicted ln(Recruitment)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
# Title
mtext(paste("BBRKC Model 3", model), side=3, outer=TRUE, font=2)
# plot(x=pred.bas$fit, y=pred.bas$Ybma) 
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3)

# Timeseries
plot(x=dat.mod3.na.omit$YEAR, y=dat.mod3.na.omit$ln_rec,
     xlab="Year", ylab="ln(Recruitment)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.mod3.na.omit$YEAR, y=dat.mod3.na.omit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.mod3.na.omit$YEAR, y=pred.bas$Ybma, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('bottomleft', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
                                                                    rgb(0,0,1, alpha=0.5)),
       bg="white")

dev.off()

#Plot Inclusion Probabilities 

inc.probs <- summary(bas.m3)[2:ncol(dat.mod1),1]
bas.names <- coef(bas.m3)$namesx
inc.probs <- coef(bas.m3)$probne0
post.mean <- coef(bas.m3)$postmean
post.sd <- coef(bas.m3)$postsd

#Calculate lower and upper 95% CI
low.95 <- post.mean - 1.96*post.sd
up.95 <- post.mean + 1.96*post.sd

cond.mean <- coef(bas.m3)$conditionalmean[,2]
cond.sd <- coef(bas.m3)$conditionalsd

names(coef(bas.m3))

#Plot Effect Sizes
plot.df <- data.frame(bas.names, inc.probs, post.mean, post.sd, low.95, up.95)

g <- ggplot(filter(plot.df, bas.names!='Intercept'),
            aes(x=bas.names, post.mean, fill='blue')) +
  theme_bw() +
  geom_errorbar(aes(ymin=post.mean-post.sd, ymax=post.mean+post.sd), width=0.25) +
  geom_point(pch=21, fill='blue', size=3) +
  geom_hline(yintercept = 0, col='red', alpha=0.5) +
  ylab('Effect') +
  xlab('Covariate') +
  coord_flip() +
  theme(legend.position='none')
g

#Plot Inclusion Probabilities
g2 <-  ggplot(filter(plot.df, bas.names!='Intercept'),
              aes(x=bas.names, y=inc.probs, fill=inc.probs)) +
  theme_bw() +
  geom_bar(stat='identity', color='black') +
  ylab('Inclusion\nProbability') +
  # coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  geom_hline(yintercept=c(0,1)) +
  theme(legend.position='none', axis.text.y = element_blank(), 
        axis.title.y=element_blank()) +
  coord_flip() +
  scale_fill_continuous_tableau()
g2

# Bring Figs Together ========
g3 <- plot_grid(g,g2, nrow=1, ncol=2, rel_widths=c(3,1), align='h')
ggsave(file=file.path(dir.figs,"Model 3 Inclusion Prob.png"), plot=g3, height=5, width=8, units='in',
       dpi=500)

#Results do not appear robust to shifting reference period- how to provide 
  #management advice under non-stationarity?! 
