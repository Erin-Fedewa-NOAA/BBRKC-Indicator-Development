#==================================================================================================
#Project Name: ECOSYSTEM AND SOCIOECONOMIC PROFILES - BBRKC
#
#Creator: Dr. Curry James Cunningham, UAF, CFOS
#Additions by Erin Fedewa
#Date: 8/27/23
#
#Purpose: To evaluate linkages between recruitment and a standard set of atmospheric, oceanographic, 
#           and biological indicators of ecosystem status for the Ecosystem and Socioeconomic Profiles (ESPs).
#
#==================================================================================================
#NOTES:
#2023 BBRKC Indicator dataset is raw data from the webservice, so requires some wrangling
#Response variable one, male survey abundance output is produced via seperate script
  #This dataset is then merged with indicator timeseries for the BAS analysis 

#2023 Follow ups- run a second BAS model with RKC recruitment output from assmt model 
#Extend available timeseries like temperature/AO etc. to 1980 so that we don't lose data when 
  #lagging 7 yrs 
  #Need to also follow up on BAS model/recruit plot- currently not running 
#==================================================================================================
#TIMING:
#Initial run May 2019, model run by Curry
#Follow up runs with new indicators/modeled rec response in Sept 2023, model run by Erin
##==================================================================================================
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
#### Define Directory Structure ####
wd <- getwd()

dir.data <- file.path(wd,"Data")
dir.output <- file.path(wd,"Output")
dir.figs <- file.path(wd, "Figs")
#=============================================================
#### Control Section ####

fit <- TRUE

offset <- 0

#Define Model Name
model <- "BAS_Sep_2023" # Fall 2023 BBRKC Crab ESP Report Card


#Update location references for figs and outputs
dir.output <- file.path(dir.output, model)
dir.create(dir.output, recursive=TRUE)
dir.figs <- file.path(dir.figs, model)
dir.create(dir.figs, recursive=TRUE)

#For Data
if(model=="BAS_Sep_2023") {
  years <- c(1988:2019,2022:2023)
  n.years <- length(years)
}

if(model!="BAS_Sep_2023") {
  years <- NULL
  n.years <- NULL
  stop(paste("WRONG model:", model))
}
#Wheter to do initial exploratory plots
do.initPlot <- TRUE

#Remove Correlated Covariates:
# rem.cor.cov <- FALSE

# Plotting Fxns ========================
q.50 <- function(x) { return(quantile(x, probs=c(0.25,0.75))) }
q.95 <- function(x) { return(quantile(x, probs=c(0.025,0.975))) }

q_0.025 <- function(x) { return(quantile(x, probs=0.025)) }
q_0.975 <- function(x) { return(quantile(x, probs=0.975)) }

#=============================================================
#### MODEL RUN 1: Using BT survey estimate for male recruitment as response

#Read Indicator Data and response output 
dat <- read.csv("./Data/bbrkc_2023_indicators.csv")
r_survey <- read.csv("./Output/BAS_survey_recruit.csv")

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
#pH, chla and sockeye are the shortest. Let's do a model run with all for now 
#We'll also drop any spatial distribution indicators, as these are not drivers of recruitment 

#And plot timeseries with all covariates 
rkcindic %>%
  pivot_longer(c(2:15), names_to="indicator", values_to="value") %>%
  ggplot(aes(YEAR, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~indicator, scales = "free_y") +
  theme_bw()

#Assign Lags for indicators - see metadata file in repo for rationales for lags
rkcindic %>%
  select(-c(Annual_Red_King_Crab_Catch_Distance_Shore_BBRKC_Fishery,
            Annual_Red_King_Crab_Recruit_Abundance_BBRKC_Survey,
            Summer_Red_King_Crab_Female_Area_Occupied_BBRKC_Model,
            Summer_Red_King_Crab_Male_Area_Occupied_BBRKC_Model,
            Spring_Corrosivity_Index_BBRKC_Model)) %>%
  mutate(ao_lag = lag(Winter_Spring_Arctic_Oscillation_Index_Model, n=7, order_by = YEAR),
         wind_lag = lag(Summer_Wind_Stress_BBRKC_Satellite, n=7, order_by = YEAR),
         temp_lag = lag(Summer_Temperature_Bottom_BBRKC_Survey, n=6, order_by = YEAR),
         salmon_lag = lag(Summer_Sockeye_Salmon_Abundance_EBS_Survey, n=7, order_by = YEAR),
         chla_lag = lag(Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite, n=7, order_by = YEAR),
         ph_lag = lag(Spring_pH_BBRKC_Model, n=6, order_by = YEAR),
         cod_lag = lag(Summer_Pacific_Cod_Density_BBRKC_Survey, n=1, order_by = YEAR),
         invert_lag = lag(Summer_Benthic_Invertebrate_Density_BBRKC_Survey, n=1, order_by = YEAR),
         cp_lag = lag(Summer_Cold_Pool_SEBS_BBRKC_Survey, n=2, order_by = YEAR)) %>%
  select(-c(Winter_Spring_Arctic_Oscillation_Index_Model, Summer_Wind_Stress_BBRKC_Satellite, 
         Summer_Temperature_Bottom_BBRKC_Survey, Summer_Sockeye_Salmon_Abundance_EBS_Survey,
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
#with so many large ELH lags, we're going to lose early years in the timeseries 

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
hist(dat1$salmon_lag)
hist(dat1$invert_lag)

#Determine Covariates
if(model=="BAS_Sep_2023") {
  covars <- names(dat1)[-which(names(rkcindic) %in% c("YEAR", "imm_survey_abun"))]
}
n.cov <- length(covars)

# Calculate Log Recruitment ===================================
dat.2 <- dat1 %>% 
  mutate("ln_rec"=log(imm_survey_abun))

# Log transform biomass predictors ============================
hist(log(dat.2$salmon_lag))
hist(log(dat.2$invert_lag))

if(model=="BAS_Sep_2023") {
  dat.2$salmon_lag <- log(dat.2$salmon_lag)
  dat.2$invert_lag <- log(dat.2$invert_lag)
}

# Limit Years =================================================
#dat.3 <- dat.2 %>% 
  #filter(year %in% years)

# Standardize Covariates ======================================
# Plot Covariates
covar.list <- dat.2 %>% 
  dplyr::select(-c("imm_survey_abun","ln_rec")) %>% 
  gather(key=type, value=value, -YEAR) 
head(covar.list)

explore.hist <- ggplot(covar.list, aes(x=value, fill=type)) +
  theme_linedraw() +
  geom_histogram() +
  geom_density(alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~type, scales='free') +
  theme(legend.position = "NA")

ggsave(file.path(dir.figs,"Covar Histogram_M1.png"), plot=explore.hist, 
       height=8, width=12, units='in')

# Z-score Predictors that are bounded at zero =======================================
dat.4 <- dat.2
c <- 1
for(c in 1:n.cov) {
  dat.4[[covars[c]]] <- (dat.4[[covars[c]]] - mean(dat.4[[covars[c]]], na.rm=TRUE)) / sd(dat.4[[covars[c]]], na.rm=TRUE)
}

# Checkup - make sure all predictors are correctly z-scored
apply(dat.4, 2, mean, na.rm=TRUE)
apply(dat.4, 2, sd, na.rm=TRUE)
# }

# Subset Data for Fitting =====================================
if(model=="BAS_Sep_2023") {
  dat.fit <- dat.4 %>% dplyr::select(-c("imm_survey_abun"))
  dat.fit.list <- dat.fit %>% gather(key='var', value='value', -YEAR)
}  

#Plot Timeseries
if(do.initPlot==TRUE) {
  g <- dat.fit.list %>% filter(var!="ln_rec") %>% 
    ggplot(aes(x=YEAR, y=var, fill=value)) +
    theme_linedraw() +
    # geom_point()
    geom_point(aes(cex=value), alpha=0.5, pch=21, color='black') +
    scale_fill_viridis_c() +
    ggtitle("Standardized Covariate Values")
  g
  ggsave(file.path(dir.figs,"Standardized Covariates_M1.png"), plot=g, height=6, width=10, units='in', dpi=500)
  
  # Correlation Plot
  covar.mtx <- dat.fit %>% 
    dplyr::select(-c("YEAR"))
  
  corr.mtx <- cor(covar.mtx, use="na.or.complete")
  png(file.path(dir.figs, "Covariate Correlation_M1.png"), height=12, width=12, 
      units='in', res=300)
  corrplot::corrplot(corr.mtx, method="number")
  dev.off()
}

#Fairly high correlations b/w cod biomass and wind but no clear mechanism? Lets
  #drop wind b/c shorter timeseries
#Let's also drop salmon b/c byfar shortest timeseries 

#Fit Models ====================================

# Remove Year and highly correlated covariates
dat.temp <- dat.fit %>% 
  dplyr::select(-c(wind_lag, salmon_lag)) %>%
  arrange(by_group=YEAR) %>%
  rename(`Cold pool`="cp_lag", `Arctic Oscillation`="ao_lag", `Bottom Temperature`=temp_lag,
  `Pcod Density`="cod_lag", `Benthic Invert Density`="invert_lag", `pH`="ph_lag",
  `Chla Concentration`="chla_lag")

#Trial LM
temp.lm <- lm(ln_rec ~ ., data=dat.temp)
summary(temp.lm)
  #Plot
coefplot::coefplot(temp.lm)

# Bayesian Model Selection
bas.lm <-  bas.lm(ln_rec ~ ., data=dat.temp[,-c(1)],
                  # prior="ZS-null",
                  modelprior=uniform(), initprobs="Uniform",
                  method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas.lm)

plot(bas.lm, which = 4, ask=FALSE, caption="", sub.caption="")
plot(coef(bas.lm),  ask=FALSE)
plot(bas.lm, which=4)

# Plot Model Predictions vs. Observed ==============================
pdf(file.path(dir.figs,"Model Fit_1.pdf"), height=5, width=10)
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
pred.bas <- predict(bas.lm, estimator="BMA")

# Omit NAs
dat.temp.na.omit <- na.omit(dat.temp)

plot(x=dat.temp.na.omit$ln_rec, y=pred.bas$Ybma,
     xlab="Observed ln(Recruitment)", ylab="Predicted ln(Recruitment)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
# Title
mtext(paste("BBRKC", model), side=3, outer=TRUE, font=2)
# plot(x=pred.bas$fit, y=pred.bas$Ybma) 
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3)

# Timeseries
plot(x=dat.temp.na.omit$YEAR, y=dat.temp.na.omit$ln_rec,
     xlab="Year", ylab="ln(Recruitment)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.temp.na.omit$YEAR, y=dat.temp.na.omit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.temp.na.omit$YEAR, y=pred.bas$Ybma, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('bottomleft', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
                                                                  rgb(0,0,1, alpha=0.5)),
       bg="white")

dev.off()


# bas.lm.2 <-  bas.lm(ln_rec ~ ., data=dat.temp,
#                     # prior="ZS-null",
#                     modelprior=uniform(), initprobs="Uniform",
#                     method='MCMC', MCMC.iterations=1e6, thin=10)


# PLOT RESULTS ==================================================
names(summary(bas.lm))

inc.probs <- summary(bas.lm)[2:ncol(dat.temp),1]
# par(oma=c(1,1,1,1), mar=c(4,20,1,1))
# barplot(inc.probs, horiz=TRUE, xlim=c(0,1), las=2)
# abline(v=seq(from=0.2, to=0.8, by=0.2), lty=2)
# box()

bas.names <- coef(bas.lm)$namesx
inc.probs <- coef(bas.lm)$probne0
post.mean <- coef(bas.lm)$postmean
post.sd <- coef(bas.lm)$postsd
#Calcualte lower and upper 95% CI
low.95 <- post.mean - 1.96*post.sd
up.95 <- post.mean + 1.96*post.sd

# confint(coef(bas.lm), level=c(0.5))
# post.probs <- coef(bas.lm)$postprobs

cond.mean <- coef(bas.lm)$conditionalmean[,2]
cond.sd <- coef(bas.lm)$conditionalsd

names(coef(bas.lm))


#Plot it out....
par(mfrow=c(1,2), mar=c(4,1,2,1), oma=c(0,10,1,1))

plot.df <- data.frame(bas.names, inc.probs, post.mean, post.sd, low.95, up.95)
# plot.list <- melt(plot.df)

# g <- ggplot(filter(plot.df, bas.names!='Intercept'),
#             aes(x=bas.names, post.mean, fill=bas.names)) +
#        theme_linedraw() +
#        geom_errorbar(aes(ymin=low.95, ymax=up.95), width=0.25) +
#        geom_point(pch=21) +
#        geom_hline(yintercept = 0, col='red', alpha=0.5) +
#        ylab('Effect') +
#        xlab('Covariate') +
#        coord_flip() +
#        theme(legend.position='none')
# 
# g

g <- ggplot(filter(plot.df, bas.names!='Intercept'),
            aes(x=bas.names, post.mean, fill=bas.names)) +
  theme_bw() +
  geom_errorbar(aes(ymin=post.mean-post.sd, ymax=post.mean+post.sd), width=0.25) +
  geom_point(pch=21, size=3) +
  geom_hline(yintercept = 0, col='red', alpha=0.5) +
  ylab('Effect') +
  xlab('Covariate') +
  coord_flip() +
  theme(legend.position='none')
g

#Inclusion prob

g2 <-  ggplot(filter(plot.df, bas.names!='Intercept'),
              aes(x=bas.names, y=inc.probs, fill=bas.names)) +
  theme_bw() +
  geom_bar(stat='identity', color='black') +
  ylab('Inclusion\nProbability') +
  # coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  geom_hline(yintercept=c(0,1)) +
  theme(legend.position='none', axis.text.y = element_blank(), 
        axis.title.y=element_blank()) +
  coord_flip()
# scale_fill_continuous()
g2

# Bring Figs Together ========
g3 <- plot_grid(g,g2, nrow=1, ncol=2, rel_widths=c(3,1), align='h')
ggsave(file=file.path(dir.figs,"BAS.png"), plot=g3, height=5, width=8, units='in',
       dpi=500)

#PLOT OUTPUT WITHOUT RAINBOW ===========
g.b <- ggplot(filter(plot.df, bas.names!='Intercept'),
              aes(x=bas.names, post.mean, fill='blue')) +
  theme_bw() +
  geom_errorbar(aes(ymin=post.mean-post.sd, ymax=post.mean+post.sd), width=0.25) +
  geom_point(pch=21, fill='blue', size=3) +
  geom_hline(yintercept = 0, col='red', alpha=0.5) +
  ylab('Effect') +
  xlab('Covariate') +
  coord_flip() +
  theme(legend.position='none')
g.b

#Inclusion prob

g2.b <-  ggplot(filter(plot.df, bas.names!='Intercept'),
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
g2.b

# Bring Figs Together ========
g3.b <- plot_grid(g.b,g2.b, nrow=1, ncol=2, rel_widths=c(3,1), align='h')
ggsave(file=file.path(dir.figs,"BAS_noRainbow.png"), plot=g3.b, height=5, width=8, units='in',
       dpi=500)

#######################################################
#Model run with longer timeseries (only difference here is pulling chla)
  #Previous run: 2005+ and this run 1988+

# Remove Year, highly correlated covariates and short timeseries 
dat.temp.2 <- dat.fit %>% 
  dplyr::select(-c(wind_lag, salmon_lag, chla_lag)) %>%
  arrange(by_group=YEAR) %>%
  rename(`Cold pool`="cp_lag", `Arctic Oscillation`="ao_lag", `Bottom Temperature`=temp_lag,
         `Pcod Density`="cod_lag", `Benthic Invert Density`="invert_lag", `pH`="ph_lag")

# Bayesian Model Selection
bas.lm.2 <-  bas.lm(ln_rec ~ ., data=dat.temp.2[,-c(1)],
                  # prior="ZS-null",
                  modelprior=uniform(), initprobs="Uniform",
                  method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas.lm.2)

plot(bas.lm.2, which = 4, ask=FALSE, caption="", sub.caption="")
plot(coef(bas.lm.2),  ask=FALSE)
plot(bas.lm.2, which=4)

# Plot Model Predictions vs. Observed ==============================
pdf(file.path(dir.figs,"Model Fit_m2.pdf"), height=5, width=10)
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
pred.bas <- predict(bas.lm.2, estimator="BMA")

# Omit NAs
dat.temp.na.omit <- na.omit(dat.temp.2)

plot(x=dat.temp.na.omit$ln_rec, y=pred.bas$Ybma,
     xlab="Observed ln(Recruitment)", ylab="Predicted ln(Recruitment)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
# Title
mtext(paste("BBRKC", model), side=3, outer=TRUE, font=2)
# plot(x=pred.bas$fit, y=pred.bas$Ybma) 
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3)

# Timeseries
plot(x=dat.temp.na.omit$YEAR, y=dat.temp.na.omit$ln_rec,
     xlab="Year", ylab="ln(Recruitment)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.temp.na.omit$YEAR, y=dat.temp.na.omit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.temp.na.omit$YEAR, y=pred.bas$Ybma, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('topright', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
                                                                rgb(0,0,1, alpha=0.5)),
       bg="white")

dev.off()


# PLOT RESULTS ==================================================
names(summary(bas.lm.2))

inc.probs <- summary(bas.lm.2)[2:ncol(dat.temp),1]
# par(oma=c(1,1,1,1), mar=c(4,20,1,1))
# barplot(inc.probs, horiz=TRUE, xlim=c(0,1), las=2)
# abline(v=seq(from=0.2, to=0.8, by=0.2), lty=2)
# box()

bas.names <- coef(bas.lm.2)$namesx
inc.probs <- coef(bas.lm.2)$probne0
post.mean <- coef(bas.lm.2)$postmean
post.sd <- coef(bas.lm.2)$postsd
#Calcualte lower and upper 95% CI
low.95 <- post.mean - 1.96*post.sd
up.95 <- post.mean + 1.96*post.sd

# confint(coef(bas.lm), level=c(0.5))
# post.probs <- coef(bas.lm)$postprobs

cond.mean <- coef(bas.lm.2)$conditionalmean[,2]
cond.sd <- coef(bas.lm.2)$conditionalsd

names(coef(bas.lm.2))


#Plot it out....
par(mfrow=c(1,2), mar=c(4,1,2,1), oma=c(0,10,1,1))

plot.df <- data.frame(bas.names, inc.probs, post.mean, post.sd, low.95, up.95)
# plot.list <- melt(plot.df)

# g <- ggplot(filter(plot.df, bas.names!='Intercept'),
#             aes(x=bas.names, post.mean, fill=bas.names)) +
#        theme_linedraw() +
#        geom_errorbar(aes(ymin=low.95, ymax=up.95), width=0.25) +
#        geom_point(pch=21) +
#        geom_hline(yintercept = 0, col='red', alpha=0.5) +
#        ylab('Effect') +
#        xlab('Covariate') +
#        coord_flip() +
#        theme(legend.position='none')
# 
# g

g <- ggplot(filter(plot.df, bas.names!='Intercept'),
            aes(x=bas.names, post.mean, fill=bas.names)) +
  theme_bw() +
  geom_errorbar(aes(ymin=post.mean-post.sd, ymax=post.mean+post.sd), width=0.25) +
  geom_point(pch=21, size=3) +
  geom_hline(yintercept = 0, col='red', alpha=0.5) +
  ylab('Effect') +
  xlab('Covariate') +
  coord_flip() +
  theme(legend.position='none')
g

#Inclusion prob

g2 <-  ggplot(filter(plot.df, bas.names!='Intercept'),
              aes(x=bas.names, y=inc.probs, fill=bas.names)) +
  theme_bw() +
  geom_bar(stat='identity', color='black') +
  ylab('Inclusion\nProbability') +
  # coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  geom_hline(yintercept=c(0,1)) +
  theme(legend.position='none', axis.text.y = element_blank(), 
        axis.title.y=element_blank()) +
  coord_flip()
# scale_fill_continuous()
g2

# Bring Figs Together ========
g3 <- plot_grid(g,g2, nrow=1, ncol=2, rel_widths=c(3,1), align='h')
ggsave(file=file.path(dir.figs,"BAS_M2.png"), plot=g3, height=5, width=8, units='in',
       dpi=500)

#PLOT OUTPUT WITHOUT RAINBOW ===========
g.b <- ggplot(filter(plot.df, bas.names!='Intercept'),
              aes(x=bas.names, post.mean, fill='blue')) +
  theme_bw() +
  geom_errorbar(aes(ymin=post.mean-post.sd, ymax=post.mean+post.sd), width=0.25) +
  geom_point(pch=21, fill='blue', size=3) +
  geom_hline(yintercept = 0, col='red', alpha=0.5) +
  ylab('Effect') +
  xlab('Covariate') +
  coord_flip() +
  theme(legend.position='none')
g.b

#Inclusion prob

g2.b <-  ggplot(filter(plot.df, bas.names!='Intercept'),
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
g2.b

# Bring Figs Together ========
g3.b <- plot_grid(g.b,g2.b, nrow=1, ncol=2, rel_widths=c(3,1), align='h')
ggsave(file=file.path(dir.figs,"BAS_noRainbow_M2.png"), plot=g3.b, height=5, width=8, units='in',
       dpi=500)

