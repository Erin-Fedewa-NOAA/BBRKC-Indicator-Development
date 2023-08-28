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
  years <- c(1989:2019,2022:2023)
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
#### MODEL RUN 1: Using design-based BT survey estimate for male recruitment as response

#Read Indicator Data and response output 
dat <- read.csv("./Data/bbrkc_2023_indicators.csv")
r_survey <- read.csv("./Output/")

#Data wrangling of webservice indicator data 
dat %>%
  select(YEAR, INDICATOR_NAME, DATA_VALUE, INDICATOR_TYPE) %>%
  filter(INDICATOR_TYPE != "Socioeconomic") %>%
  select(-INDICATOR_TYPE) %>%
  pivot_wider(names_from="INDICATOR_NAME", values_from="DATA_VALUE") %>%
  #add in response variable, "imm_abun"


#Look at temporal coverage of data- drop chla and salmon? 

#Assign Lags for indicators - see metadata file in repo for rationales for lags
dat %>%
  filter(year>1988) %>%
  mutate(ao_lag = lag(Winter_Spring_Arctic_Oscillation_Index_Model, n=3),
         wind_lag = lag(Summer_Wind_Stress_BBRKC_Satellite, n=6),
         temp_lag = lag(Summer_Temperature_Bottom_BBRKC_Survey, n=3),
         salmon_lag = lag(Summer_Sockeye_Salmon_Abundance_EBS_Survey, n=3),
         chla_lag = lag(Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite, n=3),
         ph_lag = lag(Spring_pH_BBRKC_Model, n=3),
         cod_lag = lag(Summer_Pacific_Cod_Density_BBRKC_Survey, n=3),
         invert_lag = lag(Summer_Benthic_Invertebrate_Density_BBRKC_Survey, n=3),
         cp_lag = lag(Summer_Cold_Pool_SEBS_BBRKC_Survey, n=3)) %>%
  select(-cp_extent, -Mean_AO, -Jan_ice, -Pcod_consumption) -> dat1

#Determine Covariates
if(model=="BAS_Sep_2023") {
  covars <- names(dat1)[-which(names(dat) %in% c("YEAR", "imm_abund"))]
}
n.cov <- length(covars)

# Calculate Log Recruitment ===================================
dat.2 <- dat1 %>% 
  mutate("ln_rec"=log(imm_abund))

# Log transform biomass predictors ============================
hist(log(dat.2$consump_lag))
hist(log(dat.2$bcs_imm))

if(model=="BAS_Sep_2023") {
  dat.2$consump_lag <- log(dat.2$consump_lag)
  dat.2$bcs_imm <- log(dat.2$bcs_imm)
}

# Limit Years =================================================
dat.3 <- dat.2 %>% 
  filter(year %in% years)

# Standardize Covariates ======================================
# Plot Covariates
covar.list <- dat.3 %>% 
  dplyr::select(-c("imm_abund","ln_rec")) %>% 
  gather(key=type, value=value, -year) 
head(covar.list)

explore.hist <- ggplot(covar.list, aes(x=value, fill=type)) +
  theme_linedraw() +
  geom_histogram() +
  geom_density(alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~type, scales='free') +
  theme(legend.position = "NA")

ggsave(file.path(dir.figs,"Covar Histogram.png"), plot=explore.hist, 
       height=8, width=12, units='in')

# Z-score Predictors that are bounded at zero =======================================
dat.4 <- dat.3
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
  dat.fit <- dat.4 %>% dplyr::select(-c("imm_abund"))
  dat.fit.list <- dat.fit %>% gather(key='var', value='value', -year)
}  


#Plot Timeseries
if(do.initPlot==TRUE) {
  g <- dat.fit.list %>% filter(var!="ln_rec") %>% 
    ggplot(aes(x=year, y=var, fill=value)) +
    theme_linedraw() +
    # geom_point()
    geom_point(aes(cex=value), alpha=0.5, pch=21, color='black') +
    scale_fill_viridis_c() +
    ggtitle("Standardized Covariate Values")
  g
  ggsave(file.path(dir.figs,"Standardized Covariates.png"), plot=g, height=6, width=10, units='in', dpi=500)
  
  # Correlation Plot
  covar.mtx <- dat.fit %>% 
    dplyr::select(-c("year"))
  
  corr.mtx <- cor(covar.mtx, use="na.or.complete")
  png(file.path(dir.figs, "Covariate Correlation.png"), height=12, width=12, 
      units='in', res=300)
  corrplot::corrplot(corr.mtx, method="number")
  dev.off()
}

#Fairly high correlations b/w cod consumption, sea ice, cp extent and temperature of occupancy
  #Because all 3 environmental indicators are likely capturing a similar mechanism, let's drop
  #sea ice and temperature occupancy from the model run 


#Fit Models ====================================

# Remove Year and highly correlated covariates
dat.temp <- dat.fit %>% 
  dplyr::select(-c("year", "ice_lag", "temp_occ_imm"))

#Trial LM
temp.lm <- lm(ln_rec ~ ., data=dat.temp)
summary(temp.lm)
  #Plot
coefplot::coefplot(temp.lm)

# Bayesian Model Selection
bas.lm <-  bas.lm(ln_rec ~ ., data=dat.temp,
                  # prior="ZS-null",
                  modelprior=uniform(), initprobs="Uniform",
                  method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas.lm)

plot(bas.lm, which = 4, ask=FALSE, caption="", sub.caption="")
plot(coef(bas.lm),  ask=FALSE)
plot(bas.lm, which=4)

# Plot Model Predictions vs. Observed ==============================
pdf(file.path(dir.figs,"Model Fit.pdf"), height=5, width=10)
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
pred.bas <- predict(bas.lm, estimator="BMA")

# Omit NAs
dat.temp.na.omit <- na.omit(dat.fit)

plot(x=dat.temp.na.omit$ln_rec, y=pred.bas$Ybma,
     xlab="Observed ln(Recruitment)", ylab="Predicted ln(Recruitment)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
# Title
mtext(paste("Snow Crab", model), side=3, outer=TRUE, font=2)
# plot(x=pred.bas$fit, y=pred.bas$Ybma) 
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3)

# Timeseries
plot(x=dat.temp.na.omit$year, y=dat.temp.na.omit$ln_rec,
     xlab="Year", ylab="ln(Recruitment)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.temp.na.omit$year, y=dat.temp.na.omit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.temp.na.omit$year, y=pred.bas$Ybma, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('bottom', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
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


# Exploration with Boosted Regression Trees =========================================
form.covars <- paste(covars, collapse=" + ")
form <- formula(paste("ln_rec", "~",form.covars))

gbm.fit <- gbm(formula=form, distribution = "gaussian", data=dat.fit, 
                 n.trees=1e5, interaction.depth=1,
                 shrinkage=0.001,
                 n.minobsinnode=3,
                 train.fraction=0.5)

summary(gbm.fit, las=2)

# Plot Fit
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
# pred.bas <- predict(bas.lm, estimator="BMA")

plot(x=dat.fit$ln_rec, y=gbm.fit$fit,
     xlab="Observed ln(MMB)", ylab="Predicted ln(MMB)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
mtext(paste("BBRKC", model), side=3, outer=TRUE, font=2)
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3)

# Timeseries
plot(x=dat.fit$Year, y=dat.fit$ln_rec,
     xlab="Year", ylab="ln(MMB)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.fit$Year, y=dat.fit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.fit$Year, y=gbm.fit$fit, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('bottom', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
                                                                rgb(0,0,1, alpha=0.5)),
       bg="white")

# Plot Partials
par(mfrow=c(2,3))

plot(gbm.fit, i.var=3)


# # Plot Fit =========
# 
# #Plot Fitted Model - POSTERIOR PREDICTIVE DISTRIBUTION
# # post.preds <- apply(out$BUGSoutput$sims.list$post.pred, 2, quantile, probs=c(0.025,0.25,0.5,0.75,0.975))
# pdf(file.path(dir.figs,'Fits and Other Params.pdf'), height=6, width=7)
# 
# preds <- predict(bas.lm)
# 
# post.preds <- preds$Ybma
# # pred.low <- preds$Ybma - 1.96* preds$se.bma.pred
# 
# y.lim <- c(min(input.rec, post.preds), max(input.rec, post.preds))
# x.lim <- c(min(years),max(years))
# 
# plot(x=NULL, y=NULL, xlab='Recruitment Year', ylab='log(recruitment)', pch=21, bg='blue',
#      ylim=y.lim, xlim=x.lim)
# abline(h=0)
# lines(x=years, y=input.rec, lwd=2, col='blue', lty=3)
# points(x=years, y=input.rec, pch=21, bg='blue')
# 
# #Fitted Model
# # polygon(x=c(years, rev(years)), y=c(post.preds[1,],rev(post.preds[5,])), col=rgb(1,0,0,alpha=0.25), border=FALSE)
# # polygon(x=c(years, rev(years)), y=c(post.preds[2,],rev(post.preds[4,])), col=rgb(1,0,0,alpha=0.25), border=FALSE)
# lines(x=years, y=post.preds[,1], col=rgb(1,0,0,alpha=0.5), lwd=2)
# legend('topleft', legend=c('Observed','Post. Pred.'), col=c('blue','red'), lty=c(3,1))
# 
# dev.off()
# 
# 
# #Plot Model Ranks =========
# png(file.path(dir.figs, 'Model Ranks.png'), height=6, width=9, units='in', res=500)
# par(oma=c(0,18,0,0))
# image(bas.lm, rotate=F)
# 
# dev.off()
# 
