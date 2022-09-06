# notes ----
# Calculate length:weight regressions and pre-recruit residuals for BBRKC 
# Erin Fedewa
# last updated: 2021/8/26


#########NOTE: This script uses haul data but BBRKC specimen table is needed- not updated in 2021 

# load ----
library(tidyverse)

# data ----

df1 <- read.csv("./Data/crabhaul_rkc.csv")

#subset for crab with weights 
  df1 %>%
    mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
    filter(WEIGHT != "NA" & WEIGHT >0) -> rkc

#look at the distribution
hist(rkc$WEIGHT)
hist(log(rkc$WEIGHT)) # log transformed - better

#BBRKC Male L:W regression 

#Plots
rkc %>%
  mutate(loglength = log(LENGTH_1MM),
          logweight = log(WEIGHT)) %>%
      filter(SEX == 1, SHELL_CONDITION == 2) -> male #Only SC2 as to not bias for weight of epibionts 

ggplot(male, aes(x = LENGTH_1MM, y = WEIGHT, group =YEAR)) +
      geom_point(aes(colour = factor(YEAR))) #two outliers

ggplot(male, aes(x = loglength, y = logweight, group = YEAR )) +
  geom_point(aes(colour = factor(YEAR)))

  #Just pre-recruits
male %>%
  filter(LENGTH_1MM %in% (110:134)) %>% 
  ggplot(aes(x = loglength, y = logweight, group = YEAR)) +
  geom_point(aes(colour = factor(YEAR))) +
  geom_line(aes(y = predict(lm(logweight~loglength)))) +
  facet_wrap(~YEAR) #Very low sample size for some years!
  
#Linear model (natural log of power function W = a * L^b) ----
  #See http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf

fit1 <- lm(logweight~loglength, data=male) 
  plot(fit1)
  summary(fit1)
  coef(fit1)
  
#Addressing outliers 
plot(cooks.distance(fit1), pch="*", cex=2, main="Influential Obs by Cook's distance") 
     abline(h = 4/(nrow(male)), col="red")  # add cutoff line (critical Cooks D > 4/n)
     
male$cd <- cooks.distance(fit1)
  
keepers<-subset(male, cd < (4/(nrow(male)))) 
  nrow(male) - nrow(keepers) #3 observations removed 
  
ggplot(keepers, aes(x = LENGTH_1MM, y = WEIGHT, group = YEAR)) +
    geom_point(aes(colour = factor(YEAR)))

#Re-fit model with outliers removed 
fit2 <- lm(logweight~loglength, data=keepers) 
  plot(fit2)
  summary(fit2)
  coef(fit2)
  
#SC2 Male RKC best-fit equations:
  # log(W) = -7.742947   + 3.127607  * log(L) on transformed scale
  # W = exp(-7.742947)*L^(3.127607 )  on original scale 
   
#L:W residual calculations for males ----
  
#All SC2 males
keepers %>% 
    mutate(resid = residuals(lm(logweight~loglength))) %>%
    group_by(YEAR) %>%
    summarise(Avg_resid = mean(resid)) %>%
    ggplot(aes(YEAR, Avg_resid)) +
    geom_bar(stat = "identity")
 
#Only pre-recruits
keepers %>% 
    mutate(resid = residuals(lm(logweight~loglength))) %>%
    filter(LENGTH_1MM %in% (110:134)) %>%
    group_by(YEAR) %>%
    summarise(Avg_resid = mean(resid)) %>%
    ggplot(aes(YEAR, Avg_resid)) +
      geom_bar(stat = "identity")
  
#Second method - 
  keepers %>%
    filter(LENGTH_1MM %in% (110:134)) %>%
    group_by(YEAR) %>%
    summarise(loglength = log(median(LENGTH)),
              logweight = log(median(WEIGHT))) %>%
    mutate(fitted_weight = predict(fit2, newdata = .),
           diff = logweight - fitted_weight) %>%
    ggplot(aes(YEAR, diff)) +
      geom_bar(stat = "identity")
  
  
#Condition factor K for male SC2 RKC ----
  keepers %>%
    filter(SEX == 1, SHELL_CONDITION == 2) %>%
    mutate(K=WEIGHT/(LENGTH_1MM^3)*100000) %>%
    group_by(YEAR) %>%
    summarise(Avg_cond = mean(K)) %>%
    ggplot(aes(as.numeric(YEAR), Avg_cond)) +
      geom_point(size=4) +
      geom_line() #Seem to follow same trends as residuals 
  
#Thoughts: Diff in L:W likely driven by molt timing/moisture content in muscle- are residuals 
  #consistent with thermal regime in that cold years molt is delayed and crab weight less? -
  #in future, could pull out temperature as a covariate 
#Also pre-recruit size class presumably includes both imm/mature crabs- are trends in L:W due to differences in gonad development? 
