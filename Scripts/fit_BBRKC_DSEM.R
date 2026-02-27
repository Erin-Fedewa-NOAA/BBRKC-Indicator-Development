#Testing preliminary DSEM model for BBRKC

#Author: EJF

#load 
library(tidyverse)
library(dsem)
library(ggplot2)
library(dplyr)
library(dagitty)
library(ggdag)
library(knitr)
library(corrplot)

## Read in setup for crab data
source("./Scripts/get_crab_data.R")

#Read in indicator data
indicators <- read.csv("./Output/BBRKC_esp_indicator_timeseries.csv")

# Set years
current_year <- 2025
years <- 1982:current_year

############################################################
#data wrangling and some quick data exploration

# calculate abundance of immature male RKC (95-120 mm) as response for DSEM
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

#Plot response
recruit_abun %>%
  ggplot(aes(x = year, y = abundance)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#join indicator and response variables
recruit_abun %>%
  right_join(indicators) %>%
  arrange(year) %>%
  #dropping indicators that don't have a mechanistic link w/ recruitment
  #these are evaluated as contextual indicators only in the ESP process
  select(-prop_closed_area, -mean_distance_shore_km, -mature_female_d95, 
         -mature_male_d95, -bbrkc_northern_ratio) -> dsem_dat

#plot indicators
dsem_dat %>%
  pivot_longer(2:ncol(dsem_dat), names_to="variable", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw()

#Lets look at distributions of covariates
plot_histo <- function(data) {
  # Use imap to iterate over columns (values) and their names (keys)
  plots <- imap(data, ~ggplot(data, aes(x = .x)) +
                  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
                  labs(title = paste("Histogram of", .y), x = .y, y = "Frequency") +
                  theme_minimal())
  # Return the list of plots
  return(plots)
}

plot_histo(dat)

#Normalize and standardize covariates
dat %>%
  mutate(log_hybrid_abundance = log(hybrid_abundance)) %>%
  select(-hybrid_abundance) %>%
  mutate(across(c(2:6), ~ (.-mean(.,na.rm=T))/sd(.,na.rm=T), .names = "z_{.col}")) %>%
  select(-sea_ice, -snow_tanner_overlap, -snow_abundance, -tanner_abundance, -log_hybrid_abundance) %>%
  rename(sea_ice = z_sea_ice, snow_tanner_overlap = z_snow_tanner_overlap, 
         snow_abundance = z_snow_abundance,
         tanner_abundance = z_tanner_abundance,
         hybrid_abundance = z_log_hybrid_abundance) -> hybrid_data


#Assess collinearity b/w variables
#if correlated, we should have a node in our DAG below
dsem_dat %>% 
  select(-year, -abundance) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(method="number")

#Now look at distributions of normalized/standardized covariates
plot_histo(hybrid_data)
#looks much better

#plot z-scored variables
hybrid_data %>%
  pivot_longer(2:6, names_to="variable", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", nrow=3) +
  theme_bw()

#visually explore lags, though we'll base primarily on mechanistic understanding
#for lag specification in models below
ccf(hybrid_data$snow_abundance, hybrid_data$hybrid_abundance) #1-4 yr lag
ccf(hybrid_data$tanner_abundance, hybrid_data$hybrid_abundance) #0 yr lag
ccf(hybrid_data$sea_ice, hybrid_data$hybrid_abundance) #6-7 yr lag
ccf(hybrid_data$sea_ice, hybrid_data$snow_tanner_overlap)
ccf(hybrid_data$sea_ice, hybrid_data$tanner_abundance)
ccf(hybrid_data$snow_tanner_overlap, hybrid_data$hybrid_abundance) #0-1 yr lag

#########################################################
#First let's use a DAG to test for conditional independencies and validate our
#model structure 

#download specified DAG from dagitty.net - conditional independencies are 
#identified based on structure of DAG drawn in dagitty
full_dag <- dagitty('dag {
hybrid_abundance [outcome,pos="0.008,1.188"]
sea_ice [exposure,pos="-0.067,1.194"]
snow_abundance [pos="-0.027,1.121"]
snow_tanner_overlap [pos="-0.025,1.186"]
tanner_abundance [outcome,pos="-0.020,1.293"]
sea_ice -> hybrid_abundance [pos="0.004,1.254"]
sea_ice -> snow_abundance
sea_ice -> snow_tanner_overlap
sea_ice -> tanner_abundance
snow_abundance -> hybrid_abundance [pos="0.008,1.139"]
snow_abundance -> tanner_abundance [pos="-0.041,1.166"]
snow_tanner_overlap -> hybrid_abundance
tanner_abundance -> hybrid_abundance [pos="0.012,1.235"]
}
')

#plot DAG
ggdag(full_dag, layout = "nicely") +
  theme_dag()

plot(full_dag) 

ggdag_status(full_dag, text = FALSE, use_labels = "name") +
  #guides(color = "none") +  # Turn off legend
  theme_dag()

#identify paths
paths(full_dag)
#13 open causal pathways 

#and plot paths
ggdag_paths(full_dag, text = FALSE, use_labels = "name") +
  theme_dag()

#find adjustment sets for response variable 1
adjustmentSets(full_dag, exposure="sea_ice", outcome="tanner_abundance")
#This tells us that no covariate adjustment is necessary to identify the causal effect
#of marine heatwave on tanner abundance, ie there are no open backdoor paths 

#find adjustment sets for response variable 2
adjustmentSets(full_dag, exposure="sea_ice", outcome="hybrid_abundance")
#This tells us that no covariate adjustment is necessary to identify the causal effect
#of marine heatwave on tanner abundance, ie there are no open backdoor paths 

#and visualize adjustment sets, if there are any
ggdag_adjustment_set(full_dag, shadow = TRUE) +
  theme_dag()

#find conditional independencies- i.e. two variables that are implied to 
# be independent and not correlated shouldn't be connected by a node
impliedConditionalIndependencies(full_dag)
#because this is empty (ie no independencies exist, localTests()
#below will produce no results)

# evaluate the d-separation implications of our DAG with our simulated dataset
#i.e. test for correlation between conditional independencies that are assumed
#based on DAG
test <- localTests(full_dag, hybrid_data, type="cis")
#Note that this approach is not valid for our DAG b/c it does not test lags
#that we'll define in the model structure below

# perform Holm-Bonferrino correction to mitigate problems around multiple testing 
test$p.value <- p.adjust(test$p.value) 

test # should show all p values above 0.05, suggesting DAG-data consistency- aka you don't need 
#to reject DAG based on results of test
#If you fail data consistency test, you need to go back and rethink DAG - there is either a 
#common cause that you're not accounting for (or can't account for if latent), 
#or a spurious correlation with no mechanism- but we shouldn't be held to this, data shouldn't be informing DAG!

############################################
#Now since our DAG looks good, let's specify a SEM

#remove year
hybrid_data <- hybrid_data %>% select(-year)
data = ts(hybrid_data)

#define formulation: link, lag, param_name, start_value (see ?make_dsem_ram())
sem ="
#AR1 for each variable (change 1 to 0 for iid)
 #Estimate in output shows correlation (if very small, might not need AR1)
sea_ice -> sea_ice, 1, ar1 
tanner_abundance -> tanner_abundance, 1, ar2
snow_abundance -> snow_abundance, 1, ar3
snow_tanner_overlap -> snow_tanner_overlap, 1, ar4
hybrid_abundance -> hybrid_abundance, 1, ar5  

#causal links with mechanistic lags
sea_ice -> hybrid_abundance, 5, icetohybrid
sea_ice -> snow_abundance, 1, icetosnow
sea_ice -> snow_tanner_overlap, 1, icetooverlap
sea_ice -> tanner_abundance, 5, icetotanner
snow_abundance -> hybrid_abundance, 3, snowtohybrid
snow_abundance -> tanner_abundance, 3, snowtotanner
snow_tanner_overlap -> hybrid_abundance, 5, overlaptohybrid
tanner_abundance -> hybrid_abundance, 3, tannertohybrid
"
#define family
family <- rep('fixed', ncol(hybrid_data))
## family <- c('normal', 'normal', 'normal', 'fixed', 'fixed', 'fixed')

#control section
control <- dsem_control(use_REML=FALSE, run_model=TRUE,
                        getsd=TRUE, trace=100, newton_loops=0)


## initial first model run without delta0 (to improve starting values)
fit0 <- dsem(sem=sem, 
             tsdata=data, 
             family=family,
             estimate_delta0=FALSE, 
             control = dsem_control(
               quiet = FALSE,
               getsd = FALSE))

parameters = fit0$obj$env$parList()
parameters$delta0_j = rep( 0, ncol(data) )


## refit model with delta0
fit_dsem <- dsem(sem=sem, tsdata=data, 
                 family=family,
                 estimate_delta0=TRUE,
                 control=dsem_control(quiet=TRUE,
                                      parameters = parameters))

summary(fit_dsem)
fit_dsem$opt$par
ParHat = fit_dsem$obj$env$parList()
knitr::kable( summary(fit_dsem), digits=3 )

#plot dsem output
plot_fit(data, fit_dsem)
#Need to revise function: also see https://james-thorson-noaa.github.io/dsem/articles/features.html
#for timeseries plot and dag with effects 

# sample-based quantile residuals
samples = loo_residuals(fit_dsem, what="samples", track_progress=FALSE)
which_use = which(!is.na(hybrid_data))
fitResp = loo_residuals( fit_dsem, what="loo", track_progress=FALSE)[,'est']
simResp = apply(samples, MARGIN=3, FUN=as.vector)[which_use,]

# Build and display DHARMa object
res = DHARMa::createDHARMa(
  simulatedResponse = simResp,
  observedResponse = unlist(hybrid_data)[which_use],
  fittedPredictedResponse = fitResp )
plot(res)
#looks good

# Calculate total effects
effect = total_effect(fit_dsem, n_lags = 6)
#specifying n+1 longest lag so sparse matrix doesn't have NAs

# Plot total effect
ggplot( effect) + 
  geom_bar(aes(lag, total_effect, fill=lag), stat='identity', col='black', position='dodge' ) +
  facet_grid( from ~ to  )
#if we test an iid model structure above (ie switch 1's in AR1 terms to 0) there shouldn't
#be n-1 lags, only the specified lag
#note that for response variables with direct and indirect causal links, we see total
#effects for each specified lag/causal path

#relative importance of variables as predictors of female size structure
partition_variance(fit_dsem,
                   which_response = "hybrid_abundance",
                   n_times = 10 )
#hmm maybe this is in development version of dsem only? 

#D-separation test
test_dsep(fit_dsem) #null hypothesis- the data came from the model- 
#hmmm, need to investigate this, we don't have missing data  