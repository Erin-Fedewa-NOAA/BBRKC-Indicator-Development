#Calculate proportion empty clutches in mature female RKC

#Author: Erin Fedewa

#NOTE: Resampling wasn't started until 1999 so in an attempt to standardize, we're
  #using newshell mature females only, and not replacing with rasampling data. A few
  #things to follow up on here:

#1) Look at proportion of shell condition 1 females that have clutches- might be worth 
  #including them since we know they've molted/mated?
#2) Look at the proportion of the mature female population this actually includes-
  #b/c probably a very small portion in resampling yrs, and we're assuming this metric 
  #is representative of the population - would be good to caveat results with this
#3) We should be taking this time series back to 1980 because this seems to be an informative
  #year for assigning an upper threshold (i.e. ~40% clutch failure during collapse!) but it 
  #would be worth digging into 1980 data a bit more to make sure this is a real signal- i.e. were
  #these all dead eggs, were most mature females old shell, skewed sex ratio etc?
#4) Also a note that pre-1980, there are many very small females classified as mature, and this
  #results in very high %empty, BUT these females are not likely mature. Safer bet to use 
  #1980+, and there are ongoing SAP conversations about retrospectively correcting some of 
  #these early data. OR you could use earlier years, but use GE90mm subset of females, which
  #is the ADFG cutline 
#5) Resampling data probably isn't much help here. Since the protocol wasn't started until 1999, 
  #replacing resampling data would mean pre-1999 data aren't really comparable 

#2026 to do: Overlay with Kodiak RKC clutch failure data? I think this is Resolution data but need 
  #to confirm that clutch codes are classified using the same methods 

library(tidyverse)
library(ggridges)

## Read in setup
source("./Scripts/get_crab_data.R")

########################################
#calculate abundance of all mature newshell females
mature <- dat
mature$specimen <- mature$specimen %>%
  filter(SEX == 2,
         CLUTCH_SIZE > 0,
         SHELL_CONDITION == 2)

mature <- calc_bioabund(crab_data = mature,
                        species = "RKC",
                        region = "EBS",
                        district = "BB",
                        years = years,
                        replace_retow = F) %>%
  mutate(mature_abun = ABUNDANCE) %>%
  select(YEAR, mature_abun)

# calculate abundance of just mature newshell females with empty clutches
barren <- dat
barren$specimen <- barren$specimen %>%
  filter(SEX == 2,
         SHELL_CONDITION == 2,
         EGG_CONDITION %in% c(0,3,4),
         CLUTCH_SIZE > 0) 

barren <- calc_bioabund(crab_data = barren,
                        species = "RKC",
                        region = "EBS",
                        district = "BB",
                        years = years,
                        replace_retow = F) %>%
  mutate(barren_abun = ABUNDANCE) %>%
  select(YEAR, barren_abun)

# calculate proportion empty clutches
prop_empty <- mature %>%
  full_join(barren) %>%
  mutate(prop_empty = (barren_abun/mature_abun) *100) %>% 
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) 

#plot proportion empty
prop_empty %>%
  ggplot(aes(x= YEAR, y=prop_empty)) +
  geom_point() + 
  geom_line() +
  geom_hline(aes(yintercept = mean(prop_empty, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#write output
prop_empty %>%
  select(YEAR, prop_empty) %>%
  write.csv(file="./Output/clutch_fullness.csv", row.names = F)

  