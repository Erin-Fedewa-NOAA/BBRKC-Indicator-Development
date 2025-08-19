#Calculate proportion empty clutches in mature female RKC

#Author: Erin Fedewa

#NOTE: Resampling wasn't started until 1999 so in an attempt to standardize, we're
  #using newshell mature females only, and not replacing with rasampling data- though
  #this metric is still tricky to interpret 

#2026 to due: Overlay with Kodiak RKC clutch failure data 

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
missing <- data.frame(YEAR = 2020)

prop_empty %>%
  select(YEAR, prop_empty) %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
  write.csv(file="./Output/clutch_fullness.csv", row.names = F)

  