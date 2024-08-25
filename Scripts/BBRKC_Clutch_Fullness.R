#Calculate proportion full clutches in mature female RKC

#Author: Erin Fedewa

library(tidyverse)
library(ggridges)


##############################################

## EBS haul data ----
rkc_catch <- read.csv("./Data/crabhaul_rkc.csv")

#EBS strata data ----
rkc_strata <- read.csv("./Data/crabstrata_rkc.csv")

#Create look up table with BBRKC stations 
rkc_strata %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly

########################################
#Proportion of mature females by clutch size 
rkc_catch %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE != 17,
         SEX == 2,
         CLUTCH_SIZE > 0,
         YEAR >= 1982,
         GIS_STATION %in% BBonly) %>%
  mutate(SHELL_CONDITION = case_when(SHELL_CONDITION %in% c(0,1,2) ~ "Primiparous",
                                     SHELL_CONDITION > 2 ~ "Multiparous"),
         CLUTCH_TEXT = case_when(CLUTCH_SIZE %in% c(1,2) ~ "Empty_trace",
                                 CLUTCH_SIZE %in% c(3,4) ~ "Quarter_half",
                                 CLUTCH_SIZE %in% c(5,6) ~ "Full")) -> female_rkc_spec
#Compute CPUE
female_rkc_spec %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT, SHELL_CONDITION, CLUTCH_TEXT) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue = ncrab / AREA_SWEPT) %>%
  #add in data field for total mature female population
  bind_rows(rkc_catch %>% 
              mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
              filter(HAUL_TYPE == 3, SEX == 2,
                     CLUTCH_SIZE > 0,
                     YEAR >= 1982,
                     GIS_STATION %in% BBonly) %>% 
              mutate(CLUTCH_TEXT = "All",
                     SHELL_CONDITION = case_when(SHELL_CONDITION %in% c(0,1,2) ~ "Primiparous",
                                                 SHELL_CONDITION > 2 ~ "Multiparous")) %>%
              group_by(YEAR, GIS_STATION, AREA_SWEPT, SHELL_CONDITION, CLUTCH_TEXT) %>%
              summarise(ncrab = round(sum(SAMPLING_FACTOR,na.rm = T)))) %>%
  filter(!is.na(CLUTCH_TEXT)) %>%
  ungroup() %>%
  mutate(cpue = ncrab / AREA_SWEPT) %>%
  #Join to stations that didn't catch crab 
  right_join(expand_grid(SHELL_CONDITION = c("Primiparous", "Multiparous"),
                         CLUTCH_TEXT = c("Empty_trace", "Quarter_half", "Full", "All"),
                         rkc_strata %>%
                           select(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
                           filter(SURVEY_YEAR >= 1980) %>%
                           rename_all(~c("GIS_STATION", "YEAR",
                                         "STRATUM", "TOTAL_AREA")))) %>%
  replace_na(list(ncrab = 0, cpue = 0)) %>%
  #Scale to abundance by strata
  group_by(YEAR, STRATUM, TOTAL_AREA, SHELL_CONDITION, CLUTCH_TEXT) %>%
  summarise(MEAN_CPUE = mean(cpue , na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(YEAR, SHELL_CONDITION, CLUTCH_TEXT) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) -> fem_abundance

#Calculate proportion full of primiparous/multiparous females 
fem_abundance %>%
  group_by(YEAR, SHELL_CONDITION) %>%
  summarise(Prop_full = (ABUNDANCE_MIL[CLUTCH_TEXT=="Full"]/ABUNDANCE_MIL[CLUTCH_TEXT=="All"])) -> full

#Plot
full %>%
  ggplot() +
  geom_point(aes(x= YEAR, y=Prop_full)) + 
  geom_line(aes(x= YEAR, y=Prop_full)) +
  facet_wrap(~SHELL_CONDITION) + 
  theme_bw()

#Plot primiparous females only
full %>%
  filter(SHELL_CONDITION == "Primiparous") %>%
  ggplot() +
  geom_point(aes(x= YEAR, y=Prop_full)) + 
  geom_line(aes(x= YEAR, y=Prop_full)) +
  geom_hline(aes(yintercept = mean(Prop_full, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#Hmmm this is a tough one. In years when the molt/mate cycle is delayed, 
#the proportion full declines b/c more females are coded as 001 (e.g. 2017). 
#This isn't really an indicator of sperm limitation/not finding a mate, but 
#instead, confounded with reproductive timing. Let's skip as an indicator for now....

  