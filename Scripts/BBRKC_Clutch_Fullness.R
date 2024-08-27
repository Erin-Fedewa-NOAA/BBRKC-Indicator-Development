#Calculate proportion empty clutches in mature female RKC

#Author: Erin Fedewa

library(tidyverse)
library(ggridges)

#2025 follow up: Resampling wasn't started until 1999 so in earlier timeseries, is high
  #proportion of empty clutches due to females not yet extruding? Maybe % full clutch is 
  #a better metric? Also overlay with Kodiak RKC clutch data 


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
  filter(SEX == 2,
         CLUTCH_SIZE > 0,
         #YEAR >= 1988,
         SHELL_CONDITION %in% c(1,2),
         GIS_STATION %in% BBonly,
        #using only resampled data in resample years
         ((HAUL_TYPE == 17 & YEAR %in% c(1999,2000, 2006:2012,2017,2021)) | 
            (HAUL_TYPE == 3 & !(YEAR %in% c(1999,2000, 2006:2012,2017,2021))))) %>%
    mutate(CLUTCH_TEXT = case_when(CLUTCH_SIZE == 1 ~ "Empty",
                                 CLUTCH_SIZE %in% c(2,3) ~ "Trace_quarter",
                                 CLUTCH_SIZE %in% c(4,5,6) ~ "Full")) %>%
  filter(!is.na(CLUTCH_TEXT)) %>%
  group_by(YEAR, CLUTCH_TEXT) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  #add in data field for total mature female population
  bind_rows(rkc_catch %>% 
              mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
              filter(SEX == 2,
                     CLUTCH_SIZE > 0,
                     #YEAR >= 1988,
                     SHELL_CONDITION %in% c(1,2),
                     GIS_STATION %in% BBonly,
                     #using only resampled data in resample years
                     ((HAUL_TYPE == 17 & YEAR %in% c(1999,2000, 2006:2012,2017,2021)) | 
                        (HAUL_TYPE == 3 & !(YEAR %in% c(1999,2000, 2006:2012,2017,2021))))) %>%
              mutate(CLUTCH_TEXT = "All") %>%
              filter(!is.na(CLUTCH_TEXT)) %>%
              group_by(YEAR, CLUTCH_TEXT) %>%
              summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T))) -> fem_clutch

#Calculate proportion of females with empty clutches and write output
fem_clutch %>%
  group_by(YEAR) %>%
  summarise(Prop_empty = (ncrab[CLUTCH_TEXT=="Empty"]/ncrab[CLUTCH_TEXT=="All"]) *100) -> prop

#Plot 
test %>%
  ggplot() +
  geom_point(aes(x= YEAR, y=Prop_empty)) + 
  geom_line(aes(x= YEAR, y=Prop_empty)) +
  geom_hline(aes(yintercept = mean(Prop_empty, na.rm=TRUE)), linetype = 5) +
  theme_bw() +
  labs(y="Proportion (%) empty clutches")

#write output
missing <- data.frame(YEAR = 2020, Prop_empty = "NA")
zero <- data.frame(YEAR = c(1993:1994,1996,2000:2001,2003,2005:2007,
                            2010,2012, 2014, 2016:2017,2021), Prop_empty= 0)

prop %>%
  bind_rows(missing %>% mutate(Prop_empty = as.numeric(Prop_empty))) %>%
  bind_rows(zero) %>%
  arrange(YEAR) %>%
  write.csv(file="./Output/clutch_fullness.csv")

  