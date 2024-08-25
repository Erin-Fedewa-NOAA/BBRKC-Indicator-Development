#Ratio of Northern District : Bristol Bay RKC abundance 

# Erin Fedewa

# load ----
library(tidyverse)
library(ggridges)

##############################################

# data ----
haul <- read.csv("./Data/crabhaul_rkc.csv")
strata <- read.csv("./Data/crabstrata_rkc.csv")

#Create look up table with BBRKC stations 
strata %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly

#Create look up table with Northern District stations 
strata %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Northern Unstratified") %>% 
  pull(STATION_ID) -> NDonly

##################################################
#Calculate total abundance of BBRKC 
haul %>%
  mutate(SURVEY_YEAR = as.integer(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE != 17, 
         SURVEY_YEAR > 1981,
         GIS_STATION %in% BBonly) %>%
  group_by(SURVEY_YEAR, GIS_STATION, AREA_SWEPT,MID_LATITUDE, MID_LONGITUDE) %>%
  summarise(N_CRAB = sum(SAMPLING_FACTOR, na.rm = T),
            CPUE = N_CRAB / mean(AREA_SWEPT)) %>%
  #join to zero catch stations
  right_join(strata %>%
               filter(SURVEY_YEAR > 1981,
                      STATION_ID %in% BBonly) %>%
               distinct(SURVEY_YEAR, STATION_ID, STRATUM, TOTAL_AREA) %>%
               rename_all(~c("SURVEY_YEAR", "GIS_STATION",
                             "STRATUM", "TOTAL_AREA"))) %>%
  replace_na(list(CPUE = 0)) %>%
  #Scale to abundance by strata
  group_by(SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
  summarise(MEAN_CPUE = mean(CPUE, na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(SURVEY_YEAR) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) %>%
  mutate(District = "Bristol_Bay") -> BB_abundance

#Calculate total abundance of Northern District
haul %>%
  mutate(SURVEY_YEAR = as.integer(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE != 17, 
         SURVEY_YEAR > 1981,
         GIS_STATION %in% NDonly) %>%
  group_by(SURVEY_YEAR, GIS_STATION, AREA_SWEPT,MID_LATITUDE, MID_LONGITUDE) %>%
  summarise(N_CRAB = sum(SAMPLING_FACTOR, na.rm = T),
            CPUE = N_CRAB / mean(AREA_SWEPT)) %>%
  #join to zero catch stations
  right_join(strata %>%
               filter(SURVEY_YEAR > 1981,
                      STATION_ID %in% NDonly) %>%
               distinct(SURVEY_YEAR, STATION_ID, STRATUM, TOTAL_AREA) %>%
               rename_all(~c("SURVEY_YEAR", "GIS_STATION",
                             "STRATUM", "TOTAL_AREA"))) %>%
  replace_na(list(CPUE = 0)) %>%
  #Scale to abundance by strata
  group_by(SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
  summarise(MEAN_CPUE = mean(CPUE, na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(SURVEY_YEAR) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) %>%
  mutate(District = "Northern_District") -> ND_abundance

#join and calculate ratio
ND_abundance %>% 
  full_join(BB_abundance) %>%
  pivot_wider(names_from = "District", values_from = "ABUNDANCE_MIL") %>%
  mutate(ND_BB_ratio = Northern_District/Bristol_Bay) -> rkc_ratio

#plot
rkc_ratio %>%
  ggplot(aes(SURVEY_YEAR, ND_BB_ratio)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=mean(ND_BB_ratio))) +
  theme_bw() 

#save output 
missing <- data.frame(YEAR = 2020)

rkc_ratio %>%
  select(SURVEY_YEAR, ND_BB_ratio) %>%
 rename(YEAR = SURVEY_YEAR) %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
  write.csv(file="./Output/northern_BB_ratio.csv")
