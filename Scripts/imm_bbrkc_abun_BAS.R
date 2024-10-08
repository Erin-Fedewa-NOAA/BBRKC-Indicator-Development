# notes ----]
# Calculate abundance of immature male RKC (95-120 mm) as response for BAS analysis
  
#To update for 2024: Add recruitment model output as second model response 
  #Also add responses for immature females, and smaller RKC cohorts to reduce 
  #uncertainty associated with long larval lags 

# Erin Fedewa
# last updated: 2023/8/27

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

#Calculate CPUE by station for pre-recruits 
haul %>%
  mutate(SURVEY_YEAR = as.integer(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE != 17, 
         SEX == 1,
         SURVEY_YEAR > 1981,
         GIS_STATION %in% BBonly) %>%
  mutate(MAT_SEX = case_when((LENGTH_1MM >= 95 & LENGTH_1MM < 120) ~ "PreRecruit")) %>%
  filter(MAT_SEX == "PreRecruit") %>%
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
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) -> recruit_abundance

#Write output 
write_csv(recruit_abundance, "./Output/BAS_survey_recruit.csv")

#Plot
recruit_abundance %>%
  ggplot(aes(x = SURVEY_YEAR, y = ABUNDANCE_MIL)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()
