# notes ----]
# Calculate abundance of immature male RKC ( - mm) as response for BAS analysis
  #Size range selected using BSFRF selectivity curves and St. Marie 1995 size at 
  #age estimates (~5.7-6.7 years post settlement)

#To update for 2024: Add recruitment model output as second model response 

# Erin Fedewa
# last updated: 2023/8/27

# load ----
library(tidyverse)
library(ggridges)

##############################################

## EBS haul data ----
rkc_catch <- read.csv("./Data/crabhaul_rkc.csv")

#EBS strata data ----
strata_rkc <- read.csv("./Data/crabstrata_rkc.csv")

#Create look up table with BBRKC stations 
sta <- read_csv("./Data/crabstrata_rkc.csv")
sta %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly

########################################
rkc_catch %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE != 17,
         GIS_STATION %in% BBonly,
         LENGTH_1MM >= 50 & LENGTH_1MM <= 65,
         YEAR >= 1982) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue_cnt = ncrab / AREA_SWEPT) %>%
  # join to hauls that didn't catch crab 
  right_join(rkc_catch %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3,
                      YEAR >= 1982) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
                replace_na(list(cpue_cnt = 0)) %>%
                replace_na(list(ncrab = 0)) %>%

#join to stratum
  left_join(strata_rkc %>%
              select(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
              filter(SURVEY_YEAR >= 1982) %>%
              rename_all(~c("GIS_STATION", "YEAR",
                                     "STRATUM", "TOTAL_AREA"))) %>%
  #Scale to abundance by strata
  group_by(YEAR, STRATUM, TOTAL_AREA) %>%
  summarise(MEAN_CPUE = mean(cpue_cnt , na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(YEAR) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) -> abundance

#Plot
ggplot(abundance, aes(y=ABUNDANCE_MIL, x=YEAR)) +
  geom_point() +
  geom_line()

#Write csv as output (abundance in millions of crab)
write.csv(abundance, file = ("./Output/BAS_survey_response.csv"))
