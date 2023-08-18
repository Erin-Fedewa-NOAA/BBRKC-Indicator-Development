# notes ----
# Generate avg bottom temp, and cold pool extent indices within Bristol Bay
#from EBS BT timeseries 
#Arctic Oscillation is pulled from NOAA-NWS via:
  #https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/ao.shtml

#FOLLOW UP: Use an imputed timeseries to date correct and account for missing 
  #stations

# Erin Fedewa
# last updated: 2023/8/18

# load ----
library(tidyverse)

# BT survey data ----
temp <- read_csv("./Data/crabhaul_rkc.csv") 

#Create look up table with BBRKC stations 
sta <- read_csv("./Data/crabstrata_rkc.csv")
sta %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly

#Num of stations with catch data each yr within Bristol Bay 
temp %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(GIS_STATION %in% BBonly,
         HAUL_TYPE!=17) %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(GIS_STATION))) %>%
  print(n=50)
#Missing stations in early years-lets pull pre-1979   

# compute mean summer bottom temperature
temp %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(GIS_STATION %in% BBonly,
         YEAR >= 1979,
         HAUL_TYPE!=17) %>%
  distinct(YEAR, GIS_STATION, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(summer_bt = mean(GEAR_TEMPERATURE, na.rm = T))-> avg_bt

#Plot
avg_bt %>%
  ggplot(aes(x = as.numeric(YEAR), y = summer_bt)) +
  geom_point() +
  geom_line()+
  labs(y = "Bottom temperature (C)", x = "") +
  xlim(1978, 2023) +
  theme_bw()

#compute cold pool areal extent
temp %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(GIS_STATION %in% BBonly,
         YEAR >= 1979,
         HAUL_TYPE != 17) %>%
  distinct(YEAR, GIS_STATION, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(cpa = sum(GEAR_TEMPERATURE < 2, na.rm = T) * 401) -> cpa

#Plot
cpa %>%
  ggplot(aes(x = as.numeric(YEAR), y = cpa)) +
  geom_point() +
  geom_line()+
  labs(y = "Cold Pool Extent (nmi2)", x = "") +
  xlim(1978, 2023) +
  theme_bw()

###########################################
#Arctic Oscillation

AO<- read_csv("./Data/Arctic_oscillation.csv")

#Mean Winter Arctic Oscillation
AO %>% 
  pivot_longer(c(2:13), names_to="Month", values_to="Index") %>%
  filter(Year >= 1979,
         Month %in% c(1,2,3)) %>% 
  group_by(Year) %>%
  rename(., YEAR = Year) %>%
  summarize(Mean_AO = mean(Index)) -> mean_AO

#Plot
mean_AO %>%
  ggplot(aes(x = as.numeric(YEAR), y = Mean_AO)) +
  geom_point() +
  geom_line()+
  labs(y = "Arctic Oscillation Index", x = "") +
  xlim(1978, 2023) +
  theme_bw()

# combine indices and save output
avg_bt %>%
  full_join(cpa) %>%
  full_join(mean_AO %>%
              mutate(YEAR = as.character(YEAR))) ->env
write_csv(env, "./Output/environmental_timeseries.csv")

#########################################################

#ROMS Bering 10k hindcast timeseries 
#Used for 2020 ESP report card due to cancellation of EBS BT survey
#Note that output was already post-processed for BBRKC district 

ROMS<- read_csv("./Data/ROMS_temp_timeseries.csv")


# Compute spring (Feb-March) bottom temps in BB
  ROMS %>%
    filter(year>=1979,
           month %in% c(2,3)) %>%
    group_by(year) %>%
    rename(., SURVEY_YEAR = year) %>%
    summarise(ROMS_spr_bt = mean(btemp_bbrkc)) ->ROMS_spr
  
#Compute summer temps (June-July) in BB
  ROMS %>%
    filter(year>=1979,
           month %in% c(6,7)) %>%
    group_by(year) %>%
    rename(., SURVEY_YEAR = year) %>%
    summarise(ROMS_summer_bt = mean(btemp_bbrkc)) ->ROMS_summer
  
#Compute cold pool index (<2C) for EBS on July 1
  ROMS %>%
    filter(year>=1979,
           isjuly1 == TRUE) %>%  #True corresponds to closest-to-July point for a given yr 
    group_by(year) %>%
    rename(., SURVEY_YEAR = year) %>%
    summarise(ROMS_cp = mean(fracbelow2_sebs)) -> ROMS_cp








