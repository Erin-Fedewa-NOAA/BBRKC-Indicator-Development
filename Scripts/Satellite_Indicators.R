# notes ----
# Generate annual wind stress and chl-a indices for RKC larval stages   

#Wind stress data (1987-2019) is a product of NOAA Blended Winds and MetOp ASCAP sensors 
      #and is from NOAA NCEI/NOAA NESDIS- subset for BBRKC and SMBKC mgmt areas  
#The chlorophyll product (1997-2018) is from MODIS Aqua, VIIRS, MERIS, and SeaWiFS sensors 
      #and is from the Ocean Colour Program, European Space Agency. Spatial coverage is EBS
#Second chl-a product (2003 to 2019) is from MODIS: 4x4 km resolution and aggregated as 8-day composites
    #Spatial coverage is by BSIERP regions in EBS/NBS and by crab mgmt area 

#FOLLOW UP NOTE: This script was used to spatiotemporally subset satellite products for the 
#2021 BBRKC ESP report card. As of 2022, look up tables for crab managment areas are being 
#used to pull data direct online and spatially subset for indicator submission- no 
#further post-processing needed. 

# Erin Fedewa
# last updated: 2020/4/25

# load ----
library(tidyverse)
library(lubridate)

# data ----

wind <- read_csv("./Data/Blended_windstress.csv")
head(wind)
chl <- read_csv("./Data/ChlA_Merged.csv")
head(chl)
chl2 <- readRDS("./Data/merged_8day_2003_2021_EBS.RDS")
head(chl2)

# data mgmt ---

#Avg June wind stress indicator *********************************

#Plots
#BB May/June/July wind stress 
wind %>%
  select(Year, Month, bbay_mean_mps) %>%
  filter(Month %in% c(5,6,7)) %>%
  ggplot(aes(x = Year, y = bbay_mean_mps, group = as.factor(Month))) +
  geom_point(aes(color=as.factor(Month))) +
  geom_line(aes(color=as.factor(Month)))
#Very different! Worth looking into May vrs June for RKC larvae, though presumably June
  #would be most appropriate for larval stages 

#June EBS vrs BB
wind %>%
  select(Year, Month, sbs_mean, bbay_mean_mps) %>%
  filter(Month == 6) %>%
  group_by(Year) %>%
  pivot_longer(c(3:4), names_to = "region", values_to = "windstress") %>%
  ggplot(aes(x = Year, y = windstress, group = as.factor(region))) +
    geom_point(aes(colour = region)) +
    geom_line(aes(colour = region))
  #Overall similar trends 

#Calculate annual indicator 
wind %>%
  select(Year, Month, bbay_mean_mps) %>%
  filter(Month == 6) %>%
  group_by(Year) %>%
  rename(., YEAR = Year) %>%
  summarise(BB_wind = mean(bbay_mean_mps))->BBwind
 


#Avg May chla indicator *********************************
chl %>%
  select(YEAR, MONTH, north_bering_chl, south_bering_chl) %>%
  filter(MONTH == 5) %>%
  group_by(YEAR) -> Mchl

#Plots 
#June EBS vrs NBS
Mchl %>%
  pivot_longer(c(3:4), names_to = "region", values_to = "chla") %>%
  ggplot(aes(x = YEAR, y = chla, group = as.factor(region))) +
  geom_point(aes(colour = region)) +
  geom_line(aes(colour = region))
#Trends similar since 2010

#EBS May/June/July chlA 
chl %>%
  select(YEAR, MONTH, south_bering_chl) %>%
  filter(MONTH %in% c(5,6,7)) %>%
  ggplot(aes(x = YEAR, y = south_bering_chl, group = as.factor(MONTH))) +
  geom_point(aes(colour = as.factor(MONTH))) +
  geom_line(aes(colour = as.factor(MONTH)))
#May seems to best capture spring bloom and June to some extent

#MODIS only chla indicator for BBRKC mgmt area ********************
  
#Calcuate Apr-June chl-a biomass 
  chl2 %>%
    mutate(month=month(date),
           year=year(date)) %>% 
    filter(crab == "BristolBay",
           month %in% c(4, 5, 6)) %>%
    group_by(year) %>%
    summarise(BB_chla = mean(chlorophyll, na.rm=TRUE)) %>%
    rename(YEAR=year) ->modis
  
  #plot
    modis %>%
    ggplot(aes(x = YEAR, y = BB_chla)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(y = 'Chlorophyll-a [ug/l]', x = "")
  

# combine datasets now and save output
BBwind %>%
  full_join(Mchl, by = "YEAR") %>%
  full_join(modis, by = "YEAR") %>%
  write_csv("./Output/satellite_timeseries.csv")





