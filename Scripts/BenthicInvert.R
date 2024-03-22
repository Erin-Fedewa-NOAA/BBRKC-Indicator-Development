# notes ----
#Summarize benthic invert mean CPUE across years in Bristol Bay 

# Erin Fedewa
# last updated: 2023/9/22 with 2023 groundfish data 

# load ----
library(tidyverse)
library(mgcv)

# data ----

#Load groundfish data queried directly from Racebase (see gf_data_pull.R script)
benthic <- read_csv("./Data/gf_cpue_timeseries.csv")

#Create look up table with BBRKC stations 
sta <- read_csv("./Data/crabstrata_rkc.csv")
sta %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly

#Num of stations with catch data each yr within Bristol Bay 
benthic %>%
  filter(STATION %in% BBonly) %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(STATION))) %>%
  print(n=50)
#A few missing stations in some years- 

#Calculate mean CPUE (in kg/km^2) for each guild across years 
benthic %>%
  filter(STATION %in% BBonly, 
         !(SPECIES_CODE %in% c(68560, 68580, 69322, 69323))) %>% #remove commercial crab species 
  group_by(YEAR, STATION) %>%
  summarise(Gersemia_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(41201:41221)], na.rm = T),
            Pennatulacea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(42000:42999)], na.rm = T),
            Actinaria_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(43000:43999)], na.rm = T),
            Polychaeta_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(50000:59099)], na.rm = T),
            Barnacles_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(65100:65211)], na.rm = T),
            Shrimps_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(66000:66912)], na.rm = T),
            Crabs_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(68000:69599)], na.rm = T),
            Gastropods_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(71000:73999)], na.rm = T),
            Bivalves_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(74000:75799)], na.rm = T),
            Asteroidea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(80000:82499)], na.rm = T),
            Echinoidea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(82500:82729)], na.rm = T),
            Ophiuroidea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(83000:84999)], na.rm = T),
            Holothuroidea_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(85000:85999)], na.rm = T),
            Porifera_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(91000:91999)], na.rm = T),
            Bryozoans_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(95000:95499)], na.rm = T),
            Ascidians_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(98000:99909)], na.rm = T),
            Total_Benthic_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(41201:99909)], na.rm = T)) %>%
  group_by(YEAR) %>%
  summarise(Gersemia = mean(Gersemia_cpue),
            Pennatulacea = mean(Pennatulacea_cpue),
            Actinaria = mean(Actinaria_cpue),
            Polychaeta = mean(Polychaeta_cpue),
            Barnacles = mean(Barnacles_cpue),
            Shrimps = mean(Shrimps_cpue),
            Crabs = mean(Crabs_cpue),
            Gastropods = mean(Gastropods_cpue),
            Bivalves = mean( Bivalves_cpue),
            Asteroidea = mean(Asteroidea_cpue),
            Echinoidea = mean(Echinoidea_cpue),
            Ophiuroidea = mean(Ophiuroidea_cpue),
            Holothuroidea = mean(Holothuroidea_cpue),
            Porifera = mean(Porifera_cpue),
            Bryozoans = mean(Bryozoans_cpue),
            Ascidians = mean(Ascidians_cpue),
            Total_Benthic = mean(Total_Benthic_cpue))-> BBbenthic_timeseries

write.csv(BBbenthic_timeseries, file = "./Output/BBbenthic_timeseries.csv")

#Plots 
BBbenthic_timeseries %>%
  pivot_longer(c(2:17), names_to = "benthic_guild", values_to = "CPUE_KGKM2") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2, group = factor(benthic_guild)))+
  geom_point(aes(colour = benthic_guild)) +
  geom_line(aes(colour = benthic_guild)) +
  # geom_hline(aes(yintercept = mean(CPUE_KGKM2)), linetype = 2)+
  labs(y = "Benthic Invert CPUE (1000t/km2)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

#Cleaned up figure for industry symposium 
BBbenthic_timeseries %>%
  ggplot(aes(x = YEAR, y = Total_Benthic)) +
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Total_Benthic, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Total_Benthic, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Total_Benthic, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2022.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Total Benthic Invert CPUE (1000t/km2)", x = "") +
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Benthic Invertebrate Density")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  theme(axis.text=element_text(size=12))


