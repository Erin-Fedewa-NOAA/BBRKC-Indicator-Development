# notes ----
#Summarize benthic predator and pcod mean CPUE across years

# Erin Fedewa
# last updated: 2022/9/24 with 2022 groundfish data

# load ----
library(tidyverse)

# data mgmt----

#Benthic predator species guild look up table
ben <- read_csv("./Data/ForagingGuildsSource_SID.csv")
ben %>% 
  #Pulling NRS and YFS for ESP (used for BBRKC discussion paper analysis)
  filter(!Benthic_predator %in% c(10261,10210,10260)) %>% 
  pull(Benthic_predator)%>%
  na.omit() -> benpred

#Create look up table with BBRKC stations 
sta <- read_csv("./Data/crabstrata_rkc.csv")
sta %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly

#Function to import data and filter for only benthic predators
import <- function(filename) {
  ebs <- read_csv(filename)
  ebs %>%
    filter(SID %in% c(benpred))
}

#Add all bottom trawl data files
ebs82 <- import("./Data/Groundfish Catch Data/ebs1982_1984.csv")
ebs85 <- import("./Data/Groundfish Catch Data/ebs1985_1989.csv")
ebs90 <- import("./Data/Groundfish Catch Data/ebs1990_1994.csv")
ebs95 <- import("./Data/Groundfish Catch Data/ebs1995_1999.csv")
ebs00 <- import("./Data/Groundfish Catch Data/ebs2000_2004.csv")
ebs05 <- import("./Data/Groundfish Catch Data/ebs2005_2008.csv")
ebs09 <- import("./Data/Groundfish Catch Data/ebs2009_2012.csv")
ebs13 <- import("./Data/Groundfish Catch Data/ebs2013_2016.csv")
ebs17 <- import("./Data/Groundfish Catch Data/ebs2017_2018.csv")
ebs19 <- import("./Data/Groundfish Catch Data/ebs2019.csv")
ebs21 <- import("./Data/Groundfish Catch Data/ebs2021.csv")
ebs22 <- import("./Data/Groundfish Catch Data/ebs2022.csv")

# combine datasets and save output
bind_rows(ebs82, ebs85, ebs90, ebs95, ebs00, ebs05, ebs09, ebs13, ebs17, ebs19, ebs21, ebs22) %>%
  write_csv("./Output/pred_timeseries.csv")
pred <- read_csv("./Output/pred_timeseries.csv")

################################
#Num of stations with catch data each yr within SMBKC district 
pred %>%
  filter(STATION %in% BBonly) %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(STATION))) %>%
  print(n=50)
#A few missing stations in some years

#Calculate mean CPUE for each guild across years 
#Note:specifying each species here because stomach contents/diets were validated for most
#and included if assumed to be benthic predator on crab juv/adult stages 
pred %>%
  filter(STATION %in% BBonly) %>%
  mutate(thoustons = ((WTCPUE*100*1371.9616)/1000000)) %>% #Convert WTCPUE in kg/HA to thoustons/km^2
  group_by(YEAR, STATION) %>%
  summarise(Sab_Hal_cpue = sum(thoustons[SID %in% c(20510, 10120)], na.rm = T),
            Pcod_cpue = sum(thoustons[SID %in% c(21720, 21722)], na.rm = T),
            Skates_cpue = sum(thoustons[SID %in% c(420,435,440,455,471,472,480,460,485)], na.rm = T),
            Flatfish_cpue = sum(thoustons[SID %in% c(10220,10115,10130,10140)], na.rm = T),
            Sculpin_cpue = sum(thoustons[SID %in% c(21347,21348,21368,21370,21388,21420,21311,21315,21390,21438,21371)], na.rm = T),
            Eelpout_cpue = sum(thoustons[SID %in% c(24184, 24191, 24185)], na.rm = T),  
            Wolfish_cpue = sum(thoustons[SID %in% c(20320, 20322)], na.rm = T), 
            Octopus_cpue = sum(thoustons[SID %in% c(78010, 78012, 78403)], na.rm = T),
            Total_Pred_cpue = sum(thoustons[SID %in% benpred], na.rm = T)) %>% 
  group_by(YEAR) %>%
  summarise(Sab_Hal = mean(Sab_Hal_cpue),
            Pcod = mean(Pcod_cpue),
            Skates = mean(Skates_cpue),
            Flatfish = mean(Flatfish_cpue),
            Sculpin = mean(Sculpin_cpue),
            Eelpout = mean(Eelpout_cpue),
            Wolfish = mean(Wolfish_cpue),
            Octopus = mean(Octopus_cpue),
            Total_Pred = mean(Total_Pred_cpue)) -> BBpred_timeseries
write.csv(BBpred_timeseries, file = "./Output/BBpred_timeseries.csv")

#Plots 
BBpred_timeseries %>%
  pivot_longer(c(2:8), names_to = "pred_guild", values_to = "thoustons") %>%
  ggplot(aes(x = YEAR, y = thoustons, group = factor(pred_guild)))+
  geom_point(aes(colour = pred_guild)) +
  geom_line(aes(colour = pred_guild)) +
  labs(y = "Benthic Predator CPUE (1000t/km2)", x = "") +
  xlim(1980, 2022) +
  theme_bw() +
  theme(legend.title=element_blank())

BBpred_timeseries %>%
  ggplot(aes(x = YEAR, y = Total_Pred)) +
  geom_point() +
  geom_line()+
  labs(y = "Benthic Predator CPUE (1000t/km2)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

#Just Pcod Plot
BBpred_timeseries %>%
  ggplot(aes(x = YEAR, y = Pcod)) +
  geom_point() +
  geom_line()+
  labs(y = "Pacific Cod CPUE (1000t/km2)", x = "") +
  theme_bw()

