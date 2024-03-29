# notes ----
#Summarize benthic predator and pcod mean CPUE across years

# Erin Fedewa
# last updated: 2023/9/12 with 2023 groundfish data

#Note: This dataset will differ from past iterations- zero filled gf data now being used, 
#and units now in kg/km^2

# load ----
library(tidyverse)

# data mgmt----

#Load groundfish data queried directly from Racebase (see gf_data_pull.R script)
pred <- read_csv("./Data/gf_cpue_timeseries.csv")

#Benthic predator species guild look up table
ben <- read_csv("./Data/ForagingGuildsSource_SPECIES_CODE.csv")
ben %>% 
  pull(Benthic_predator)%>%
  na.omit() -> benpred
#Can use this to filter down dataset for benthic predators only but we'll skip for now...

#Create look up table with BBRKC stations 
sta <- read_csv("./Data/crabstrata_rkc.csv")
sta %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly

################################
#Num of stations with catch data each yr within BBRKC district 
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
  group_by(YEAR, STATION) %>%
  summarise(Sab_Hal_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(20510, 10120)], na.rm = T),
            Pcod_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(21720, 21722)], na.rm = T),
            Skates_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(420,435,440,455,471,472,480,460,485)], na.rm = T),
            Flatfish_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(10220,10115,10130,10140,10120,10261,10210,10260)], na.rm = T),
            Sculpin_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(21347,21348,21368,21370,21388,21420,21311,21315,21390,21438,21371)], na.rm = T),
            Eelpout_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(24184, 24191, 24185)], na.rm = T),  
            Wolfish_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(20320, 20322)], na.rm = T), 
            Octopus_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% c(78010, 78012, 78403)], na.rm = T),
            Total_Pred_cpue = sum(CPUE_KGKM2[SPECIES_CODE %in% benpred], na.rm = T)) %>% 
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
  pivot_longer(c(2:9), names_to = "pred_guild", values_to = "CPUE_KGKM2") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2, group = factor(pred_guild)))+
  geom_point(aes(colour = pred_guild)) +
  geom_line(aes(colour = pred_guild)) +
  labs(y = "Benthic Predator CPUE (1000t/km2)", x = "") +
  theme_bw() +
  theme(legend.title=element_blank())
#YFS really dominates biomass here....

BBpred_timeseries %>%
  pivot_longer(c(2:9), names_to = "pred_guild", values_to = "CPUE_KGKM2") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2))+
  geom_point() +
  geom_line() +
  labs(y = "CPUE (1000t/km2)", x = "") +
  theme_bw() +
  theme(legend.title=element_blank()) +
  facet_wrap(~pred_guild, scales = "free_y")

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

