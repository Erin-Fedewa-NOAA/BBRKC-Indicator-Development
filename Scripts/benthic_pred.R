# notes ----
#Summarize benthic predator and pcod mean CPUE across years

# Erin Fedewa

# load ----
library(tidyverse)

#2025 update: move invert/pcod indicators to biomass instead of mean CPUE, and subset pcod sizes
  #Also check with GAP on skate species codes in early years?

# data mgmt----

#Load groundfish data queried directly from Racebase (see gf_data_pull.R script)
pred <- read_csv("./Data/gf_cpue_timeseries_2024.csv")

#Create look up table with BBRKC stations 
sta <- read_csv("./Data/crabstrata_rkc.csv")
sta %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly

#Benthic predator species guild look up table
ben <- read_csv("./Data/ForagingGuildsSource_SID.csv")
ben %>% 
  pull(Benthic_predator)%>%
  na.omit() -> benpred

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
  filter(STATION %in% BBonly,
         YEAR > 1987) %>%
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

#Add in missing 2020 line and write output 
missing <- data.frame(YEAR = 2020)

BBpred_timeseries %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
write.csv(file = "./Output/BBpred_timeseries.csv")

#Plots 
BBpred_timeseries %>%
  pivot_longer(c(2:9), names_to = "pred_guild", values_to = "CPUE_KGKM2") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2, group = factor(pred_guild)))+
  geom_point(aes(colour = pred_guild)) +
  geom_line(aes(colour = pred_guild)) +
  labs(y = "Benthic Predator CPUE (kg/km2)", x = "") +
  theme_bw() +
  theme(legend.title=element_blank())
#YFS really dominates here....

BBpred_timeseries %>%
  pivot_longer(c(2:9), names_to = "pred_guild", values_to = "CPUE_KGKM2") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2))+
  geom_point() +
  geom_line() +
  labs(y = "CPUE (kg/km2)", x = "") +
  theme_bw() +
  theme(legend.title=element_blank()) +
  facet_wrap(~pred_guild, scales = "free_y")

BBpred_timeseries %>%
  ggplot(aes(x = YEAR, y = Total_Pred)) +
  geom_point() +
  geom_line()+
  labs(y = "Benthic Predator CPUE (kg/km2)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

#Just Pcod Plot
BBpred_timeseries %>%
  ggplot(aes(x = YEAR, y = Pcod)) +
  geom_point() +
  geom_line()+
  geom_hline(aes(yintercept = mean(Pcod)), linetype = 2)+
  labs(y = "Pacific Cod CPUE (kg/km2)", x = "") +
  theme_bw()

