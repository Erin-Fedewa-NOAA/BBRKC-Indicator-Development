#Create a predation index for BBRKC Discussion paper: mean CPUE of 
  #RKC predators within BBRKC managment area

# Erin Fedewa
# last updated: 2022/8/22

# load ----
library(tidyverse)

# data ----

pred <- read_csv("./Output/pred_timeseries.csv")
#This csv was generated via BenthicPred.R script

#Create look up table with BBRKC stations 
sta <- read_csv("./Data/crabstrata_rkc.csv")
sta %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BB

#Num of stations with catch data each yr within SMBKC district 
pred %>%
  filter(STATION %in% BB) %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(STATION))) %>%
  print(n=50)
#A few missing stations in some years but no giant red flags 

###########################################
#Juvenile/Adult RKC predators 

#Calculate mean CPUE for each predator guild across years 
pred %>%
  filter(STATION %in% BB) %>%
  mutate(thoustons = ((WTCPUE*100*1371.9616)/1000000)) %>% #Convert WTCPUE in kg/HA to thoustons/km^2
  group_by(YEAR, STATION) %>%
  summarise(pcod_cpue = sum(thoustons[SID %in% c(21720,21722)], na.rm = T),
            halibut_cpue = sum(thoustons[SID == 10120], na.rm = T),
            akskate_cpue = sum(thoustons[SID == 471], na.rm = T),
            sculpin_cpue = sum(thoustons[SID %in% c(21315,21347,21368,21370,21371,21420)], na.rm = T),
            Total_pred_cpue = sum(thoustons[SID %in% c(21720,21722,10120,471,
                                                       21315,21347,21368,21370,21371,21420)], na.rm = T)) %>%
  group_by(YEAR) %>%
  summarise(Pcod = mean(pcod_cpue),
            Halibut = mean(halibut_cpue),
            Skates = mean(akskate_cpue),
            Sculpin = mean(sculpin_cpue),
            Total_pred = mean(Total_pred_cpue))-> BBpred_timeseries1

#Plots 
BBpred_timeseries1 %>%
  pivot_longer(c(2:5), names_to = "pred_guild", values_to = "thoustons") %>%
  ggplot(aes(x = YEAR, y = thoustons, group = factor(pred_guild)))+
  geom_point(aes(colour = pred_guild), size=3) +
  geom_line(aes(colour = pred_guild),size=1) +
  labs(y ="Benthic Predator CPUE (1000t/km2)", x = "") +
  xlim(1980, 2021) + 
  theme(legend.title=element_blank())+
  ggtitle("Juvenile & adult RKC predators") +
  theme_bw()
ggsave("./Figs/Juv_ad_guild_predators.png")

BBpred_timeseries1 %>%
  ggplot(aes(x = YEAR, y = Total_pred)) +
  geom_point(size=3) +
  geom_line(size=1) +
  #geom_smooth(method = gam, formula = y~s(x))+
  labs(y = "Total Predator CPUE (1000t/km2)", x = "") +
  xlim(1980, 2021) + 
  theme_bw() +
  ggtitle("Juvenile & adult RKC predators")
ggsave("./Figs/Juv_ad_total_predators.png")

###########################################
#Early benthic juvenile RKC predators 

#Calculate mean CPUE for each predator guild across years 
pred %>%
  filter(STATION %in% BB) %>%
  mutate(thoustons = ((WTCPUE*100*1371.9616)/1000000)) %>% #Convert WTCPUE in kg/HA to thoustons/km^2
  group_by(YEAR, STATION) %>%
  summarise(nrs_cpue = sum(thoustons[SID %in% c(10261,10260)], na.rm = T),
            yfs_cpue = sum(thoustons[SID == 10210], na.rm = T),
            Total_pred_cpue = sum(thoustons[SID %in% c(10261,10210)], na.rm = T)) %>%
  group_by(YEAR) %>%
  summarise(NRS = mean(nrs_cpue),
            YFS = mean(yfs_cpue),
            Total_pred = mean(Total_pred_cpue))-> BBpred_timeseries2

#Plots 
BBpred_timeseries2 %>%
  pivot_longer(c(2:3), names_to = "pred_guild", values_to = "thoustons") %>%
  ggplot(aes(x = YEAR, y = thoustons, group = factor(pred_guild)))+
  geom_point(aes(colour = pred_guild), size=3) +
  geom_line(aes(colour = pred_guild),size=1) +
  labs(y = "Benthic Predator CPUE (1000t/km2)", x = "") +
  theme_bw()+
  xlim(1980, 2021) + 
  theme(legend.title=element_blank())+
  ggtitle("Early juvenile RKC predators")
ggsave("./Figs/Early_juv_guild_predators.png")

BBpred_timeseries2 %>%
  ggplot(aes(x = YEAR, y = Total_pred)) +
  geom_point(size=3) +
  geom_line(size=1) +
  #geom_smooth(method = gam, formula = y~s(x))+
  labs(y = "Total Predator CPUE (1000t/km2)", x = "") +
  xlim(1980, 2021) + 
  theme_bw() +
  ggtitle("Early juvenile RKC predators")
ggsave("./Figs/Early_juv_total_predators.png")
