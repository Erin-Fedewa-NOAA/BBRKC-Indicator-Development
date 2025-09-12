# notes ----
#Summarize benthic predator and pcod mean CPUE across years

# Erin Fedewa

# load ----
library(tidyverse)

#2025 update: move invert/pcod indicators to biomass instead of mean CPUE, and subset pcod sizes
  #Also check with GAP on skate species codes in early years?

# data mgmt----

## Read in setup
source("./Scripts/get_crab_data.R")

#Load groundfish data queried directly from Racebase (see gf_data_pull.R script)
pred <- read_csv("./Data/gf_cpue_timeseries.csv") %>%
  rename(STATION_ID = STATION)

#Create look up table with BBRKC stations 
haul %>%
  filter(YEAR==2021,
         #Selecting a yr when entire grid was sampled
         HAUL_TYPE != 17,
         DISTRICT == "BB") %>% 
  pull(STATION_ID) -> BBonly

#stations/years for appending zero-catch data
haul %>%
  filter(YEAR>=1982, 
         DISTRICT == "BB") %>% 
  select(YEAR, STATION_ID) %>%
  distinct() -> stations

#Benthic predator species guild look up table
ben <- read_csv("./Data/ForagingguildsSource_SID.csv")
ben %>% 
  pull(Benthic_predator)%>%
  na.omit() -> benpred

################################
#Calculate mean CPUE for each guild across years 
pred %>%
  mutate(guild = case_when(SPECIES_CODE == 10120 ~ "halibut",
                           SPECIES_CODE %in% c(21720, 21722) ~ "pcod",
                           SPECIES_CODE %in% c(420,435,440,455,471,472,480,460,485) ~ "skates",
                           SPECIES_CODE %in% c(21347,21348,21368,21370,21388,21420,21311,21315,21390,21438,21371) ~ "scuplin",
                           SPECIES_CODE %in% c(24184, 24191, 24185) ~ "eelpout",
                           SPECIES_CODE %in% c(20320, 20322) ~ "wolfish",
                           SPECIES_CODE %in% c(78010, 78012, 78403) ~ "octopus")) %>%
    filter(STATION_ID %in% BBonly,
         YEAR >= 1982,
         !is.na(guild)) %>%
  # station-level cpue by guild
  group_by(YEAR, STATION_ID, guild) %>%
  summarise(CPUE_KGKM2 = sum(CPUE_KGKM2)) %>%
  # add in 0-catch stations by guild
  right_join(., expand_grid(stations, guild = c("halibut", "pcod", "skates", "sculpin", "eelpout", "wolfish", "octopus"))) %>%
  arrange(YEAR, STATION_ID, guild) %>%
  mutate(CPUE_KGKM2 = replace_na(CPUE_KGKM2, 0)) %>%
  # annual mean cpue by guild
  group_by(YEAR, guild) %>%
  summarise(CPUE_KGKM2 = mean(CPUE_KGKM2)) -> pred_density
 
#Add in missing 2020 line and write output 
missing <- data.frame(YEAR = 2020)

pred_density %>%
  filter(guild == "pcod") %>%
  select(YEAR, CPUE_KGKM2) %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
write.csv(file = "./Output/pcod_density.csv", row.names = F)

#Plots 
pred_density %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2, group = factor(guild)))+
  geom_point(aes(colour = guild)) +
  geom_line(aes(colour = guild)) +
  labs(y = "Benthic Predator CPUE (kg/km2)", x = "") +
  theme_bw() +
  theme(legend.title=element_blank())

#Just Pcod Plot
pred_density %>%
  filter(guild == "pcod") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2)) +
  geom_point() +
  geom_line()+
  geom_hline(aes(yintercept = mean(CPUE_KGKM2)), linetype = 2)+
  labs(y = "Pacific Cod CPUE (kg/km2)", x = "") +
  theme_bw()

