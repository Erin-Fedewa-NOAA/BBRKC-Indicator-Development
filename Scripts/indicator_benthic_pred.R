# notes ----
#Summarize benthic predator and pcod mean CPUE across years

# Erin Fedewa

# load ----
library(tidyverse)

#TO DO: move invert/pcod indicators to biomass instead of mean CPUE, and subset pcod sizes
  #Also check with GAP on skate species codes in early years?

# data mgmt----

## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

#Load groundfish data queried directly from Racebase (see gf_data_pull.R script)
pred <- readRDS("./Data/gf_cpue_timeseries.rds") %>%
  rename(STATION_ID = STATION)

#Create look up table with BBRKC stations 
haul %>%
  filter(YEAR==2021,
         #Selecting a yr when entire grid was sampled
         HAUL_TYPE != 17,
         DISTRICT == "BB") %>% 
  pull(STATION_ID) -> BBonly

#Benthic predator species guild look up table
pred_lookup <- read_csv("./Data/ForagingguildsSource_SID.csv")

benpred <- pred_lookup %>% 
  pull(Benthic_predator)%>%
  na.omit() 

#########################################################
#WORKFLOW #1: Use this script if gf data is from FOSS and not zero filled

#stations/years for appending zero-catch data
haul %>%
  filter(YEAR>=1982, 
         DISTRICT == "BB") %>% 
  select(YEAR, STATION_ID) %>%
  distinct() -> stations

#Calculate mean CPUE for each guild across years 
pred_density <- pred %>%
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
  summarise(CPUE_KGKM2 = mean(CPUE_KGKM2), .groups = "drop") 
 
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

###########################################
#WORKFLOW #2: Use this if gf data is from GAP products and already zero filled

#Calculate mean CPUE for each guild across years 
pred_density <- pred %>%
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
  summarise(CPUE_KGKM2 = sum(CPUE_KGKM2, na.rm = TRUE), .groups = "drop") %>%
  # annual mean cpue by guild
  group_by(YEAR, guild) %>%
  summarise(CPUE_KGKM2 = mean(CPUE_KGKM2, na.rm = TRUE), .groups = "drop")

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
  geom_hline(aes(yintercept = mean(CPUE_KGKM2, na.rm = TRUE) - sd(CPUE_KGKM2, na.rm = TRUE)), color = "green4") +
  geom_hline(aes(yintercept = mean(CPUE_KGKM2, na.rm = TRUE) + sd(CPUE_KGKM2, na.rm = TRUE)), color = "green4") +
  labs(y = "Pacific Cod CPUE (kg/km2)", x = "") +
  theme_bw()

###########################################

#Write output 
indicator_pred <- pred_density %>%
  filter(guild == "pcod") %>%
  select(YEAR, CPUE_KGKM2) %>%
  rename_with(tolower) %>%
  rename(pcod_density = cpue_kgkm2) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)

write.csv(indicator_pred, file = "./Output/indicator_pcod_density.csv", row.names = F)

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_Pacific_Cod_Density_BBRKC_Survey"

##EDITABLE TEXT
description <- paste0("Summer Pacific cod density (kg/km²) estimated from EBS bottom trawl survey stations 
                      included in the BBRKC management district. Proposed sign of the relationship is negative 
                      and the time series is lagged one year for intermediate stage indicator analysis.")

status_trends <- paste0("Pacific cod density decreased from 2025 to 2026 in Bristol Bay, but is still within the 43-year mean.")

factors <- paste0("Pacific cod are a major predator of red king crab, and consumption rates increase in the 
                  spring when crab are soft and molting.")

implications <- paste0("An increase in Pacific cod densities in Bristol Bay may suggest increased predation 
                       on red king crab.")

references <- paste0("")


##INDICATOR DATA
indicator_data <- indicator_pred %>%
  rename(indicator = pcod_density)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)

