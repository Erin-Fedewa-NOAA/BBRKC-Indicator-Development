# notes ----
#Summarize benthic invert mean CPUE across years in Bristol Bay 

# E. Fedewa/S. Hennessey 

# load ----
library(tidyverse)
library(mgcv)

# data ----

## Read in setup
source("./Scripts/get_crab_data.R")

#Load groundfish data queried directly from Racebase (see gf_data_pull.R script)
prey <- read_csv("./Data/gf_cpue_timeseries.csv") %>%
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

##############################################################

#Calculate mean CPUE (in kg/km^2) for each invert guild across years 

# Define guilds by species code
#gersemia <- c(41201:41221)
pennatulacea <- c(42000:42999)
actinaria <- c(43000:43999)
polychaeta <- c(50000:59099)
worms_misc <- c(92000, 92500, 92502, 92511, 93100, 94000, 94500)
barnacles <- c(65000, 65100:65211)
shrimps <- c(66000:66912)
crabs <- c(68000:69599) 
crabs <- crabs[!crabs %in% c(68560, 68580, 68590, 69322, 69323, 69400, 69310, 68550, 68541)] # remove commercial species
gastropods <- c(71000:73999)
bivalves <- c(74000:75799)
asteroidea <- c(80000:82499)
echinoidea <- c(82500:82729, 82730, 82740)
ophiuroidea <- c(83000:84999)
holothuroidea <- c(85000:85999)
porifera <- c(91000:91999)
bryozoans <- c(95000:95499)
ascidians <- c(98000:99909)

guilds <- c("shrimps", "crabs", "gastropods", "bivalves", "asteroidea", "echinoidea",
  "polychaeta",  "ophiuroidea", "holothuroidea",  "bryozoans")

# Calculate mean CPUE by guild and year  
ben_prey <- prey %>%
  mutate(GUILD = case_when(SPECIES_CODE %in% polychaeta ~ "polychaeta",
    SPECIES_CODE %in% shrimps ~ "shrimps",
    SPECIES_CODE %in% crabs ~ "crabs",
    SPECIES_CODE %in% gastropods ~ "gastropods",
    SPECIES_CODE %in% bivalves ~ "bivalves",
    SPECIES_CODE %in% asteroidea ~ "asteroidea",
    SPECIES_CODE %in% echinoidea ~ "echinoidea",
    SPECIES_CODE %in% ophiuroidea ~ "ophiuroidea",
    SPECIES_CODE %in% holothuroidea ~ "holothuroidea",
    SPECIES_CODE %in% bryozoans ~ "bryozoans",
    TRUE ~ NA)) %>%             
  filter(STATION_ID %in% BBonly, 
         YEAR %in% years,
         !is.na(GUILD)) %>%
  # station-level cpue by guild
  group_by(YEAR, STATION_ID, GUILD) %>%
  summarise(CPUE_KGKM2 = sum(CPUE_KGKM2)) %>%
  # add in 0-catch stations by guild
  right_join(., expand_grid(stations, GUILD = guilds)) %>% 
  arrange(YEAR, STATION_ID, GUILD) %>%
  mutate(CPUE_KGKM2 = replace_na(CPUE_KGKM2, 0)) %>%
  # annual mean cpue by guild
  group_by(YEAR, GUILD) %>%
  summarise(CPUE_KGKM2 = mean(CPUE_KGKM2)) 

ben_prey <- ben_prey %>%
  group_by(YEAR) %>%
  summarise(GUILD = "total_invert",
            CPUE_KGKM2 = sum(CPUE_KGKM2)) %>%
  rbind(ben_prey)

# Plot 
ben_prey %>%
  filter(!GUILD == "total_invert") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2, group = factor(GUILD))) +
  geom_point(aes(colour = GUILD)) +
  geom_line(aes(colour = GUILD)) +
  labs(y = "Benthic Prey CPUE (kg/km2)", x = "Year") +
  theme_bw() +
  theme(legend.title = element_blank())
# dominated by sponges, tunicates, crabs, stars

ben_prey %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2)) +
  geom_point() +
  geom_line() +
  labs(y = "CPUE (kg/km2)", x = "Year") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~GUILD, scales = "free_y", nrow = 4)

ben_prey %>%
  filter(GUILD == "total_invert") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mean(CPUE_KGKM2, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(CPUE_KGKM2, na.rm = TRUE) - sd(CPUE_KGKM2, na.rm = TRUE)), color = "green4") +
  geom_hline(aes(yintercept = mean(CPUE_KGKM2, na.rm = TRUE) + sd(CPUE_KGKM2, na.rm = TRUE)), color = "green4") +
  labs(y = "Benthic Invertebrate\nPrey CPUE (kg/km2)", x = "Year") +
  theme_bw() +
  theme(legend.title = element_blank()) 
#hmmm, sort of skeptical about earlier data. Let's subset to 1988

## Write .csv output of benthic prey density
missing <- data.frame(YEAR = 2020)

ben_prey %>%
  filter(YEAR >= 1988) %>%
  pivot_wider(names_from = GUILD, values_from = CPUE_KGKM2) %>%
  select(YEAR, total_invert) %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
  write.csv(file = "./Output/invert_density.csv", row.names = F)


