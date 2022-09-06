# notes ----
# Pcod/BKC Spatial Overlap in BBRKC managment distict 
# Erin Fedewa

# Last Updated 8/24/22

# load ----

library(tidyverse)
library(cowplot)
library(mgcv)
library(latticeExtra)

# data ----

#Add all EBS bottom trawl data files
ebs82 <- read_csv("./Data/ebs1982_1984.csv")
ebs85 <- read_csv("./Data/ebs1985_1989.csv")
ebs90 <- read_csv("./Data/ebs1990_1994.csv")
ebs95 <- read_csv("./Data/ebs1995_1999.csv")
ebs00 <- read_csv("./Data/ebs2000_2004.csv")
ebs05 <- read_csv("./Data/ebs2005_2008.csv")
ebs09 <- read_csv("./Data/ebs2009_2012.csv")
ebs13 <- read_csv("./Data/ebs2013_2016.csv")
ebs17 <- read_csv("./Data/ebs2017_2018.csv")
ebs19 <- read_csv("./Data/ebs2019.csv")
ebs21 <- read_csv("./Data/Groundfish Catch Data/ebs2021.csv")

# combine datasets now and save output
bind_rows(ebs82, ebs85, ebs90, ebs95, ebs00, ebs05, ebs09, ebs13, ebs17, ebs19, ebs21) %>%
  write_csv("./Output/ebs_timeseries.csv")
ebs <- read_csv("./Output/ebs_timeseries.csv")

#Create look up table with BBRKC stations 
sta <- read_csv("./Data/crabstrata_rkc.csv")
sta %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly


#Calculate % overlap for BB timeseries 
ebs %>%
  filter(STATION %in% BBonly) %>%
  group_by(YEAR) %>% 
  mutate(TOTAL_STATIONS = n_distinct(STATION)) %>%
  filter(SID %in% c("21720", "69322")) %>%
  select(YEAR, STATION, SID, WTCPUE, TOTAL_STATIONS) %>%
  mutate(SID = case_when(SID == 69322 ~ "CRAB",
                         SID == 21720 ~ "COD")) %>%
  group_by(YEAR, STATION, SID) %>%
  pivot_wider(names_from = SID, values_from = WTCPUE) %>%
  group_by(YEAR) %>%
  # method 1 -  % of total stations that include both cod and snow crab
  # method 2 - % of positive snow crab stations that included cod
  summarise(METHOD_1 = sum((CRAB > 0 & COD > 0), na.rm = T) / mean(TOTAL_STATIONS) * 100,
            METHOD_2 = sum((CRAB > 0 & COD > 0), na.rm = T) / sum((CRAB > 0), na.rm = T) * 100) -> overlap

#Plot Method 1
ggplot(aes(x = YEAR, y = METHOD_1), data = overlap) +
  geom_point() +
  geom_line() +
  labs(y = expression(atop("BBRKC Pacific cod spatial overlap (%)")), x = "")+
  theme_bw()

#Plot Method 2
ggplot(aes(x = YEAR, y = METHOD_2), data = overlap) +
  geom_point() +
  geom_line() +
  labs(y = expression(atop("BBRKC Pacific cod spatial overlap (%)")), x = "")+
  theme_bw()

#Overlap is near or at 100% in most years- probably not a very meaningful index
