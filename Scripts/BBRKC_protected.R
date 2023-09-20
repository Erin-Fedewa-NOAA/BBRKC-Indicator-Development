# notes ----
#Quantify the proportion of RKC in protected areas - 2024 ESP indicator addition?
  #Methodology: Protected areas are all stations where at least half of their area is in a 
  #year-round non-pelagic trawl closure (ie not including stations in Togiak trawl area or Area 516, 
  #which are only protected for part of the year). Savings Subarea only included in years when BBRKC 
  #directed fishery closed 

# Erin Fedewa
# last updated: 2023/9/22 

#NOTE: need to account for years with fishery closures when savings subarea is closed 
  #see lookup table
# Replace retow stations for females!! (confirm results visually with tech memo maps)

# load ----
library(tidyverse)
library(mgcv)

# data ----
## EBS RKC Haul data 
crab_ebs <- read.csv("./Data/crabhaul_rkc.csv") %>% as_tibble()

#Protected station look-up table (see metadata for how this was created)
sta <- read.csv("./Data/protected_RKC_stations.csv") %>% as_tibble()

#Station look up for protected stations
sta %>% 
  pull(Protected) -> prot

#Station look up for unprotected stations
sta %>% 
  pull(Not_protected) -> unprot

#calculate number of RKC at each station 
crab_ebs %>% 
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(LENGTH_1MM >= 0,
         HAUL_TYPE != 17,
         YEAR > 1996) %>%
  mutate(prot = ifelse(GIS_STATION %in% prot, "protected", 
                           ifelse(GIS_STATION %in% unprot, "unprotected",NA))) %>% 
  filter(prot != "NA") %>%
  group_by(YEAR, GIS_STATION, MID_LATITUDE, MID_LONGITUDE, AREA_SWEPT, prot) %>%
  summarise(num_crab = round(sum(SAMPLING_FACTOR))) %>%
  mutate(num_crab = replace_na(num_crab, 0)) %>%
  group_by(YEAR) %>%
  summarise(ratio = ((sum(num_crab[prot=="protected"]))/(sum(num_crab))*100)) ->prot_rkc
  
#Plot
prot_rkc %>%
  ggplot(aes(YEAR, ratio, group=1)) +
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(ratio, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(ratio, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(ratio, .90, na.rm=TRUE)), linetype = 3)+
  labs(y = "% of population protected", x = "") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("% of RKC population in protected areas")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  scale_x_discrete(breaks = seq(1980, 2024, 3)) +
  theme(axis.text=element_text(size=12)) 
  