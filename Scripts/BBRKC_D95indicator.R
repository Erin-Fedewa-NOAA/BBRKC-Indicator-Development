# notes ----
# Calculate "D95" for BBRKC size/sex groups in EBS 
    #area of stations that make up 95% of the cumulative cpue

# Erin Fedewa
# last updated: 2022/8/26

# load ----
library(tidyverse)
library(mgcv)

# Data ----

## EBS RKC Haul data 
crab_ebs <- read.csv("./Data/crabhaul_rkc.csv") %>% as_tibble()

#Create look up table with BBRKC stations 
sta <- read_csv("./Data/crabstrata_rkc.csv")
sta %>% 
  filter(SURVEY_YEAR==2021,
         #Selecting a yr when entire grid was sampled
         DISTRICT == "Bristol Bay") %>% 
  pull(STATION_ID) -> BBonly

#Retow stations
crab_ebs %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(GIS_STATION %in% BBonly) %>%
  group_by(YEAR, HAUL_TYPE) %>%
  summarise(num_stations = nrow(GIS_STATION))%>%
  print(n=60) #Re-tow years 1999, 2000, 2006-12, 2017, 2021


## compute cpue for mature males/females at each station, no re-tow data for females 
crab_ebs %>% 
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(LENGTH_1MM >= 0,
         HAUL_TYPE ==3,
         GIS_STATION %in% BBonly) %>%
  mutate(size_sex = ifelse(SEX == 1 & LENGTH_1MM >= 120, "mature_male", 
                           ifelse(SEX == 2 & CLUTCH_SIZE >= 1, "mature_female",NA))) %>% 
  group_by(YEAR, GIS_STATION, MID_LATITUDE, MID_LONGITUDE, AREA_SWEPT, size_sex) %>%
  summarise(num_crab = round(sum(SAMPLING_FACTOR))) %>%
  filter(!is.na(AREA_SWEPT),
          size_sex != "NA") %>%
  mutate(num_crab = replace_na(num_crab, 0),
         cpue = num_crab / AREA_SWEPT ) %>%
  ungroup() -> cpue_long

# compute D95 for mature males and females ----
# i.e. the number of stations contributing to 95% of cumulative cpue, from stations sampled in every year

# function to compute D95
f_d95_est <- function(x){
  x %>%
    arrange(-cpue) %>% #sort by cpue (large:small)
    mutate(prop_cpue = cpue/sum(cpue),  #calculate the proportion of total cpue for each station
           cum_cpue = cumsum(prop_cpue)) %>%  
    filter(cum_cpue <= 0.95) %>% #T if in d95, F if not
    count() %>%
    mutate(d95 = (n + 1) * 401) %>% #add 1 station to n to push over 95%
    pull(d95)
}

# do the estimation
cpue_long %>%
  nest(-YEAR, -size_sex) %>%
  mutate(d95 = purrr::map_dbl(data, f_d95_est)) %>% #apply d95 function to each element 
  unnest() %>%
  group_by(YEAR, size_sex) %>%
  summarise(cpue = sum(num_crab) / sum(AREA_SWEPT), # add a column for total cpue of each group in each year
            d95 = mean(d95)) -> d95 # take 'mean' just to get one value (they are all the same)

# Plot 
d95 %>%
  ggplot(aes(YEAR, d95, group = as.factor(size_sex))) +
  geom_point(aes(colour = size_sex)) +
  geom_line(aes(colour = size_sex)) 
  #geom_smooth(method = gam, formula = y~s(x))
#Need to overlay this w/ abundance/biomass estimates

#Save output
d95 %>%
  select(-cpue) %>%
  pivot_wider(names_from = size_sex, values_from = d95) ->D95
write.csv(D95, file="./Output/D95_output.csv")

