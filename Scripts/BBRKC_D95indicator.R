# notes ----
# Calculate "D95" for BBRKC size/sex groups in EBS 
    #area of stations that make up 95% of the cumulative cpue

# Erin Fedewa

#2025 follow up: explore different metric for d95?

# load ----
library(tidyverse)
library(mgcv)
library(patchwork)

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
  print(n=65) #Re-tow years 1999, 2000, 2006-12, 2017, 2021


## compute cpue for mature males/females at each station, no re-tow data for females 
crab_ebs %>% 
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(LENGTH_1MM >= 0,
         HAUL_TYPE != 17,
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
  summarise(num_crab = sum(num_crab), #add total abundance
            d95 = mean(d95))  -> d95 # take 'mean' just to get one value (they are all the same)

# Plot 
d95 %>%
  filter(YEAR > 1981) %>%
  ggplot(aes(YEAR, d95, group = as.factor(size_sex))) +
  geom_point(aes(colour = size_sex)) +
  geom_line(aes(colour = size_sex)) +
  theme_bw() +
  scale_x_discrete(breaks=seq(1975,2023,6)) -> d95plot
  #geom_smooth(method = gam, formula = y~s(x))

#Overlay d95 with abundance
d95 %>%
  filter(YEAR > 1981) %>%
  ggplot(aes(YEAR, num_crab, group = as.factor(size_sex))) +
  geom_point(aes(colour = size_sex)) +
  geom_line(aes(colour = size_sex)) +
  theme_bw() +
  scale_x_discrete(breaks=seq(1975,2023,6)) -> abunplot
  
#combine plots 
d95plot/abunplot

#just male plot 
d95 %>%
  filter(YEAR >= 1980,
         size_sex == "mature_male") %>%
  ggplot(aes(as.numeric(YEAR), d95)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=mean(d95))) +
  theme_bw() 

#Save output
missing <- data.frame(YEAR = 2020)

d95 %>%
  select(-num_crab) %>%
  pivot_wider(names_from = size_sex, values_from = d95) %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  bind_rows(missing %>% mutate(YEAR = as.numeric(YEAR))) %>%
  arrange(YEAR) %>%
write.csv(file="./Output/D95_output.csv")

