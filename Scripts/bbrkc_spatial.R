# notes ----
# Calculate "D95" for BBRKC size/sex groups in EBS 
    #area of stations that make up 95% of the cumulative cpue

# Erin Fedewa

# load ----
library(tidyverse)
library(mgcv)
library(patchwork)

## Read in setup
source("./Scripts/get_crab_data.R")

# function to compute D95
f_d95_est <- function(x){
  x %>%
    arrange(-CPUE) %>% #sort by cpue (large:small)
    mutate(prop_cpue = CPUE/sum(CPUE),  #calculate the proportion of total cpue for each station
           cum_cpue = cumsum(prop_cpue)) %>%  
    filter(cum_cpue <= 0.95) %>% #T if in d95, F if not
    count() %>%
    mutate(d95 = (n + 1) * 401) %>% #add 1 station to n to push over 95%
    pull(d95)
}

##########################################
#compute CPUE for mature males 
mat_male_cpue <- calc_cpue(crab_data = dat,
                    species = "RKC",
                    region = "EBS",
                    district = "BB",
                    years = years, 
                    sex = "male",
                    crab_category = "mature_male")

#compute d95
mat_male_cpue %>%
  select(-CATEGORY, -DISTRICT, -STRATUM, -TOTAL_AREA, -COUNT, -CPUE_LBS, -CPUE_MT) %>%
  nest(data = -YEAR) %>%
  mutate(d95 = purrr::map_dbl(data, f_d95_est)) %>% #apply d95 function to each element 
  unnest(cols = c(data)) %>%
  group_by(YEAR) %>%
  summarise(mean_cpue = mean(CPUE), # add a column for mean cpue of each group in each year
            d95 = mean(d95)) -> male_d95 # take 'mean' just to get one value 

#plot timeseries
male_d95 %>%
  ggplot(aes(x = YEAR, y = d95))+
  geom_point(size=3)+
  geom_line() 

#d95 vs. mean cpue plot
male_d95 %>%
  ggplot(aes(x = mean_cpue, y = d95)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm') +
  labs(x = "CPUE", y = expression("Area Occupied ("~nmi^2~")")) +
  theme_bw() +
  theme(legend.title = element_blank()) 

#d95 vs. bottom temperature plot
haul %>%
  filter(HAUL_TYPE != 17,
         DISTRICT == "BB") %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(summer_bt = mean(GEAR_TEMPERATURE, na.rm = T)) %>%
  right_join(male_d95, by="YEAR") %>%
  ggplot(aes(x = summer_bt, y = d95)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm') +
  labs(x = "Bottom Temperature (C)", y = expression("Area Occupied ("~nmi^2~")")) +
  theme_bw() +
  theme(legend.title = element_blank()) 

####################################################################
## compute cpue for mature females
  #NOTE: Not using re-tow data for females! 
mat_female_cpue <- calc_cpue(crab_data = dat,
                           species = "RKC",
                           region = "EBS",
                           district = "BB",
                           years = years, 
                           sex = "female",
                           crab_category = "mature_female",
                           replace_retow = F)

#compute d95
mat_female_cpue %>%
  select(-CATEGORY, -DISTRICT, -STRATUM, -TOTAL_AREA, -COUNT, -CPUE_LBS, -CPUE_MT) %>%
  nest(data = -YEAR) %>%
  mutate(d95 = purrr::map_dbl(data, f_d95_est)) %>% #apply d95 function to each element 
  unnest(cols = c(data)) %>%
  group_by(YEAR) %>%
  summarise(mean_cpue = mean(CPUE), # add a column for mean cpue of each group in each year
            d95 = mean(d95)) -> female_d95 # take 'mean' just to get one value 

#plot timeseries
female_d95 %>%
  ggplot(aes(x = YEAR, y = d95))+
  geom_point(size=3)+
  geom_line() 

#d95 vs. mean cpue plot
female_d95 %>%
  ggplot(aes(x = mean_cpue, y = d95)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm') +
  labs(x = "CPUE", y = expression("Area Occupied ("~nmi^2~")")) +
  theme_bw() +
  theme(legend.title = element_blank()) 

#d95 vs. bottom temperature plot
haul %>%
  filter(HAUL_TYPE != 17,
         DISTRICT == "BB") %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(summer_bt = mean(GEAR_TEMPERATURE, na.rm = T)) %>%
  right_join(female_d95, by="YEAR") %>%
  ggplot(aes(x = summer_bt, y = d95)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm') +
  labs(x = "Bottom Temperature (C)", y = expression("Area Occupied ("~nmi^2~")")) +
  theme_bw() +
  theme(legend.title = element_blank()) 

#interesting, only strong male d95/temperature/abundance relationships!

#########################################

#Save output
missing <- data.frame(YEAR = 2020)

female_d95 %>%
  select(-mean_cpue) %>%
  rename(mature_female_d95 = d95) %>%
  full_join(male_d95 %>%
              select(-mean_cpue) %>%
              rename(mature_male_d95 = d95), by="YEAR") %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  bind_rows(missing %>% mutate(YEAR = as.numeric(YEAR))) %>%
  arrange(YEAR) %>%
write.csv(file="./Output/D95_output.csv", row.names = F)

##################################################
#Not currently an indicator, but let's also look at mature 
  #male/female centroids of lat/long

#mature male
mat_male_cpue %>%
  group_by(YEAR) %>%
  summarise(Lat_COD = weighted.mean(LATITUDE, w = CPUE),
            Lon_COD = weighted.mean(LONGITUDE, w = CPUE),
            mean_cpue = mean(CPUE)) -> male_COD #add a column for mean cpue of each group in each year

#plot center of latitude
male_COD %>%
  ggplot(aes(x = YEAR, y = Lat_COD))+
  geom_point(size=3)+
  geom_line() +
  theme_bw() 

#plot center of longitude
male_COD %>%
  ggplot(aes(x = YEAR, y = Lon_COD))+
  geom_point(size=3)+
  geom_line() +
  theme_bw() 

#mature female
mat_female_cpue %>%
  group_by(YEAR) %>%
  summarise(Lat_COD = weighted.mean(LATITUDE, w = CPUE),
            Lon_COD = weighted.mean(LONGITUDE, w = CPUE),
            mean_cpue = mean(CPUE)) -> female_COD #add a column for mean cpue of each group in each year

#plot center of latitude
female_COD %>%
  ggplot(aes(x = YEAR, y = Lat_COD))+
  geom_point(size=3)+
  geom_line() +
  theme_bw() 

#plot center of longitude
female_COD %>%
  ggplot(aes(x = YEAR, y = Lon_COD))+
  geom_point(size=3)+
  geom_line() +
  theme_bw() 

