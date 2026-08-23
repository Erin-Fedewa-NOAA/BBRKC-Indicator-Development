# notes ----
# Calculate "D95" for BBRKC size/sex groups in EBS 
    #area of stations that make up 95% of the cumulative cpue

# Erin Fedewa

## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

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
  geom_line() +
  geom_hline(aes(yintercept = mean(d95, na.rm=TRUE)), linetype = 5) 

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
  geom_line() +
  geom_hline(aes(yintercept = mean(d95, na.rm=TRUE)), linetype = 5) 

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
indicator_d95 <- female_d95 %>%
  select(-mean_cpue) %>%
  rename(mature_female_d95 = d95) %>%
  full_join(male_d95 %>%
              select(-mean_cpue) %>%
              rename(mature_male_d95 = d95), by="YEAR") %>%
  rename_with(tolower) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)

write.csv(indicator_d95, file="./Output/indicator_d95.csv", row.names = F)

############################################
#MATURE MALES - #WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

indicator_name <- "Summer_Red_King_Crab_Male_Area_Occupied_BBRKC_Model"

##EDITABLE TEXT
description <- paste0("The minimum area containing 95% of the cumulative mature male red king crab CPUE in the 
                      BBRKC management district during the EBS summer bottom trawl survey.  Proposed sign of 
                      the relationship is positive.")

status_trends <- paste0("The spatial extent of mature male BBRKC increased slightly from 2025 to 2026 and is 
                        above the time series mean.")

factors <- paste0("Red king crab spatial distributional shifts have been associated with changes in bottom 
                  temperatures (Loher and Armstrong, 2005; Zacher et al., 2018).")

implications <- paste0("The range expansion of mature male red king crab during the past decade has been 
                       associated with a northward shift in centroids of abundance. Range expansion may 
                       suggest that mature males are tracking thermal habitat preferences in response to 
                       recent warming in the Bering Sea.")

references <- paste0("Loher, T., and Armstrong, D. A. 2005. Historical changes in the abundance and distribution of ovigerous red king crabs (Paralithodes camtschaticus) in Bristol Bay (Alaska), and potential relationship with bottom temperature. Fisheries Oceanography, 14: 292-306.
                      Zacher, L. S., Kruse, G. H., and Hardy, S. M. 2018. Autumn distribution of Bristol Bay red king crab using fishery logbooks. PloS one, 13: 22.")


##INDICATOR DATA
indicator_data <- indicator_d95 %>%
  select(year, mature_male_d95) %>%
  rename(indicator = mature_male_d95)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)


############################################
#MATURE FEMALES - #WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

indicator_name <- "Summer_Red_King_Crab_Female_Area_Occupied_BBRKC_Model"

##EDITABLE TEXT
description <- paste0("The minimum area containing 95% of the cumulative mature female red king crab CPUE in the 
                      BBRKC management district during the EBS summer bottom trawl survey.  Proposed sign of 
                      the relationship is positive.")

status_trends <- paste0("The spatial extent of mature female BBRKC increased from 2025 to 2026 and is above the time series mean")

factors <- paste0("Northerly shifts in stock distribution are generally associated with both warmer temperatures and high Pacific 
                  Decadal Oscillation values during the summer, and mature female RKC appear to avoid waters < 2°C 
                  (Loher and Armstrong, 2005; Zheng and Kruse, 2006).")

implications <- paste0("Spatial distribution shifts may result in more mature females inhabiting the Nearshore Bristol Bay Trawl 
                       Closure Area, although continued range contraction may raise concern for increased competition for resources.")

references <- paste0("Loher, T., and Armstrong, D. A. 2005. Historical changes in the abundance and distribution of ovigerous red king crabs (Paralithodes camtschaticus) in Bristol Bay (Alaska), and potential relationship with bottom temperature. Fisheries Oceanography, 14: 292-306.
                      Zheng, J., and Kruse, G. 2000. Recruitment patterns of Alaskan crabs in relation to decadal shifts in climate and physical oceanography. ICES Journal of Marine Science, 57: 438.")


##INDICATOR DATA
indicator_data <- indicator_d95 %>%
  select(year, mature_female_d95) %>%
  rename(indicator = mature_female_d95)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)




