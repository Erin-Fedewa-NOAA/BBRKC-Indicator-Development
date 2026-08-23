# notes ----
# Generate avg bottom temp time series within Bristol Bay and 
  #date correct for variation in survey timing

# Erin Fedewa

# load ----
library(tidyverse)
library(mgcv)

## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

#########################################################

# compute observed average summer bottom temperature in BB 
avg_bt <- haul %>%
  mutate(julian=yday(parse_date_time(START_DATE, "ymd", "US/Alaska"))) %>%
  filter(YEAR > 1978, #earlier years are missing stations
         HAUL_TYPE != 17,
         DISTRICT == "BB") %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(observed_temp = mean(GEAR_TEMPERATURE, na.rm = T)) 

#Plot
avg_bt %>%
  ggplot(aes(x = as.numeric(YEAR), y = observed_temp)) +
  geom_point() +
  geom_line()+
  labs(y = "Bottom temperature (C)", x = "") +
  theme_bw()

#Timing of survey sampling by year (b/c this impacts average temp)
sampling_dates <- haul %>%
  mutate(julian=yday(parse_date_time(START_DATE, "ymd", "US/Alaska"))) %>%
  filter(YEAR > 1978,
         HAUL_TYPE != 17,
         DISTRICT == "BB") %>%
  group_by(YEAR) %>%
  summarise(mean_date = mean(julian, na.rm=T),
            min_date = min(julian, na.rm=T),
            max_date = max(julian, na.rm=T))

#Plot
#Points show mean sampling date; vertical ranges show min/max
ggplot(sampling_dates, aes(x = YEAR)) +
  geom_linerange(aes(ymin = min_date, ymax = max_date)) +
  geom_point(aes(y = mean_date), size = 2) +
  geom_line(aes(y = mean_date)) +
  labs(x = "Year", y = "Julian day",
       title = "Timing of survey sampling by year") +
  theme_classic()

#Define mean reference survey date and predict each year's temperature 
#at that date to control for differences in timing of sampling
temp.dat <- haul %>%
  filter(YEAR > 1978,
         HAUL_TYPE != 17,
         DISTRICT == "BB") %>%
  mutate(julian=yday(parse_date_time(START_DATE, "ymd", "US/Alaska")),
         YEAR = as.factor(YEAR)) %>%
  filter(!is.na(GEAR_TEMPERATURE),
         !is.na(julian)) 

ref_date <- mean(temp.dat$julian, na.rm = TRUE)

temp.mod <- gam(GEAR_TEMPERATURE ~ YEAR + s(julian, k = 5) + YEAR, 
                data = temp.dat, method = "REML")

summary(temp.mod) 
gam.check(temp.mod)  
plot(temp.mod, pages = 1, shade = TRUE)

#create a prediction dataset to estimate mean bottom temperature in 
  #each year on June 14th
pred.dat <- data.frame(
  YEAR = factor(levels(temp.dat$YEAR),
                levels = levels(temp.dat$YEAR)),
  julian = ref_date)

pred <- predict(temp.mod, newdata = pred.dat,
                se.fit = TRUE)

date_temp <- pred.dat %>%
  mutate(YEAR = as.numeric(as.character(YEAR)),
         date_corrected_temp = pred$fit,
         se = pred$se.fit,
         lower = date_corrected_temp - 1.96 * se,
         upper = date_corrected_temp + 1.96 * se) %>%
  left_join(avg_bt, by = "YEAR")

#plot
date_temp_long <- date_temp %>%
  select(YEAR, observed_temp, date_corrected_temp) %>%
  pivot_longer(cols = c(observed_temp, date_corrected_temp),
               names_to = "temperature_type",
               values_to = "temperature")

ggplot(date_temp_long,
       aes(x = YEAR, y = temperature,
           color = temperature_type,
           group = temperature_type)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = mean(temperature[temperature_type == "date_corrected_temp"], na.rm=TRUE)), linetype = 5) 
#very similar! 

#######################################

#Save output

indicator_temp <- date_temp %>% 
  select(YEAR, date_corrected_temp) %>%
  rename_with(tolower) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)

write.csv(indicator_temp, file= "./Output/indicator_bottom_temp.csv", row.names = F)

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

indicator_name <- "Summer_Temperature_Bottom_BBRKC_Survey"

##EDITABLE TEXT
description <- paste0("Estimated average summer bottom temperature in Bristol Bay from the EBS bottom trawl survey. Bottom
                      temperatures are standardized to June 14th of each year to account for differences in survey sampling dates among years.
                      Proposed sign of the relationship is negative.")

status_trends <- paste0("The mean bottom temperature in Bristol Bay was well below average in 2026, and one of the coldest years
                        in the survey time series.")

factors <- paste0("Bottom temperatures in the Bering Sea are driven by winter sea ice extent and winds, 
                  and summer cold pool formation.")

implications <- paste0("Strong year classes of BBRKC in the early 1970’s corresponded with low bottom 
                       temperatures (Zheng and Kruse 2000).")

references <- paste0("Zheng, J., and Kruse, G. 2000. Recruitment patterns of Alaskan crabs in relation to decadal shifts in climate and physical oceanography. ICES Journal of Marine Science, 57: 438.")


##INDICATOR DATA
indicator_data <- indicator_temp %>% 
  rename(indicator = date_corrected_temp)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)
