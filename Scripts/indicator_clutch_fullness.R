#Calculate proportion empty clutches in mature female RKC

#Author: Erin Fedewa

#NOTE: Resampling wasn't started until 1999, but we're replacing data in any 
  #year onward that resampling did occur in an attempt to look at clutch failures in the
  #largest portion of the population that has molted and mated. 

#TO DO: Overlay with Kodiak RKC clutch failure data? I think this is Resolution data but need 
  #to confirm that clutch codes are classified using the same methods 

## Read in setup and functions
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

########################################
#calculate abundance of all mature newshell females
mature <- dat
mature$specimen <- mature$specimen %>%
  filter(SEX == 2,
         CLUTCH_SIZE > 0,
         SHELL_CONDITION == 2)

mature <- calc_bioabund(crab_data = mature,
                        species = "RKC",
                        region = "EBS",
                        district = "BB",
                        years = years,
                        replace_retow = T) %>%
  mutate(mature_abun = ABUNDANCE) %>%
  select(YEAR, mature_abun)

# calculate abundance of just mature newshell females with empty clutches
barren <- dat
barren$specimen <- barren$specimen %>%
  filter(SEX == 2,
         SHELL_CONDITION == 2,
         EGG_CONDITION %in% c(0,3,4),
         CLUTCH_SIZE > 0) 

barren <- calc_bioabund(crab_data = barren,
                        species = "RKC",
                        region = "EBS",
                        district = "BB",
                        years = years,
                        replace_retow = T) %>%
  mutate(barren_abun = ABUNDANCE) %>%
  select(YEAR, barren_abun)

# calculate proportion empty clutches
prop_empty <- mature %>%
  full_join(barren) %>%
  mutate(prop_empty = (barren_abun/mature_abun) *100) %>% 
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) 

#plot proportion empty
prop_empty %>%
  ggplot(aes(x= YEAR, y=prop_empty)) +
  geom_point() + 
  geom_line() +
  geom_hline(aes(yintercept = mean(prop_empty, na.rm=TRUE)), linetype = 5) +
  theme_bw() 

############################################

#write output
indicator_clutch <- prop_empty %>%
  select(YEAR, prop_empty) %>%
  rename_with(tolower) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)

write.csv(indicator_clutch, file="./Output/indicator_clutch_fullness.csv", row.names = F)

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_BBRKC_Female_Reproductive_Failure_SEBS_Survey"

##EDITABLE TEXT
description <- paste0("The proportion of newshell mature female snow crab with no eggs, 
                      empty egg cases, or dead eggs. Because cold temperatures delay the 
                      female molt/mate cycle, oldshell barren females are excluded under 
                      the assumption that they will likely molt and mate in the coming months 
                      after survey sampling. Proposed sign of the relationship is negative")

status_trends <- paste0("The proportion of females with empty clutches decreased from 2025 to 2026. 
                        Of the females that had molted and mated at the time of the survey, only ~1% 
                        had empty clutches.")

factors <- paste0("Female reproductive potential is a function of female size, clutch size, 
                        and sperm reserves. Increases in the proportion of mature females with 
                        empty clutches suggest that females were unable to find a mate.")

implications <- paste0("An increase in the proportion of mature females with empty clutches suggests 
                        a reduction in reproductive potential of the stock. A > 30% proportion of mature 
                        females with empty clutches in the late 1970’s coincided with a stock collapse, 
                        suggesting that clutch failures should be continually monitored for under depressed
                        population levels.")

references <- paste0("")


##INDICATOR DATA
indicator_data <- indicator_clutch %>%
  rename(indicator = prop_empty)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)
