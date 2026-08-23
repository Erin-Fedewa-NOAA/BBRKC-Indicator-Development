#plot and write text file for fishery catch distance from shore indicator 

#read in setup and data 
source("./Scripts/make_indicator_text.R")
indicator_distance <- read.csv("./Output/Contributor indicators/indicator_fishery_offshore.csv")

#####################################
#plot
indicator_distance %>% 
  ggplot(aes(x = year, y = mean_distance_km)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = mean(mean_distance_km, na.rm = TRUE)))

###################################################
#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Annual_Red_King_Crab_Catch_Distance_Shore_BBRKC_Fishery"

##EDITABLE TEXT
description <- paste0("The mean distance legal male red king crab were caught from shore during the fishery, 
                      calculated using fishery observer data. During 2021 and 2022 fishery closures, mean 
                      distance from shore was estimated with satellite-tagged legal male red king crab, and 
                      calculated as the mean distance from shore for all tags east of 165°W. This boundary 
                      approximates the actual fishing grounds rather than the spatial allocation of tagging efforts. 
                      Proposed sign of the relationship is positive.")

status_trends <- paste0("The legal male red king crab catch during the 2025/26 BBRKC fishery was slightly further from shore 
                        than 2025, but still much closer to shore than the timeseries average. This is likely driven by 
                        high catches in the southern portion of the RKCSA.")

factors <- paste0("Red king crab tend to aggregate in the center of Bristol Bay and the Red King Crab Savings Area in warm years, 
                  and disperse along the Alaska Peninsula in cold years (Zacher et al., 2018)")

implications <- paste0("A spatial shift in fishing effort may be indicative of population-level distribution shifts from summer 
                       to fall. Distance from shore has implications for crab-fishing gear interactions, bycatch of crab, and 
                       unobserved mortality, most notably if BBRKC are found outside of protected areas.")

references <- paste0("Zacher, L. S., Kruse, G. H., and Hardy, S. M. 2018. Autumn distribution of Bristol Bay red king crab using fishery logbooks. PloS one, 13: 22.")


##INDICATOR DATA
indicator_data <- indicator_distance %>%
  rename(indicator = mean_distance_km)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)


