#Ratio of Northern District : Bristol Bay RKC abundance 

#Author: Erin Fedewa

## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

##############################################
# Calculate total abundance/biomass of BBRKC and Northern district RKC by category
bio <- calc_bioabund(crab_data = dat,
                     species = "RKC",
                     region = "EBS",
                     years = years)

#Plot BB
bio %>%
  filter(DISTRICT == "BB") %>%
  ggplot(aes(x = YEAR, y = ABUNDANCE)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#Plot Northern
bio %>%
  filter(DISTRICT == "NORTH") %>%
  ggplot(aes(x = YEAR, y = ABUNDANCE)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()


#join and calculate ratio
bio %>% 
  filter(DISTRICT %in% c("BB", "NORTH")) %>%
  select(YEAR, DISTRICT, ABUNDANCE) %>%
  pivot_wider(names_from = DISTRICT, values_from = ABUNDANCE) %>%
  mutate(ratio = NORTH/BB) -> rkc_ratio

#plot
rkc_ratio %>%
  ggplot(aes(YEAR, ratio)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=mean(ratio))) +
  theme_bw() 

##############################

#save output 

indicator_ratio <- rkc_ratio %>%
  select(YEAR, ratio) %>%
  rename_with(tolower) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)

write.csv(indicator_ratio, file="./Output/indicator_northern_ratio.csv", row.names = F)

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#indicator name
indicator_name <- "Summer_BBRKC_Northern_Ratio_SEBS_Survey"

##EDITABLE TEXT
description <- paste0("Calculated as the ratio (0 to 1) of total red king crab abundance in the Northern District to 
                      total red king crab abundance in the Bristol Bay Management District. Proposed sign of the 
                      relationship is negative.")

status_trends <- paste0("The ratio of Northern District red king crab abundance to Bristol Bay red king crab abundance 
                        has steadily declined since the time series high in 2021. The 2026 estimate decreased from 2025
                        and is slightly above the time series mean.")

factors <- paste0("The Northern District to Bristol Bay District ratio may be driven by shifts in larval advection 
                  patterns, or exchange between the two districts due to seasonal migrations or directional movement. 
                  Tagging studies suggest that males tagged just above the management boundary move south into Bristol 
                  Bay in the fall, but don’t rejoin the core Bristol Bay stock. Mature females tagged further north 
                  did not re-enter Bristol Bay during the study.")

implications <- paste0("A large increase in the abundance ratio between the two districts may coincide with northward 
                       range expansion of the Bristol Bay stock and movement outside of management boundaries during 
                       the summer survey period. Because red king crab in the Northern Unstratified District are not 
                       included in the BBRKC stock assessment, this indicator tracks a critical management concern.")

references <- paste0("")


##INDICATOR DATA
indicator_data <- indicator_ratio %>%
  rename(indicator = ratio)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)


