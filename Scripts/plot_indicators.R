#Create master csv of ecosystem indicators 
# Assess collinearity b/w snow crab indicators for BAS
#Create indicator timeseries plot

#2026 to do: add in trend analysis to communicate on report card plots as symbols
#analysis to detect trend (change in timeseries mean) or regime-like behavior


# Erin Fedewa

# load ----
library(tidyverse)
library(corrplot)
library(patchwork)
library(mgcv)
library(ggdist)
library(scales)

#Ecosystem data to combine
invert <- read_csv("./Output/BBbenthic_timeseries.csv")
pred <- read_csv("./Output/BBpred_timeseries.csv")
env <- read_csv("./Output/environmental_timeseries.csv")
d95 <- read_csv("./Output/D95_output.csv")
protected <- read_csv("./Output/BBRKC_proportion_closure.csv")
clutch <- read_csv("./Output/clutch_fullness.csv")
northern <- read_csv("./Output/northern_BB_ratio.csv")
salmon <- read_csv("./Data/Contributor indicators/BB_Sockeye_Inshore_Run_Size_2024.csv")
distance <- read_csv("./Data/Contributor indicators/Legal_Male_Dist_Shore.csv")
wind <- read_csv("./Data/Contributor indicators/Wind Stress.csv")
chla <- read_csv("./Data/Contributor indicators/spring_Chlorophylla_Biomass.csv")

# Set years for plotting
current_year <- 2025

#########################################################

# combine indices and save output
invert %>%
  select(YEAR, Total_Benthic) %>%
  rename(benthic_invert_density = Total_Benthic) %>%
  full_join(pred %>%
              select(YEAR, Pcod) %>%
              rename(pcod_density = Pcod)) %>%
  full_join(env %>%
              select(YEAR, summer_bt, Mean_AO)) %>%
  full_join(d95) %>% 
  full_join(salmon) %>%
  full_join(protected %>%
              rename(prop_closed_area = PROP_CLOSED)) %>%
  full_join(clutch) %>%
  full_join(northern) %>%
  full_join(distance %>%
              rename(YEAR = year) %>%
              select(YEAR, mean_distance_shore_km)) %>%
  full_join(wind) %>%
  full_join(chla) %>%
  arrange(YEAR) %>%
  rename(year = YEAR) -> eco_ind
#pH not included

write_csv(eco_ind, "./Output/BBRKC_esp_indicator_timeseries.csv")

#Assess collinearity b/w indicators 
eco_ind %>% 
  select(-year) %>%
  cor(use = "complete.obs") %>%
  corrplot(method="number")

################################################
#Ecosystem Plots ----

## Arctic Oscillation
eco_ind %>%
  ggplot(aes(x = year, y = Mean_AO ))+
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = mean(Mean_AO, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(Mean_AO, na.rm = TRUE) - sd(Mean_AO, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(Mean_AO, na.rm = TRUE) + sd(Mean_AO, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin= (current_year - 0.5),xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Deviation", x = "") +
  scale_x_continuous(breaks = seq(1978,current_year, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Arctic Oscillation")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/arctic_oscillation.png")

##Chl-a
eco_ind %>%
  ggplot(aes(x = year, y = chla ))+
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mean(chla, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(chla, na.rm = TRUE) - sd(chla, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(chla, na.rm = TRUE) + sd(chla, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin= (current_year - 0.5),xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Biomass (µg/L)", x = "") +
  scale_x_continuous(breaks = seq(1998,current_year, 5), limits= c(1988, current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Spring Chlorophyll a Biomass")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/chla.png")

## Wind Stress
eco_ind %>%
  ggplot(aes(x = year, y = wind_stress ))+
  geom_point(size=3)+
  geom_line() +
  geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(wind_stress, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(wind_stress, na.rm = TRUE) - sd(wind_stress, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(wind_stress, na.rm = TRUE) + sd(wind_stress, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin= (current_year - 0.5),xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "m/s", x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Wind Stress")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/wind_stress.png")

##Sockeye
eco_ind %>%
  ggplot(aes(x = year, y = inshore_run_size))+
  geom_point(size=3)+
  geom_line() +
  geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(inshore_run_size, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(inshore_run_size, na.rm = TRUE) - sd(inshore_run_size, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(inshore_run_size, na.rm = TRUE) + sd(inshore_run_size, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "millions of sockeye", x = "") +
  scale_x_continuous(breaks = seq(1960, current_year, 5), limits=c(1960,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Bristol Bay Sockeye Run Size")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/sockeye.png")

## Pcod CPUE
eco_ind %>%
  ggplot(aes(x = year, y = pcod_density ))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(pcod_density, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(pcod_density, na.rm = TRUE) - sd(pcod_density, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(pcod_density, na.rm = TRUE) + sd(pcod_density, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Pacific Cod density (kg/km^2)", x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Pacific Cod Predator Density")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/pcod.png")

## Invert CPUE
eco_ind %>%
  ggplot(aes(x = year, y = benthic_invert_density ))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(benthic_invert_density, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(benthic_invert_density, na.rm = TRUE) - sd(benthic_invert_density, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(benthic_invert_density, na.rm = TRUE) + sd(benthic_invert_density, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Benthic Invert density (kg/km^2)", x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Benthic Invertebrate Density")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/benthic_invert.png")

#Summer Bottom Temp  
eco_ind %>%
  ggplot(aes(x = year, y = summer_bt)) +
  geom_point(size=3)+
  geom_line() +
  geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(summer_bt, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(summer_bt, na.rm = TRUE) - sd(summer_bt, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(summer_bt, na.rm = TRUE) + sd(summer_bt, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression("Bottom Temperature ("*~degree*C*")"), x = "") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1982, current_year, 5), limits=c(1982,current_year)) +
  theme(panel.grid = element_blank()) +
  ggtitle("Summer Bottom Temperature")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  theme(axis.text=element_text(size=12))  
ggsave("./Figs/temperature.png")

## Mature female area occupied  
eco_ind %>%
  ggplot(aes(x = year, y = mature_female_d95))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(mature_female_d95, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(mature_female_d95, na.rm = TRUE) - sd(mature_female_d95, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(mature_female_d95, na.rm = TRUE) + sd(mature_female_d95, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Mature Female", "Area Occupied (nmi)")), x = "")+
  scale_x_continuous(breaks = seq(1982, current_year, 5), limits=c(1982,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Mature Female Snow Crab Area Occupied")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/female_d95.png")

## Mature male Area Occupied
eco_ind %>%
  ggplot(aes(x = year, y =mature_male_d95))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(mature_male_d95, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(mature_male_d95, na.rm = TRUE) - sd(mature_male_d95, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(mature_male_d95, na.rm = TRUE) + sd(mature_male_d95, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Mature Male", "Area Occupied (nmi)")), x = "")+
  scale_x_continuous(breaks = seq(1982, current_year, 5), limits=c(1982,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Mature Male Snow Crab Area Occupied")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/male_d95.png")

#Proportion in Closed Area
eco_ind %>%
  ggplot(aes(x = year, y =prop_closed_area))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(prop_closed_area, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(prop_closed_area, na.rm = TRUE) - sd(prop_closed_area, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(prop_closed_area, na.rm = TRUE) + sd(prop_closed_area, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Proportion of Mature Males (%)", x = "")+
  scale_x_continuous(breaks = seq(1982, current_year, 5), limits=c(1982,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Proportion of Mature Males in Closure Areas")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/protected.png")

#Catch Distance from Shore
eco_ind %>%
  ggplot(aes(x = year, y =mean_distance_shore_km))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(mean_distance_shore_km, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(mean_distance_shore_km, na.rm = TRUE) - sd(mean_distance_shore_km, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(mean_distance_shore_km, na.rm = TRUE) + sd(mean_distance_shore_km, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "km", x = "")+
  scale_x_continuous(breaks = seq(1999, current_year, 5), limits=c(1999,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Fishery Catch Distance from Shore")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/distance_shore.png")

#Proportion of empty clutches
eco_ind %>%
  ggplot(aes(x = year, y = prop_empty))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(prop_empty, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(prop_empty, na.rm = TRUE) - sd(prop_empty, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(prop_empty, na.rm = TRUE) + sd(prop_empty, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "% empty clutches", x = "")+
  scale_x_continuous(breaks = seq(1982, current_year, 5), limits=c(1982,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Mature Female Reproductive Failure")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/empty_clutch.png")

#Northern District Ratio
eco_ind %>%
  ggplot(aes(x = year, y =bbrkc_northern_ratio))+
  geom_point(size=3)+
  geom_line() +
  geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(bbrkc_northern_ratio, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(bbrkc_northern_ratio, na.rm = TRUE) - sd(bbrkc_northern_ratio, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(bbrkc_northern_ratio, na.rm = TRUE) + sd(bbrkc_northern_ratio, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Northern District/BBRKC ratio", x = "")+
  scale_x_continuous(breaks = seq(1982, current_year, 5), limits=c(1982,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Northern District RKC ratio")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/northern_ratio.png")

####################################################################
#Socioeconomic Indicators

cpue <- read_csv("./Data/Contributor Indicators/Annual_Red_King_Crab_CPUE_BBRKC_Fishery_2025.csv")
potlift <- read_csv("./Data/Contributor Indicators/Annual_Red_King_Crab_Total_Potlift_BBRKC_Fishery_2025.csv")

#Fishery CPUE
cpue %>%
  ggplot(aes(x = YEAR, y = cpue_fishery))+
  geom_point(size=3)+
  geom_line() +
  geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(cpue_fishery, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(cpue_fishery, na.rm = TRUE) - sd(cpue_fishery, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(cpue_fishery, na.rm = TRUE) + sd(cpue_fishery, na.rm = TRUE)), linetype = 3) +
  labs(y = "CPUE", x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("BBRKC Fishery CPUE")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/fishery_cpue.png")

#No of Potlifts
potlift %>%
  ggplot(aes(x = YEAR, y = potlift))+
  geom_point(size=3)+
  geom_line() +
  geom_smooth(method = "lm", color = "grey40", fill="grey80") + 
  geom_hline(aes(yintercept = mean(potlift, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(potlift, na.rm = TRUE) - sd(potlift, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(potlift, na.rm = TRUE) + sd(potlift, na.rm = TRUE)), linetype = 3) +
  labs(y = "Number of Potlifts", x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("BBRKC Fishery Potlifts")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) 
ggsave("./Figs/fishery_potlifts.png")

#skipper survey plots
skipper <- read_csv("./Data/Contributor Indicators/Skipper Survey Q1_Q3.csv")

#question 1: perceived abundance
color_palette <- c("red" = "red", "blue" = "blue", "grey" = "grey")

skipper %>% 
  filter(stock == "bbrkc",
         question == "perceived_abundance") %>%
  mutate(bar_color = case_when(response %in% c("10_25_decrease", "25_plus_decrease") ~ "red",
                               response %in% c("no_change") ~ "grey",
                               response %in% c("10_25_increase","25_plus_increase") ~ "blue")) %>%
  mutate(response = factor(response, 
                           levels = c("25_plus_decrease", "10_25_decrease",
                                      "no_change","10_25_increase",
                                      "25_plus_increase"))) %>%
  ggplot(aes(number_responses, response, fill = bar_color)) +
  geom_bar(stat = "identity", alpha = .8) +
  scale_fill_manual(values = color_palette) +
  scale_y_discrete(labels = c("25_plus_decrease" = "25%+ Decrease", "10_25_decrease" = "10-25% Decrease", 
                              "no_change" = "No Change",
                              "10_25_increase" = "10-25% Increase", "25_plus_increase" = "25%+ Increase")) +
  labs(x = "Number of Responses", y = "") +
  theme_bw() +
  theme(legend.position = "none")

#question 2: changes in fishing behavior
skipper %>% 
  filter(stock == "bbrkc",
         question == "fishing_practice") %>%
  ggplot(aes(number_responses, response)) +
  geom_bar(stat = "identity", alpha = .8) +
  scale_y_discrete(labels = c("no_change" = "No Change", "move_location" = "Moved Fishing Locations", 
                              "longer_soak" = "Longer Soak Times",
                              "less_test_pots" = "Less Test Pots", "increase_communication" = "More Communication with Fleet")) +
  labs(x = "Number of Responses", y = "") +
  theme_bw() +
  theme(legend.position = "none")

#question 3: motivation for changes in fishing behavior
skipper %>% 
  filter(stock == "bbrkc",
         question == "reason_change") %>%
  ggplot(aes(number_responses, response)) +
  geom_bar(stat = "identity", alpha = .8) +
  scale_y_discrete(labels = c("no_change" = "No Change", "low_cpue" = "Low CPUE", 
                              "high_discard" = "Too Much Sorting",
                              "high_cpue" = "High CPUE")) +
  labs(x = "Number of Responses", y = "") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = label_number(accuracy = 1))


 