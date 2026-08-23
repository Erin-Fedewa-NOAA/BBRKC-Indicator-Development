#Create master csv of ecosystem indicators for indicator analysis
#Create indicator timeseries plots

#TO DO: add in trend analysis to communicate on report card plots as symbols
#analysis to detect trend (change in timeseries mean) or regime-like behavior

# Erin Fedewa

# load ----
library(tidyverse)
library(corrplot)
library(patchwork)
library(mgcv)
library(ggdist)
library(scales)

#########################################
#Read in all 14 BBRKC ecosystem indicators
  #including even those that haven't been updated with current year data

# Find all indicator files in both output folders
indicator_files <- c(list.files("./Output",
    pattern = "^indicator_.*\\.csv$",
    full.names = TRUE),
  list.files("./Output/Contributor indicators",
    pattern = "^indicator_.*\\.csv$",
    full.names = TRUE))

# Read and join all indicator files by year
indicators <- indicator_files %>%
  map(read_csv) %>%
  reduce(full_join, by = "year") %>%
  arrange(year)

#save ouput for indicator analysis
write_csv(indicators, "./Output/BBRKC_esp_indicator_timeseries.csv")

################################################

#Ecosystem Plots ----

# Set years for plotting
current_year <- 2026


# Plot information for each indicator
indicator_info <- tribble(
  ~indicator,               ~title,                                    ~y_label,                       ~filename,
  "mean_ao",                "Arctic Oscillation",                      "Deviation",                    "arctic_oscillation.png",
  "benthic_invert",         "Invertebrate Density",                    "Density (kg/km²)",             "invert_density.png",
  "pcod_density",           "Pacific Cod Density",                     "Density (kg/km²)",             "pcod_density.png",
  "date_corrected_temp",    "Bottom Temperature",                      "Temperature (°C)",             "bottom_temp.png",
  "inshore_run",            "Sockeye Salmon Inshore Run Size",         "Abundance (millions of fish)", "sockeye.png",
  "mean_distance_km",       "Catch Distance From Shore",               "Distance (km)",                "distance.png",
  "proportion_closure",     "Proportion of BBRKC in Closure Areas",    "% of BBRKC population",        "proportion_closed.png",
  "prop_empty",             "Female BBRKC Reproductive Failure",       "% Barren",                     "clutch.png",
  "mature_female_d95",      "BBRKC Mature Female Area Occupied",       "Area (nm²)",                   "mature_female_d95.png",
  "mature_male_d95",        "BBRKC Mature Male Area Occupied",         "Area (nm²)",                   "mature_male_d95.png",
  "ratio",                  "Northern District:Bristol Bay RKC Ratio", "Ratio",                        "northern_bbrkc_ratio.png",
  "chla",                   "Chlorophyll a Concentration",             "Concentration (µg/L)",         "chla.png",
  "ph",                     "Spring pH",                               "pH",                           "ph.png",
  "wind_stress",            "Summer Wind Stress",                      "meters/second",                "wind_stress.png"
  )


#plotting function:
plot_indicator <- function(data, indicator, title, y_label, filename) {
  
  plot_data <- data %>%
    select(year, value = all_of(indicator)) %>%
    filter(!is.na(value))
  
  first_year <- min(plot_data$year)
  
  p <- ggplot(plot_data, aes(x = year, y = value)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = mean(plot_data$value, na.rm = TRUE),
              linetype = 5) +
    geom_hline(yintercept = mean(plot_data$value, na.rm = TRUE) -
              sd(plot_data$value, na.rm = TRUE), linetype = 3) +
    geom_hline(yintercept = mean(plot_data$value, na.rm = TRUE) +
              sd(plot_data$value, na.rm = TRUE), linetype = 3) +
    annotate("rect", xmin = current_year - 0.5, xmax = current_year + 0.5,
              ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "green") +
    labs(y = y_label, x = "") +
    scale_x_continuous(limits = c(first_year - 0.5, current_year + 0.5),
      breaks = seq(ceiling(first_year / 5) * 5, current_year, by = 5)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(
        lineheight = .8,
        face = "bold",
        hjust = 0.5)) +
    ggtitle(title)
  
  ggsave(filename = file.path("./Figs", filename),
    plot = p, width = 8, height = 5, dpi = 300)
}

#generate all 14 ecosystem indicator plots and save
pwalk(indicator_info,
  ~ plot_indicator(data = indicators, indicator = ..1,
                  title = ..2, y_label = ..3, filename = ..4))

####################################################################
#Socioeconomic Indicators

#Skipper survey plots
skipper <- read_csv("./Data/Contributor Indicators/Skipper Survey Q1_Q3.csv")

#question 1: perceived abundance
color_palette <- c("red" = "red", "blue" = "blue", "grey" = "grey")

q1 <- skipper %>% 
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

#save plot to correct size for report card
ggsave("./Figs/Skipper_survey_1.png", plot = q1,
  width = 3.968750, height = 1.333333,
  units = "in", dpi = 300, limitsize = FALSE)

#question 2: changes in fishing behavior
q2 <- skipper %>% 
  filter(stock == "bbrkc",
         question == "fishing_practice",
    #Using only the top 4 responses here! 
         response != c("move_location", "more_test_pots", "less_test_pots")) %>%
  ggplot(aes(number_responses, response)) +
  geom_bar(stat = "identity", alpha = .8, fill = "grey") +
  scale_y_discrete(labels = c("no_change" = "No Change", "move_location" = "Moved Fishing Locations", 
                              "longer_soak" = "Longer Soak Times", "shorter_soak" = "Shorter Soak Times",
                              "less_test_pots" = "Less Test Pots", "increase_communication" = "More Communication with Fleet")) +
  labs(x = "Number of Responses", y = "") +
  theme_bw() +
  theme(legend.position = "none")

#save plot to correct size for report card
ggsave("./Figs/Skipper_survey_2.png", plot = q2,
       width = 3.968750, height = 1.333333,
       units = "in", dpi = 300, limitsize = FALSE)

#question 3: motivation for changes in fishing behavior
q3 <- skipper %>% 
  filter(stock == "bbrkc",
         question == "reason_change") %>%
  ggplot(aes(number_responses, response)) +
  geom_bar(stat = "identity", alpha = .8, fill = "grey") +
  scale_y_discrete(labels = c("no_change" = "No Change", "low_cpue" = "Low CPUE", 
                              "high_discard" = "Too Much Sorting",
                              "high_cpue" = "High CPUE")) +
  labs(x = "Number of Responses", y = "") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = label_number(accuracy = 1))

#save plot to correct size for report card
ggsave("./Figs/Skipper_survey_3.png", plot = q3,
       width = 3.968750, height = 1.333333,
       units = "in", dpi = 300, limitsize = FALSE)


 