#Central script to load EBS BT survey data via crabpack R package and define
  #Bristol Bay stations for subsetting indicators

#Script Author: Erin Fedewa 

## Load packages
library(crabpack)
library(tidyverse)
library(tidync)
library(lubridate)
library(sf)
library(httr)
library(akgfmaps)
library(rnaturalearth)

##########################################################

## Pull haul and BBRKC specimen data ----
dat <- get_specimen_data(species = "RKC",
                            district = "BB",
                            region = "EBS",
                            channel = "KOD")

bbrkc <- dat$specimen
haul <- dat$haul

#Look at spatial coverage of survey sampling in BB
haul %>%
  filter(HAUL_TYPE != 17,
         DISTRICT == "BB") %>%
  group_by(YEAR) %>%
  distinct(STATION_ID) %>%
  count() %>%
  ggplot(aes(YEAR, n)) +
  geom_bar(stat="identity")
#let's use 1982+ for survey derived indices
  
# Set years ----
current_year <- 2025
years <- 1982:current_year

