# # Calculate and plot the relative abundance of Bristol Bay red king crab in the 
# RKCSA, Sub-area, NMFS Area 512, the remainder of NBBTCA, and "all other" areas 
# in the BB district for the entire time series.

#This script is unfinished, not included in 2024 ESP

# Script written by Shannon Hennessey and Emily Ryznar 

library(RColorBrewer)
library(patchwork)
library(tidyverse)
library(sf)
library(akgfmaps)
library(terra)
library(shadowtext)
library(patchwork)
library(ggh4x)
library(ggridges)
library(coldpool)
library(ggpubr)
library(ggrepel)
library(janitor)


### LOAD DATA --------------------------------------------------------
# Load lookup tables
source("C:/Users/erin.fedewa/Work/Survey Data Processing/Tech Memo Revisions/Scripts/lookup_tables.R")

# # Load spatial packages and data layers
# source("./Scripts/spatial_setup.R")

# Load functions
source("./Scripts/functions.R")


#Specify current year
current_year <- 2024

#Defining recent years for tech memo
recent_years <- c(2019, 2021:2024)

# Set file path
path <- paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/Tech Memo/Data/")

# set output directories
out_dir <- paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/Tech Memo/Outputs/")
fig_dir <- paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/Tech Memo/Figures/")


# Load haul table
haul_table <- list.files(path, pattern = paste0("haul_newtimeseries.csv"), 
                         recursive = TRUE, ignore.case = TRUE) %>%
  map_df(~read.csv(paste0(path, .x)))

haul_table_nbs <- list.files(path, pattern = paste0("haul_newtimeseries_nbs.csv"), 
                             recursive = TRUE, ignore.case = TRUE) %>%
  map_df(~read.csv(paste0(path, .x)))


# Specify plotting labels
labelz <- read.csv("./Data/placenames.csv")

# # Specify plotting years for each crab species
# plot_years <- c(plot_years_lookup$START_YEAR[plot_years_lookup$SPECIES == species]:2019,
#                 2021:current_year)


# Read in Bristol Bay management shapefile
survey_gdb <- "./Data/SAP_layers.gdb"
BB_strata <- st_read(survey_gdb,layer="BristolBaySurveyStrata")

## **run CPUE_mapsEBSNBS through line 252 to get cpue_calc function (don't have to re-run data_preprocess.R)


### PROCESS DATA ----------------------------------------------------------------
year <- c(seq(1979, 2019, 1), seq(2021, current_year, 1)) #define years
recent <- max(year) #define recent year for plotting
calc_factor <- "BBRKC" # or "EBS_NBS" or "EBS", "NBS", or by stock
data_crab_EBS <- catch_rkc
data_crab_NBS <- NULL
species <- unique(data_crab_EBS$SPECIES_NAME)

cpue_out <- cpue_calc(data_crab_EBS, data_crab_NBS, calc_factor, year) ##running cpue function

# # Mature males
# cpue_data <- cpue_out$mat_male_cpue

### FUNCTION TO GENERATE IDW AND BUBBLE MAPS -----------------------------------
# cpue_map <- function(cpue_data, calc_factor, year){
cpue_interp <- data.frame()
maturity <- c("mat_male_cpue", "imm_male_cpue", "mat_fem_cpue", "imm_fem_cpue")

for(m in 1: length(maturity)){
  cpue_data <- as.data.frame(cpue_out[maturity[m]])
  names(cpue_data) <- c("AKFIN_SURVEY_YEAR", "GIS_STATION", "MAT_SEX", "AREA_SWEPT", 
                        "HAUL_TYPE", "MID_LATITUDE","MID_LONGITUDE", "CPUE")
  
  for(ii in 1:length(year)){ 
    #Define common plotting breaks across all years, if multiple
    cpue_data %>% 
      dplyr::rename(CPUE_KGHA = CPUE, 
                    LATITUDE = MID_LATITUDE,
                    LONGITUDE = MID_LONGITUDE) %>%
      mutate(COMMON_NAME = rep(species)) -> data
    
    eval_plot_breaks(CPUE = data$CPUE_KGHA, styles = c("equal", "quantile", "kmeans", "hclust", "fisher", "jenks"), 
                     n.breaks = 5) %>%
      filter(style=="kmeans") -> breaks
    
    #Set up crab-specific idw plotting features
    #Get other map layers from akgfmaps
    if(calc_factor %in% c("EBS", "BBRKC", "PribRKC", "PribBKC", "StMattBKC", "TannerE", "TannerW")) {
      
      map_layers <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs="auto")
      
    } else if(calc_factor %in% c("NBS", "NSRKC")){
      
      map_layers <- akgfmaps::get_base_layers(select.region = "bs.north", set.crs="auto")
      
    } else{
      
      map_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs="auto")
    }
    
    
    #map_layers$bathymetry <- akgfmaps::get_survey_bathymetry(select.region = "bs.south", 
    #set.crs = map_layers$crs) #can use general bathy by omitting this line
    
    # Load 2018 NBS survey area, project to interpolation crs
    survey_2018_layer <- st_transform(survey_2018_layer, map_layers$crs)
    
    # Conditionally specify 2018 NBS survey shape if year = 2018
    
    if (year[ii] == 2018 & calc_factor == "EBS_NBS"){
      
      map_layers$survey.area[2,] <- st_intersection(map_layers$survey.area[2,], survey_2018_layer$geometry)
      
    } else if (years[ii] == 2018 & calc_factor %in% c("NBS", "NSRKC")){
      
      map_layers$survey.area <- survey_2018_layer
      
    } else{
      
      map_layers$survey.area[2,] <- map_layers$survey.area[2,]
    }
    
    
    # Transform cpue data frame to plotting crs
    cpue_data2 <- cpue_data %>%
      dplyr::filter(AKFIN_SURVEY_YEAR %in% year[ii]) %>%
      dplyr::rename(CPUE_KGHA = CPUE, 
                    LATITUDE = MID_LATITUDE,
                    LONGITUDE = MID_LONGITUDE) %>%
      mutate(COMMON_NAME = rep(species)) %>%
      sf::st_as_sf(coords = c(x = "LONGITUDE", y = "LATITUDE"), 
                   crs = sf::st_crs("+proj=longlat")) %>% 
      sf::st_transform(crs = map_layers$crs)
    
    # Inverse distance weighting
    idw_fit <- gstat::gstat(formula = CPUE_KGHA ~ 1, locations = cpue_data2, nmax = 4)
    
    
    # Generate extrapolation grid
    extrap.box = c(xmn = -179.5, xmx = -157, ymn = 50, ymx = 68)
    grid.cell = c(0.02, 0.02)
    
    sp_extrap.raster <- raster::raster(xmn=extrap.box['xmn'],
                                       xmx=extrap.box['xmx'],
                                       ymn=extrap.box['ymn'],
                                       ymx=extrap.box['ymx'],
                                       ncol=(extrap.box['xmx']-extrap.box['xmn'])/grid.cell,
                                       nrow=(extrap.box['ymx']-extrap.box['ymn'])/grid.cell,
                                       crs = raster::crs("+proj=longlat")) %>% 
      raster::projectRaster(crs = raster::crs(cpue_data2))
    
    
    # Predict, rasterize, mask
    extrap.grid <- predict(idw_fit, as(sp_extrap.raster, "SpatialPoints")) %>% 
      sf::st_as_sf() %>% 
      sf::st_transform(crs = st_crs(BB_strata)) %>%
      sf::st_intersection(BB_strata) %>%
      sf::st_transform(crs = raster::crs("EPSG:4326")) %>% 
      stars::st_rasterize() 
    # sf::st_join(map_layers$survey.area, join = st_intersects)
    
    values <- as.data.frame(extrap.grid)
    # ggplot(data = values, aes(x = x, y = y, colour = var1.pred)) + geom_point()
    
    # back-transform projected coordinates to lon/lats
    pred_cpue <- values[,c(1:3)]
    names(pred_cpue) <- c("lon", "lat", "pred_cpue")
    pred_cpue$year <- year[ii]
    pred_cpue$MAT_SEX <- cpue_data$MAT_SEX[1]
    
    cpue_interp <- rbind(cpue_interp, pred_cpue)
  } # end year loop
} # end mat_sex loop

names(cpue_interp) <- c("LONGITUDE", "LATITUDE", "CPUE_INTERP", "AKFIN_SURVEY_YEAR", "MAT_SEX")

write.csv(cpue_interp, "~/cpue_interpolation.csv", row.names = FALSE)

# test <- cpue_interp[which(cpue_interp$AKFIN_SURVEY_YEAR == 2023),]

## define spatial extents
cpue_interp2 <- cpue_interp %>%
  mutate(SP_AREA = case_when(((LATITUDE <= 58 & LATITUDE >= 56) & (LONGITUDE <= -160 & LONGITUDE > -162)) ~ "NMFS Area 512",
                             ((LATITUDE <= 57 & LATITUDE > 56.2) & (LONGITUDE <= -162 & LONGITUDE >= -164)) ~ "RKCSA",
                             ((LATITUDE <= 56.2 & LATITUDE >= 56) & (LONGITUDE <= -162 & LONGITUDE >= -164)) ~ "RKCSS",
                             ((LATITUDE <= 58.72 & LATITUDE >= 58) & (LONGITUDE <= -159 & LONGITUDE >= -160)) ~ "all other Bristol Bay", # Togiak/NBBTA
                             ((LATITUDE <= 59 & LATITUDE >= 55) & (LONGITUDE <= -157 & LONGITUDE >= -162)) ~ "remainder of NBBTCA",
                             TRUE ~ "all other Bristol Bay")) %>%
  filter(!is.na(CPUE_INTERP))

# visualize area definitions
# ggplot(cpue_interp2, aes(x = LONGITUDE, y = LATITUDE, colour = SP_AREA)) + geom_point()


# Sum across grid cell, scale abundance to spatial area, then sum across spatial area
abund_df <- cpue_interp2 %>%
  group_by(AKFIN_SURVEY_YEAR, MAT_SEX, SP_AREA) %>% 
  dplyr::summarise(CPUE_INTERP = sum(CPUE_INTERP)) %>%
  distinct() %>%
  ungroup() 

# reorder spatial area levels for plotting
abund_df$SP_AREA2 <- factor(abund_df$SP_AREA, levels = rev(c("RKCSA", "RKCSS", "NMFS Area 512", "remainder of NBBTCA", "all other Bristol Bay")))

# figure
abund_df %>%
  ggplot(aes(x = AKFIN_SURVEY_YEAR, y = CPUE_INTERP, fill = SP_AREA2)) +
  geom_col(position = "fill") +
  theme_bw() +
  labs(fill = "Bristol Bay Area") +
  ylab("Proportion of total") +
  xlab("Year") +
  scale_fill_manual(name = "Bristol Bay Area", breaks = c("RKCSA", "RKCSS", "NMFS Area 512", "remainder of NBBTCA", "all other Bristol Bay"),
                    values = c("#018571","#80cdc1","#dfc27d","#c28337","#80471c")) +
  facet_wrap(~ MAT_SEX) +
  theme(legend.position = "bottom", legend.background = element_rect(fill = "white", color = "black"))

ggsave("C:/Users/shannon.hennessey/Work/2023 Tech Memo/BBRKC_area_relabund_timeseries.png",  height=8.5, width=10.5, units="in")
# ggsave("./Figures/BBRKC_area_relabund_timeseries.png",  height=8.5, width=10.5, units="in")


# summary stats - mean and range proportion of each group in each area
frequencies <- abund_df %>%
  filter(MAT_SEX %in% c('Immature Female','Immature Male', 'Mature Female', 'Mature Male')) %>%
  group_by(AKFIN_SURVEY_YEAR, MAT_SEX, SP_AREA) %>%
  summarise(n = CPUE_INTERP) %>%
  mutate(freq = n / sum(n))

mean_freq <- frequencies %>%
  group_by(MAT_SEX, SP_AREA) %>%
  # summarise(n = freq) %>%
  mutate(mean_prop = mean(freq, na.rm = TRUE),
         min_prop = min(freq, na.rm = TRUE), 
         max_prop = max(freq, na.rm = TRUE)) %>%
  dplyr::select(MAT_SEX, mean_prop, min_prop, max_prop) %>%
  distinct()
  