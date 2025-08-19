## Purpose: 
# calculate and plot the relative abundance of mature male BBRKC in the 
# RKCSA, Sub-area, NMFS Area 512, the remainder of NBBTCA, and "all other" areas 
# in the BB district for the entire time series.

# Author: Shannon Hennessey, Emily Ryznar, Erin Fedewa

#Note for 2026: May want to expand out to all BBRKC, not just mature males? If so, 
  #will need to rerun timeseries interpolations for all crab, not by size/sex

library(gstat)
library(akgfmaps)

### LOAD DATA -------------------------------------------------------------------
## Read in setup
source("./Scripts/get_crab_data.R")

#load timeseries cpue interpolation file - we'll just use 1982+
  #this was created using the same workflow as we'll use for the current year 
  #interpolation, but we'll just merge old + new so we don't have to run full 
  #timeseries interpolations every year
cpue_interp_timeseries <- read.csv("./Data/BBRKC_cpue_interp_1979_2024.csv") %>%
                            filter(MAT_SEX == "Mature Male",
                                   AKFIN_SURVEY_YEAR > 1981) %>%
                            select(-MAT_SEX) %>%
                            rename(YEAR = AKFIN_SURVEY_YEAR)
#NOTE: In 2026, replace this csv with "BBRKC_cpue_interp_1982_2025.csv"

#load SAP geodatabase and survey layers 
survey_gdb <- "./Figs/SAP_layers" 
survey_strata <- terra::vect(survey_gdb, layer = "EBS.NBS_surveyarea")
BB_strata <- akgfmaps::get_crab_strata(select.stock = "bbrkc", set.crs = "EPSG:3338")
map_layers <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs="auto") 
map_layers$bathymetry <- akgfmaps::get_survey_bathymetry(select.region = "bs.south",
                                      set.crs = map_layers$crs) #can use general bathy by omitting this line

## SET COORDINATE REFERENCE SYSTEMS (CRS) --------------------------------------
crs.latlon <- "epsg:4326" #lat lon crs
in.crs <- "+proj=longlat +datum=NAD83" # sometimes will need to specify an input CRS for some spatial data. This CRS is in lat/lon
map.crs <- "EPSG:3338" # final crs for mapping/plotting etc. This CRS is good for Alaska ("Alaska Albers")

# Load individual closure areas 
closure_areas <- "./Figs/Closure areas" 
RKCSA <- st_read(closure_areas, layer = "RKCSA")
RKCSA_sub <- st_read(closure_areas, layer = "RKCSA_sub")
ns_trawl <- st_read(closure_areas, layer = "ns_trawl")
fivetwelve <- st_read(closure_areas, layer = "area512")

### PROCESS CURRENT YEAR DATA ----------------------------------------------------------------
current_year <- 2025

#calculate current year CPUE of mature male BBRKC - 
cpue_data <- calc_cpue(crab_data = dat,
                        species = "RKC",
                        region = "EBS",
                        district = "BB",
                        years = current_year,
                        sex = "male",
                        crab_category = "mature_male") %>%
              select(-SPECIES, -REGION, -DISTRICT,STRATUM,-TOTAL_AREA,-COUNT,
                     -STRATUM, -CPUE_MT, -CPUE_LBS)


#Transform cpue data frame to plotting crs
cpue_data2 <- cpue_data %>%
                    sf::st_as_sf(coords = c(x = "LONGITUDE", y = "LATITUDE"), 
                    crs = sf::st_crs("+proj=longlat")) %>% 
                    sf::st_transform(crs = map_layers$crs)
   
# Inverse distance weighting
idw_fit <- gstat::gstat(formula = CPUE ~ 1, locations = cpue_data2, nmax = 4)

#Generate extrapolation grid
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

#Predict, rasterize, mask to BB boundary
extrap.grid <- predict(idw_fit, as(sp_extrap.raster, "SpatialPoints")) %>% 
                sf::st_as_sf() %>% 
                sf::st_transform(crs = st_crs(BB_strata)) %>%
                sf::st_intersection(BB_strata) %>%
                sf::st_transform(crs = raster::crs("EPSG:4326")) %>% 
                stars::st_rasterize() 
                sf::st_join(map_layers$survey.area, join = st_intersects)

extrap.grid <- predict(idw_fit, as(sp_extrap.raster, "SpatialPoints")) %>% 
                sf::st_as_sf() %>% 
                sf::st_transform(crs = st_crs(BB_strata)) %>%
                sf::st_intersection(BB_strata) %>%
               stars::st_rasterize() %>%
               sf::st_join(BB_strata, join = st_intersects) %>%
              sf::st_transform(crs = raster::crs("EPSG:4326")) 

values <- as.data.frame(extrap.grid)

#quick plot                
ggplot(data = values, aes(x = x, y = y, colour = var1.pred)) + geom_point()
    
#back-transform projected coordinates to lon/lats
pred_cpue <- values[,c(1:3)]
names(pred_cpue) <- c("lon", "lat", "pred_cpue")
pred_cpue$year <- current_year

cpue_interp_current <- data.frame()
     
cpue_interp_current <- rbind(cpue_interp_current, pred_cpue)

names(cpue_interp_current) <- c("LONGITUDE", "LATITUDE", "CPUE_INTERP", "YEAR")
write.csv(cpue_interp_current, "./Outputs/BBRKC_cpue_interp_2025.csv", row.names = FALSE)

### JOIN DATASETS AND CALCUATE PROPORTION IN CLOSURE AREA----------------------------------------------------
cpue_interp_timeseries %>%
  bind_rows(cpue_interp_current) -> interp_full

#write csv and start with this next year!
write.csv(interp_full, "./Data/BBRKC_cpue_interp_1982_2025.csv")

#Combine closure areas
areas <- bind_rows(list(RKCSA %>% mutate(AREA = "RKCSA"), 
                        RKCSA_sub %>% mutate(AREA = "RKCSA_sub"), 
                        ns_trawl %>% mutate(AREA = "ns_trawl"), 
                        fivetwelve %>% mutate(AREA = "fivetwelve")))

# Assign management area boundaries
cpue_interp2 <- interp_full %>%
  st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = raster::crs("EPSG:4326")) %>% # transform into spatial object
  st_transform(., map.crs) %>% # project to same crs as mgmt areas
  na.omit(.) %>% 
  st_join(., areas) %>% # identify data that falls within mgmt area polygons, anything that doesn't is NA
  st_transform(., crs.latlon) %>% # project back to lat/lon crs
  cbind(., st_coordinates(.)) %>% # transform from spatial object to data frame
  as.data.frame(.) %>%
  dplyr::select(!c(FID, geometry)) %>% # assign mgmt areas and closure status below
  dplyr::mutate(SP_AREA = case_when((AREA == "fivetwelve") ~ "NMFS Area 512",
                                    (AREA == "RKCSA") ~ "RKCSA",
                                    (AREA == "RKCSA_sub") ~ "RKCSSA",
                                    (is.na(AREA) == TRUE & X >= -162) ~ "remainder of NBBTCA",
                                    (AREA == "ns_trawl") ~ "NBBTA",
                                    (TRUE ~ "all other Bristol Bay")),
                STATUS = case_when((SP_AREA %in% c("NMFS Area 512", "RKCSA", "remainder of NBBTCA")) ~ "Closed",
                                   (SP_AREA == "RKCSSA" & YEAR %in% c(1995, 1996, 2002, 2023)) ~ "Closed",
                                   (TRUE ~ "Open"))) %>%
  dplyr::select(!AREA)

# visualize area definitions for a single year
ggplot(cpue_interp2 %>% filter(YEAR == 1995), aes(x = X, y = Y, colour = SP_AREA)) + geom_point()

#Calculate proportion of population in closure areas by year
  #Sum across grid cell, scale abundance to spatial area, then sum across spatial area
prop_closed <- cpue_interp2 %>%
  group_by(YEAR) %>% 
  dplyr::summarise(OPEN = sum(CPUE_INTERP[STATUS == "Open"]),
                   CLOSED = sum(CPUE_INTERP[STATUS == "Closed"])) %>%
  dplyr::mutate(PROP_CLOSED = (CLOSED/(OPEN+CLOSED))*100) %>%
  ungroup() %>%
  dplyr::select(-c(OPEN, CLOSED))

#plot
prop_closed %>%
ggplot(aes(YEAR, PROP_CLOSED)) +
  geom_point() +
  geom_line()

#save indicator output
missing <- data.frame(YEAR = 2020)

prop_closed %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
write.csv("./Output/BBRKC_proportion_closure.csv", row.names = FALSE)














# EMILY'S CODE BELOW ---
# Source spatial data, including crs' and crab-specific mapping layers
source("Y:/KOD_Survey/EBS Shelf/Spatial crab/load.spatialdata.R") 

# Load individual management areas and then bind them into a collection
st_read(closure_areas, layer = "RKCSA")  -> RKCSA
st_read(closure_areas, layer = "RKCSA_subarea")  -> RKCSA_sub
st_read(closure_areas, layer = "ns_trawl") -> ns_trawl
st_read(closure_areas, layer = "area512")  -> fivetwelve

areas <- bind_rows(list(RKCSA %>% mutate(AREA = "RKCSA"), 
                        RKCSA_sub %>% mutate(AREA = "RKCSA_sub"), 
                        ns_trawl %>% mutate(AREA = "ns_trawl"), 
                        fivetwelve %>% mutate(AREA = "fivetwelve")))

# Loading test data and adding extra dummy data to make sure area subsetting works across years and matsex
cpue_interp <- rbind(read.csv("./Outputs/cpue_interpolation_1979.csv"),
                     read.csv("./Outputs/cpue_interpolation_1979.csv") %>%
                       mutate(year = 1980, MAT_SEX = "Mature Female", 
                              pred_cpue= pred_cpue * 60)) %>%
  rename(AKFIN_SURVEY_YEAR = year, CPUE_INTERP = pred_cpue)

# Assign management area boundaries
cpue_interp2 <- cpue_interp %>%
  st_as_sf(., coords = c("lon", "lat"), crs = raster::crs("EPSG:4326")) %>% # transform into spatial object
  st_transform(., map.crs) %>% # project to same crs as mngmt areas
  na.omit(.) %>% 
  st_join(., areas) %>% # identify data that falls within mngmt area polygons, anything that doesn't is NA
  st_transform(., crs.latlon) %>% # project back to lat/lon crs
  cbind(., st_coordinates(.)) %>% # transform from spatial object to data frame
  as.data.frame(.) %>%
  dplyr::select(!c(FID, geometry)) %>% # assign mgnmt areas and closure status below
  dplyr::mutate(SP_AREA = case_when((AREA == "fivetwelve") ~ "NMFS Area 512",
                                    (AREA == "RKCSA") ~ "RKCSA",
                                    (AREA == "RKCSA_sub") ~ "RKCSSA",
                                    (is.na(AREA) == TRUE & X >= -162) ~ "remainder of NBBTCA",
                                    (AREA == "ns_trawl") ~ "NBBTA",
                                    (TRUE ~ "all other Bristol Bay")),
                STATUS = case_when((SP_AREA %in% c("NMFS Area 512", "RKCSA", "remainder of NBBTCA")) ~ "Closed",
                                   (SP_AREA == "RKCSSA" & AKFIN_SURVEY_YEAR %in% c(1995, 1996, 2002, 2023)) ~ "Closed",
                                   (TRUE ~ "Open"))) %>%
  dplyr::select(!AREA)






















## EMILY'S CODE END. DOWNSTREAM FROM HERE WILL NEED TO CHANGE BASED ON WHAT YOU WANT TO PLOT 



## For closure area specific data request (all below) ----------------------------
# define spatial extents 
cpue_interp2 <- cpue_interp %>%
  mutate(SP_AREA = case_when(((LATITUDE <= 58 & LATITUDE >= 56) & (LONGITUDE <= -160 & LONGITUDE > -162)) ~ "NMFS Area 512",
                             ((LATITUDE <= 57 & LATITUDE > 56.2) & (LONGITUDE <= -162 & LONGITUDE >= -164)) ~ "RKCSA",
                             ((LATITUDE <= 56.2 & LATITUDE >= 56) & (LONGITUDE <= -162 & LONGITUDE >= -164)) ~ "RKCSS",
                             ((LATITUDE <= 58.72 & LATITUDE >= 58) & (LONGITUDE <= -159 & LONGITUDE >= -160)) ~ "all other Bristol Bay", # Togiak/NBBTA
                             ((LATITUDE <= 59 & LATITUDE >= 55) & (LONGITUDE <= -157 & LONGITUDE >= -162)) ~ "remainder of NBBTCA",
                             TRUE ~ "all other Bristol Bay")) %>%
  filter(!is.na(CPUE_INTERP))

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
  geom_col(position = "fill", show.legend = TRUE) +
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

###############################
Start here to run most current year idw (output combined to start with next year)


current_year <- 2025
cpue_interp <- data.frame()

   
breaks <- plot_breaks(CPUE = data$CPUE_KGHA, styles = c("equal", "quantile", "kmeans", "hclust", "fisher", "jenks"), n.breaks = 5) %>%
              filter(style=="kmeans")
     
##Set up crab-specific idw plotting features
map_layers <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs="auto")
map_layers$bathymetry <- akgfmaps::get_survey_bathymetry(select.region = "bs.south", 
set.crs = map_layers$crs) #can use general bathy by omitting this line

#Start here, feed in bbrkc cpue 




#     # Transform cpue data frame to plotting crs
#     cpue_data2 <- cpue_data %>%
#                     dplyr::filter(YEAR == 2025) %>%
#                     dplyr::rename(CPUE_KGHA = CPUE, 
#                                   LATITUDE = MID_LATITUDE,
#                                   LONGITUDE = MID_LONGITUDE) %>%
#                     mutate(COMMON_NAME = rep(bbrkc)) %>%
#                     sf::st_as_sf(coords = c(x = "LONGITUDE", y = "LATITUDE"), 
#                                  crs = sf::st_crs("+proj=longlat")) %>% 
#                     sf::st_transform(crs = map_layers$crs)
#     
Inverse distance weighting
idw_fit <- gstat::gstat(formula = CPUE_KGHA ~ 1, locations = cpue_data2, nmax = 4)
#     
#     
#     # Generate extrapolation grid
#     extrap.box = c(xmn = -179.5, xmx = -157, ymn = 50, ymx = 68)
#     grid.cell = c(0.02, 0.02)
#     
#     sp_extrap.raster <- raster::raster(xmn=extrap.box['xmn'],
#                                        xmx=extrap.box['xmx'],
#                                        ymn=extrap.box['ymn'],
#                                        ymx=extrap.box['ymx'],
#                                        ncol=(extrap.box['xmx']-extrap.box['xmn'])/grid.cell,
#                                        nrow=(extrap.box['ymx']-extrap.box['ymn'])/grid.cell,
#                                        crs = raster::crs("+proj=longlat")) %>% 
#       raster::projectRaster(crs = raster::crs(cpue_data2))
#     
# 
#     # Predict, rasterize, mask to BB boundary
#     extrap.grid <- predict(idw_fit, as(sp_extrap.raster, "SpatialPoints")) %>% 
#                     sf::st_as_sf() %>% 
#                     sf::st_transform(crs = st_crs(BB_strata)) %>%
#                     sf::st_intersection(BB_strata) %>%
#                     sf::st_transform(crs = raster::crs("EPSG:4326")) %>% 
#                     stars::st_rasterize() 
#     # sf::st_join(map_layers$survey.area, join = st_intersects)
#     # extrap.grid <- predict(idw_fit, as(sp_extrap.raster, "SpatialPoints")) %>% 
#     #                sf::st_as_sf() %>% 
#     #                sf::st_transform(crs = st_crs(BB_strata)) %>%
#     #                # sf::st_intersection(BB_strata) %>%
#     #                stars::st_rasterize() %>%
#     #                sf::st_join(BB_strata, join = st_intersects) %>%
#     #                sf::st_transform(crs = raster::crs("EPSG:4326")) 
# 
#     values <- as.data.frame(extrap.grid)
#     # ggplot(data = values, aes(x = x, y = y, colour = var1.pred)) + geom_point()
#     
#     # back-transform projected coordinates to lon/lats
#     pred_cpue <- values[,c(1:3)]
#     names(pred_cpue) <- c("lon", "lat", "pred_cpue")
#     pred_cpue$year <- year[ii]
#     pred_cpue$MAT_SEX <- cpue_data$MAT_SEX[1]
#     
#     cpue_interp <- rbind(cpue_interp, pred_cpue)
#   } # end year loop
# } # end mat_sex loop
# 
# names(cpue_interp) <- c("LONGITUDE", "LATITUDE", "CPUE_INTERP", "AKFIN_SURVEY_YEAR", "MAT_SEX")
# # write.csv(cpue_interp, "./Outputs/cpue_interpolation.csv", row.names = FALSE)