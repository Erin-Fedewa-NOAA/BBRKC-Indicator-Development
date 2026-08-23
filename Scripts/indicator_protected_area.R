# calculate and plot the relative abundance of BBRKC in the 
  # RKCSA, Sub-area, NMFS Area 512, the remainder of NBBTCA, and "all other" areas 
  # in the BB district for the entire time series.

# Author: Shannon Hennessey, Emily Ryznar, Erin Fedewa

library(gstat)
library(akgfmaps)

#LOAD
## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

#load SAP geodatabase and survey layers 
survey_gdb <- "./Figs/SAP_layers" 
survey_strata <- terra::vect(survey_gdb, layer = "EBS.NBS_surveyarea")
BB_strata <- akgfmaps::get_crab_strata(select.stock = "bbrkc", set.crs = "EPSG:3338")
map_layers <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs="auto") 
map_layers$bathymetry <- akgfmaps::get_survey_bathymetry(select.region = "bs.south",
                                      set.crs = map_layers$crs) #can use general bathy by omitting this line

#set coordinate reference systems
crs.latlon <- "epsg:4326" #lat lon crs
in.crs <- "+proj=longlat +datum=NAD83" # sometimes will need to specify an input CRS for some spatial data. This CRS is in lat/lon
map.crs <- "EPSG:3338" # final crs for mapping/plotting etc. This CRS is good for Alaska ("Alaska Albers")

# Load individual closure areas 
closure_areas <- "./Figs/Closure areas" 
RKCSA <- st_read(closure_areas, layer = "RKCSA")
RKCSA_sub <- st_read(closure_areas, layer = "RKCSA_sub")
ns_trawl <- st_read(closure_areas, layer = "ns_trawl")
fivetwelve <- st_read(closure_areas, layer = "area512")

##############################################

#get cpue time series
cpue_data <- calc_cpue(crab_data = dat,
                       species = "RKC",
                       region = "EBS",
                       district = "BB",
                       years = 1982:current_year) %>%
  select(-SPECIES, -REGION, -DISTRICT,STRATUM,-TOTAL_AREA,-COUNT,
         -STRATUM, -CPUE_MT, -CPUE_LBS)


#create 5x5km grid
grid_res <- 5000

#make grid for interpolation
interp_grid <- st_make_grid(BB_strata,
  cellsize = grid_res, what = "centers") %>%
  st_as_sf() %>%
  st_filter(BB_strata) %>%
  mutate(grid_id = row_number())

#plot
ggplot() +
  geom_sf(data = BB_strata, fill = NA) +
  geom_sf(data = interp_grid, size = 0.1)

#cpue interpolation function
interpolate_cpue <- function(dat, interp_grid,
                             nmax = 8, idp = 2) 
  {
    # Convert annual CPUE data to sf
  cpue_sf <- dat %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
            crs = crs.latlon, remove = FALSE) %>%
    st_transform(map.crs)
  
  # Fit IDW
  idw_fit <- gstat(
    formula = CPUE ~ 1,
    locations = cpue_sf,
    nmax = nmax,
    set = list(idp = idp))
  
    # Predict onto common grid
  cpue_pred <- predict(
    idw_fit,
    newdata = interp_grid) %>%
    st_as_sf()
  
    cpue_pred
}


#run for each year
years <- 1982:2026

cpue_interp_list <- map(years, function(yr) {
  dat_year <- cpue_data %>% filter(YEAR == yr)
  
  if (nrow(dat_year) == 0) return(NULL)
  
  #estimate cpue at every point in grid
  pred <- interpolate_cpue(
    dat = dat_year,
    interp_grid = interp_grid,
    nmax = 8,
    idp = 2)
  
  pred$YEAR <- yr
  pred
})

cpue_interp <- bind_rows(cpue_interp_list) %>%
  rename(CPUE_INTERP = var1.pred)

#plot a test year
cpue_test <- cpue_interp %>%
  filter(YEAR == 2026) %>%
  mutate(x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2])

ggplot() +
  geom_tile(data = cpue_test,
    aes(x = x, y = y, fill = CPUE_INTERP),
    width = grid_res, height = grid_res) +
  geom_sf(data = BB_strata, fill = NA,
    color = "black", linewidth = 0.4) +
  scale_fill_viridis_c(name = "Interpolated CPUE",
                      trans = "sqrt") +
  coord_sf(crs = map.crs, expand = FALSE) +
  theme_minimal()

# Define closure years for RKCSA_sub (calendar year after BBRKC fishey closure)
closure_years_sub <- c(1984, 1995, 1996, 2022, 2023)

# Assign closure status to each interpolated CPUE prediction
cpue_closure <- map_dfr(years,
  function(yr) {
    
    # Interpolated CPUE for this year
    dat_year <- cpue_interp %>%
      filter(YEAR == yr)
    
    if (nrow(dat_year) == 0) {
      return(NULL)
    }
    
    # RKCSA is always closed
    yr_closures <- list(RKCSA_closure)
    
    # RKCSA_sub is closed only in specified years
    if (yr %in% closure_years_sub) {
      yr_closures <- append(
        yr_closures,
        list(RKCSA_sub_closure))
    }
    
    # NBBTCA minus nearshore trawl is closed
    yr_closures <- append(
      yr_closures,
      list(NBBTCA_minus_trawl))
    
    # Combine all closure areas into one geometry
    closure_poly <- do.call(rbind, yr_closures) %>%
      st_union()
    
    # Determine whether each interpolated CPUE point is inside the closure
    closure_status <- st_intersects(
      st_geometry(dat_year),
      closure_poly)
    
    # Add closure status
    dat_year %>%
      mutate(closure = lengths(closure_status) > 0)
  }
)

#check
cpue_closure %>%
  st_drop_geometry() %>%
  count(YEAR, closure)

#calculate relative abundance of BBRKC in closures
relative_abundance <- cpue_closure %>%
  st_drop_geometry() %>%
  group_by(YEAR, closure) %>%
  #Because every prediction point represents the same 5 × 5 km cell, we can sum predicted CPUE values
  summarise(abundance_index = sum(CPUE_INTERP, na.rm = TRUE),
    .groups = "drop") %>%
  group_by(YEAR) %>%
  mutate(total_abundance = sum(abundance_index),
    proportion = abundance_index / total_abundance) %>%
  ungroup()

#plot
relative_abundance %>% 
  filter(closure == TRUE) %>%
ggplot(aes(x = YEAR, y = proportion)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = mean(proportion, na.rm = TRUE))) 
 
####################################################
#save output

indicator_closure <- relative_abundance %>%
  filter(closure == TRUE) %>%
  mutate(proportion_closure = proportion*100) %>%
  select(YEAR, proportion_closure) %>%
  rename_with(tolower) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)

write.csv(indicator_closure, file="./Output/indicator_closure.csv", row.names = F)

####################################################
#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_BBRKC_Protected_Area_Survey"

##EDITABLE TEXT
description <- paste0("The proportion (%) of total BBRKC interpolated abundance during the summer EBS bottom trawl 
                      survey located in year-round closure areas. Closure areas include the Red King Crab 
                      Savings Area, the Red King Crab Savings Subarea in subsequent calendar years following
                      a BBRKC directed fishery closure, and the Nearshore Bristol Bay Trawl Closure Area 
                      (with the exception of the Togiak/Nearshore Bristol Bay Trawl Area). Proposed sign of 
                      the relationship is positive.")

status_trends <- paste0("In 2026, approximately 82% of the BBRKC population was located in closed areas 
                        during the summer survey period, which increased slightly from 2025 and is above 
                        the time series mean.")

factors <- paste0("Proportion of the population found in closure areas is influenced by interannual 
                  variability in stock distribution and spatial extent (likely temperature-mediated), 
                  although high catch stations can disproportionately impact the estimate in years when 
                  survey catches are patchy.")

implications <- paste0("An increase in the proportion of BBRKC in closure areas suggests the potential for reduced 
                       bycatch of crab in groundfish fisheries, and a potential reduction in interactions with fishing 
                       gear during the summer period. However, seasonal migrations and shifts in groundfish fishing 
                       intensity throughout the year highlight the limited utility of this indicator for drawing inference 
                       on bycatch and unobserved mortality.")

references <- paste0("")


##INDICATOR DATA
indicator_data <- indicator_closure %>%
  rename(indicator = proportion_closure)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)













