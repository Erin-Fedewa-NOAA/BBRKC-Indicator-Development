#Goal: This script pulls zero-filled CPUE tables from the RACE Oracle Schema
  #for calculating mean CPUE of groundfish/benthic invert guilds 

#NOTE: use this script to create "gf_cpue_timeseries.csv"
  #For now, Em M. ran this script to generate csv until I have ORACLE access 

# This has a specific username and password because I DONT want people to have access to this!
  #Store this source script within ESP folder 
source("Z:/Projects/ConnectToOracle.R")
channel <- channel_products

# I set up a ConnectToOracle.R that looks like this: 
#   
#   PKG <- c("RODBC")
# for (p in PKG) {
#   if(!require(p,character.only = TRUE)) {  
#     install.packages(p)
#     require(p,character.only = TRUE)}
# }
# 
# channel<-odbcConnect(dsn = "AFSC",
#                      uid = "USERNAME", # change
#                      pwd = "PASSWORD", #change
#                      believeNRows = FALSE)
# 
# odbcGetInfo(channel)


# Manipulate with SQL using AKFIN data before bringing into R ------------------

# This will pull a final, completely formatted table for only EBS and NBS

a <- RODBC::sqlQuery(channel = channel, # NOT RACEBASE.HAUL
                     query = paste0(
                       "SELECT
cr.CRUISEJOIN,
cr.CRUISE,
cr.YEAR,
cr.SURVEY_DEFINITION_ID,
cr.SURVEY_NAME,
cr.VESSEL_ID,
cr.VESSEL_NAME,
cp.HAULJOIN,
cp.SPECIES_CODE,
tt.SPECIES_NAME,
tt.COMMON_NAME,
cp.WEIGHT_KG,
cp.COUNT,
cp.AREA_SWEPT_KM2,
cp.CPUE_KGKM2,
cp.CPUE_NOKM2,
-- cp.CPUE_KGKM2/100 AS WTCPUE,
-- cp.CPUE_NOKM2/100 AS NUMCPUE,
hh.HAUL,
hh.STATION
FROM GAP_PRODUCTS.AKFIN_HAUL hh
LEFT JOIN GAP_PRODUCTS.AKFIN_CRUISE cr
ON hh.CRUISEJOIN = cr.CRUISEJOIN
LEFT JOIN GAP_PRODUCTS.AKFIN_CPUE cp
ON hh.HAULJOIN = cp.HAULJOIN
LEFT JOIN GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION tt
ON cp.SPECIES_CODE = tt.SPECIES_CODE
WHERE SURVEY_DEFINITION_ID IN (143, 98) -- 143 NBS, 98 EBS
AND tt.SURVEY_SPECIES = 1;")) 

write.csv(x = a, 
          here::here("data","indicator_cpue_timeseries.csv"))

# Alternatively, you an just download the files and manipulate locally ---------

# this will pull all standard RACE survey data (e.g., also GOA, AI, BSS)

### Load data files from Oracle ------------------------------------------------

locations <- c(
  "GAP_PRODUCTS.AKFIN_HAUL", 
  "GAP_PRODUCTS.AKFIN_CRUISE", 
  "GAP_PRODUCTS.AKFIN_CPUE", 
  "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION")

for (i in 1:length(locations)) {
  print(locations[i])
  a <- RODBC::sqlQuery(channel = channel0, # NOT RACEBASE.HAUL
                       query = paste0("SELECT *
FROM ",locations[i],"; ")) 
  write.csv(x = a, 
            here::here("data",
                       paste0(#tolower
                         (gsub(pattern = '.', 
                               replacement = "_", 
                               x = locations[i], 
                               fixed = TRUE)),
                         ".csv")))
}

### Load data from local data folder -------------------------------------------

print("Load oracle data")

a <- list.files(path = here::here("data"), 
                full.names = TRUE, recursive = FALSE, pattern = ".csv")

for (i in 1:length(a)){
  b <- readr::read_csv(file = a[i], show_col_types = FALSE)
  # b <- janitor::clean_names(b)
  # if (names(b)[1] %in% "x1"){
  #   b$x1<-NULL
  # }
  temp <- strsplit(x = a[i], split = "/")
  temp <- gsub(pattern = "\\.csv", replacement = "", x = temp[[1]][length(temp[[1]])])
  assign(x = paste0(temp, "0"), value = b)
}

### Wrangle code in R ----------------------------------------------------------

dat <- dplyr::left_join(GAP_PRODUCTS_AKFIN_HAUL, 
                        GAP_PRODUCTS_AKFIN_CRUISE) %>% 
  dplyr::left_join(GAP_PRODUCTS_AKFIN_CPUE) %>% 
  dplyr::left_join(GAP_PRODUCTS_TAXONOMIC_CLASSIFICATION) %>% 
  dplyr::filter(SURVEY_DEFINITION_ID %in% c(143, 98)) %>% 
  dplyr::select(CRUISEJOIN, CRUISE, YEAR, SURVEY_DEFINITION_ID, 
                SURVEY_NAME, VESSEL_ID, VESSEL_NAME, HAULJOIN, 
                SPECIES_CODE, SPECIES_NAME, COMMON_NAME, WEIGHT_KG, 
                COUNT, AREA_SWEPT_KM2, CPUE_KGKM2, CPUE_NOKM2,
                HAUL, STATION) %>% 
  dplyr::mutate(WTCPUE = CPUE_KGKM2/100,
                NUMCPUE = CPUE_NOKM2/100)

write.csv(x = dat, 
          here::here("data","indicator_cpue_timeseries.csv"))

# Manipulate with SQL using FOSS data before bringing into R -------------------

# This will pull a final, completely formatted table for only EBS and NBS

a <- RODBC::sqlQuery(channel = channel, # NOT RACEBASE.HAUL
                     query = paste0(
                       "SELECT
hh.CRUISEJOIN,
hh.CRUISE,
hh.YEAR,
hh.SURVEY_DEFINITION_ID,
hh.SURVEY,
hh.VESSEL_ID,
hh.VESSEL_NAME,
hh.HAULJOIN,
cc.SPECIES_CODE,
cc.SCIENTIFIC_NAME,
cc.COMMON_NAME,
cc.WEIGHT_KG,
cc.COUNT,
hh.AREA_SWEPT_KM2,
cc.CPUE_KGKM2,
cc.CPUE_NOKM2,
hh.HAUL,
hh.STATION
FROM GAP_PRODUCTS.FOSS_HAUL hh
LEFT JOIN GAP_PRODUCTS.FOSS_CATCH cc
ON hh.HAULJOIN = cc.HAULJOIN
WHERE SURVEY_DEFINITION_ID IN (143, 98);")) ## 143 NBS, 98 EBS 

write.csv(x = a, 
          here::here("data","indicator_cpue_timeseries.csv"))
