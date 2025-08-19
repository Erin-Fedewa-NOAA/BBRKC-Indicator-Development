## Purpose: Pull CPUE tables from the RACE Oracle Schemas for calculating mean 
##          CPUE of groundfish and benthic invertebrate guilds 
##
## NOTES:
## - FOSS tables aren't 0-filled, cuts down *SIGNIFICANTLY* on pulling time
##   but will need to 0-fill species of interest 

library(gapindex)
library(RODBC)
library(odbc)
library(getPass)

#Connect to AFSC Oracle Database
source("C:/Users/erin.fedewa/Work/R/ConnectToOracle.R")
channel <- channel

#Or can also use with manual entry: 
channel = gapindex::get_connected()

#########################################################
## Manipulate with SQL using FOSS data before bringing into R 
# This will pull a final, completely formatted table for only EBS and NBS
# and does not include 0-filled data by species -- run time <5 minutes

# start_time <- Sys.time()
dat <- RODBC::sqlQuery(channel = channel, 
                       query = paste0("SELECT
                                       hh.CRUISEJOIN,
                                       hh.CRUISE,
                                       hh.YEAR,
                                       --hh.SURVEY_DEFINITION_ID,
                                       hh.SURVEY,
                                       hh.VESSEL_ID,
                                       hh.VESSEL_NAME,
                                       hh.HAULJOIN,
                                       hh.HAUL,
                                       hh.STATION,
                                       hh.LATITUDE_DD_START,
                                       hh.LATITUDE_DD_END,
                                       hh.LONGITUDE_DD_START,
                                       hh.LONGITUDE_DD_END,
                                       hh.AREA_SWEPT_KM2,
                                       cc.SPECIES_CODE,
                                       tt.SCIENTIFIC_NAME,
                                       tt.COMMON_NAME,
                                       cc.WEIGHT_KG,
                                       cc.COUNT,
                                       cc.CPUE_KGKM2,
                                       cc.CPUE_NOKM2
                                       FROM GAP_PRODUCTS.FOSS_HAUL hh
                                       LEFT JOIN GAP_PRODUCTS.FOSS_CATCH cc
                                       ON hh.HAULJOIN = cc.HAULJOIN
                                       LEFT JOIN GAP_PRODUCTS.FOSS_SPECIES tt
                                       ON cc.SPECIES_CODE = tt.SPECIES_CODE
                                       WHERE SURVEY_DEFINITION_ID IN (143, 98);")) # 143 NBS, 98 EBS
# end_time <- Sys.time()
# cat(paste("Time Elapsed:", round(end_time - start_time, 2), 
#           units(end_time - start_time), "\n\n"))

#* if want to convert CPUE to weight/numeric CPUE, run the following:
#* mutate(WTCPUE = CPUE_KGKM2/100,
#*        NUMCPUE = CPUE_NOKM2/100)
#* **NEED TO CHECK what units we want -- crab CPUE is in per NMI2 not KM2...


write.csv(dat, "./Data/gf_cpue_timeseries.csv", row.names = FALSE)