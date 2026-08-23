#Function to create text file output to upload indicators 
  #onto AKFIN submission portal 

#load
library(dplyr)
library(glue)

#specify shared settings
current_year <- 2026

output_folder <- "./Output/Indicator Text Files"

################################

#function to create file 
create_indicator_file <- function(
    indicator_name,
    indicator_data,
    description,
    status_trends,
    factors,
    implications,
    references) {
  
  # Output file name
  output_file <- file.path(output_folder,
    paste0(indicator_name, "_", current_year, ".txt"))
  
  # Convert years to tab-separated string
  year_values <- indicator_data %>%
    pull(year) %>%
    paste(collapse = "\t")
  
  # Convert indicator values to tab-separated string
  indicator_values <- indicator_data %>%
    pull(indicator) %>%
    replace(is.na(.), "NA") %>%
    paste(collapse = "\t")
  
  # Build text file
  output_text <- c(
    "#Ecosystem and Socioeconomic Profile (ESP) Contribution template",
    "#For details on uploading a contribution, field descriptions, sample templates, or other information on the ESP contribution 
  uploading process, please refer to the ESP Uploader User Guide version 2 (https://drive.google.com/file/d/1H14MKtMSc0uNKvxbtLSx4JsplPHiXsyV/view?usp=sharing) 
  or contact Kalei Shotwell at kalei.shotwell@noaa.gov for questions.",
    "#INDICATOR_REVIEW ----------------------------------------------------------------------------------------",
    "",
    "#SUBMISSION_YEAR",
    current_year,
    "",
    "#INDICATOR_NAME",
    glue("\"{indicator_name}\""),
    "",
    "#DESCRIPTION",
    glue("\"{description}\""),
    "",
    "#STATUS_TRENDS",
    glue("\"{status_trends}\""),
    "",
    "#FACTORS",
    glue("\"{factors}\""),
    "",
    "#IMPLICATIONS",
    glue("\"{implications}\""),
    "",
    "#REFERENCES",
    glue("\"{references}\""),
    "#INDICATOR_DATA ----------------------------------------------------------------------------------------",
    "#YEAR",
    year_values,
    "",
    "#INDICATOR_VALUE",
    indicator_values)
  
  # Write the file
  writeLines(
    text = output_text,
    con = output_file)
  
  # Confirmation message
  message("File written to: ",
    normalizePath(output_file))
}