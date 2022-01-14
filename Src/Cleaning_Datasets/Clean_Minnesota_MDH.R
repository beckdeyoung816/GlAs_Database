library(readxl)
library(tidyverse)

# This script accounts for the multiple observations for a given well included in the Minnesota_MDH data folder
# Once this script is run, the outputted dataset is ready to be integrated into the Powell_Database

for (smudge in c("Smudged", "Precise")){


Minnesota_MDH <- read_excel(paste0("../Data/Minnesota_MDH/MDH_arsenic_locations_",smudge,".xlsx"), 
                            sheet = "Modified", skip = 1) %>% 
  dplyr::rename(As__ppb = latest_result) %>% # We are only selecting the latest result
  
  dplyr::mutate(Smudged_Coordinates = if_else(Well_type %in% c("MU", "PC", "PN", "PP", "PS"), "Smudged", NA_character_), # Note which wells have had locations smudged before publishing
                As__ppb = if_else(As__ppb < 0, NA_real_, As__ppb), # Replace -9999 with missing
                Study_ID = "Minnesota_MDH",
                Sample_ID = paste(Study_ID, row_number(), sep = "-"),
                Water_Source = "GW",
                Country = "USA") %>% 
  
  dplyr::select(-c(results_count, 
                   detection_rate, 
                   ends_with("result"),
                   sd)) %>% 
  
  dplyr::select(Country, Water_Source, Study_ID,Sample_ID, everything())

# Output as a csv
write_csv(Minnesota_MDH, paste0("../Data/Minnesota_MDH_",smudge,".csv"), na = "")

}

