library(readxl)
library(tidyverse)

# This script associates the wells in USGS_MN_PH_AS.xlsx with their site information (depth, coordinates, etc)
# Once this script is run, the outputted dataset is ready to be integrated into the Powell_Database

# Load in datasets

for (smudge in c("Smudged", "Precise")){
USGS_data <- read_excel("../Data/USGS_MN_PH_AS/USGS_MN_PH_AS.xlsx", 
                            sheet = "Modified",skip = 1, col_types = "text")
Site_info <- read_excel(paste0("../Data/USGS_MN_PH_AS/Site_Information_",smudge,".xlsx"), 
                        sheet = "Modified",skip = 1, col_types = "text")

# Combine datasets
USGS_MN_PH_AS <- left_join(USGS_data,Site_info) %>% 
  dplyr::mutate(Study_ID = "USGS_MN_PH_AS",
                 Sample_ID = paste(Study_ID, row_number(), sep = "-"),
                 Water_Source = "GW",
                 Country = "USA",
                 Smudged_Coordinates = "Smudged")%>% 
  dplyr::select(Country, Water_Source, Study_ID,Sample_ID, everything())

# Output as csv
write_csv(USGS_MN_PH_AS, paste0("../Data/USGS_MN_PH_AS_",smudge,".csv"), na = "")


rm(USGS_data, Site_info)

}
