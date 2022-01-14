
library(tidyverse)
library(readxl)

# This script combines the individual excel files in the USGS_Minnesota_2018 folder
# Once this script is run, the outputted dataset is ready to be integrated into the Powell_Database

path <- "../Data/USGS_Minnesota_2018/"

# Load in individual datasets
AsData <- read_excel(paste0(path,"AsData.xlsx"), skip = 1, col_types = "text")
ConstructionData <- read_excel(paste0(path,"WellConstructionData.xlsx"), skip = 1, col_types = "text")
AncillaryData <- read_excel(paste0(path,"AncillaryData.xlsx"), skip = 1, col_types = "text")

# Join datasets
USGS_Minnesota_2018 <- full_join(AsData, ConstructionData)
USGS_Minnesota_2018 <- full_join(USGS_Minnesota_2018, AncillaryData) %>% 
  dplyr::mutate(Study_ID = "USGS_Minnesota_2018",
               Sample_ID = paste(Study_ID, row_number(), sep = "-"),
               Water_Source = "GW",
               Country = "USA")%>% 
  dplyr::select(Country, Water_Source, Study_ID,Sample_ID, everything())

# Output as a csv
write_csv(USGS_Minnesota_2018, "../Data/USGS_Minnesota_2018.csv", na = "")

rm(path, AsData, ConstructionData, AncillaryData)
