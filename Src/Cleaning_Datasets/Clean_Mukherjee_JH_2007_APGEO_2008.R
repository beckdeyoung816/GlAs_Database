
library(tidyverse)
library(readxl)


# This script combines the individual excel files in the Mukherjee_JH_2007_APGEO_2008 folder
# Once this script is run, the outputted dataset is ready to be integrated into the Powell_Database

mukherjee_2008 <- read_excel(sheet = "Modified", path = "../Data/Mukherjee_JH_2007_APGEO_2008/Mukherjee_2008.xlsx", skip = 2, col_types = "text")
mukherjee_2007 <- read_excel(sheet = "Modified", path = "../Data/Mukherjee_JH_2007_APGEO_2008/Mukherjee_JH_2007.xlsx", skip = 1, col_types = "text")


# Join on same wells
Mukherjee_JH_2007_APGEO_2008 <- full_join(mukherjee_2007,mukherjee_2008, by= c("Lat", 
                                                                               "Long", 
                                                                               "Depth__m",
                                                                               "Water_Source"))
Mukherjee_JH_2007_APGEO_2008 <- Mukherjee_JH_2007_APGEO_2008 %>% 
  
  # Create unique sample ids and add study id
  dplyr::mutate(Country = "India",
               Study_ID = "Mukherjee_JH_2007_APGEO_2008",
               Sample_ID= paste(Study_ID, row_number(), sep = "-")) %>% 
  dplyr::select(Country, Water_Source, Study_ID,Sample_ID, everything())

write_csv(Mukherjee_JH_2007_APGEO_2008, "../Data/Mukherjee_JH_2007_APGEO_2008.csv", na = "")

rm(mukherjee_2007, mukherjee_2008)
