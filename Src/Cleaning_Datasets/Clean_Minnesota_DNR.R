library(readxl)
library(tidyverse)

# This script combines the individual excel files in the Minnesota_DNR folder
# Once this script is run, the outputted dataset is ready to be integrated into the Powell_Database

# Load in datasets

for (smudge in c("Smudged", "Precise")){

nonPWS <- read_excel("../Data/Minnesota_DNR/DNR_CGA_Chem_for_Powell_As_nonPWS.xlsx", sheet = "Modified", skip = 1, col_types = "text")
PWS <- read_excel(paste0("../Data/Minnesota_DNR/DNR_CGA_Chem_for_Powell_As_PWS_",smudge,".xlsx"), sheet = "Modified", skip = 1, col_types = "text")[-1]

# Combine datasets
Minnesota_DNR <- bind_rows(nonPWS, PWS) %>% 
  dplyr::mutate(Study_ID = "Minnesota_DNR",
                Sample_ID = paste(Study_ID, row_number(), sep = "-"),
                Water_Source = "GW",
                Country = "USA",
                Smudged_Coordinates = if_else(Well_type == "Well-PubWtrSup", "Smudged", NA_character_))%>% 
  dplyr::select(Country, Water_Source, Study_ID,Sample_ID, everything())

# Output as csv
write_csv(Minnesota_DNR, paste0("../Data/Minnesota_DNR_",smudge,".csv"), na = "")

rm(PWS, nonPWS)

}
