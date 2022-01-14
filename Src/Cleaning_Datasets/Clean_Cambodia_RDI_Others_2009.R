library(tidyverse)
library(readxl)

# This script fixes missing values stored as zeros and locations for Cambodia_RDI_Others_2009.xlsx
# Once this script is run, the outputted dataset is ready to be integrated into the Powell_Database


# The Wrongly Stored NA file has unsnapped coordinates but has missing values stored as zero when some values have actual zeroes
# The new file has snapped coordinates but missing values stored correctly as NA
# For possible samples, we will replace the snapped coordinates with unsnapped coordinates and maintain missing values stored as NA

old_NA <- read_excel(path = "../Data_Support_Files/Cambodia_RDI_Others_2009 - Wrongly Stored NAs.xlsx",sheet = "Modified",skip = 1, col_types = "text")[-1,] %>% 
  dplyr::select(Sample_ID, Well_ID, Long, Lat, As_kit__ugL, Fe__mgL, Depth__m, Sample_day, Sample_month, Sample_year)
new_NA <- read_excel("../Data/Cambodia_RDI_Others_2009.xlsx",sheet = "Modified", skip = 1, col_types = "text") %>% mutate(Water_Source = "GW")

# Find the Well_IDs that are not consistent 
# These are all because of a decimal storage issue
old_wrong_WI <- old_NA[!old_NA$Well_ID %in% new_NA$Well_ID, ] %>% dplyr::select(Well_ID_old = Well_ID)
new_wrong_WI <- new_NA[!new_NA$Well_ID %in% old_NA$Well_ID, ] %>% dplyr::select(Well_ID_new = Well_ID)
wrong_well_IDs <- bind_cols(old_wrong_WI, new_wrong_WI)

# Replace the oddly stored decimal IDs
for (well in old_NA$Well_ID) {
  if (well %in% wrong_well_IDs$Well_ID_old)
    old_NA$Well_ID[old_NA$Well_ID == well] <- wrong_well_IDs$Well_ID_new[wrong_well_IDs$Well_ID_old == well]
}


old_NA <- old_NA %>% dplyr::select(Well_ID, Long, Lat, Sample_day, Sample_month, Sample_year)

# Join the datasets by the well id
both_locations <- full_join(dplyr::select(new_NA,-Lithology), old_NA,  
                            by = c("Well_ID", "Sample_day", "Sample_month", "Sample_year")) %>% 
  dplyr::select(Well_ID, Lat.x, Lat.y,Long.x, Long.y, As_kit__ugL, everything())


combined_lat_long <- both_locations %>% 
  distinct(Well_ID, Sample_day, As_kit__ugL, Fe__mgL, .keep_all = T) %>% 
  mutate(Lat = case_when(!is.na(Lat.y) ~ Lat.y, # If we have true coordinates, choose them
                         Lat.x > 0 ~ Lat.x,     # If the snapped coordinates are realistic, choose them 
                         T ~ NA_character_),    # Do not keep bogus snapped coordinates
         
         Long = case_when(!is.na(Long.y) ~ Long.y,
                          Long.x > 100.511 ~ Long.x,
                          T ~ NA_character_))


Cambodia_RDI_Others_2009 <- combined_lat_long %>% 
  dplyr::mutate(Sample_ID = paste(Study_ID, row_number(), sep = "-")) %>%
  dplyr::select(-starts_with("Lat."),-starts_with("Long.")) %>% 
  dplyr::select(Country, Water_Source, Study_ID,Sample_ID, everything())

# Output as a csv
write_csv(Cambodia_RDI_Others_2009, "../Data/Cambodia_RDI_Others_2009.csv", na = "")

rm(old_NA, both_locations, combined_lat_long, old_wrong_WI, wrong_well_IDs, new_wrong_WI, new_NA)



