library(tidyverse)
library(readxl)
library(stringr)

# This script combines and cleans the individual excel sheets in the USGS_Data.xlsx file
# Once this script is run, the outputted dataset is ready to be integrated into the Powell_Database

# Dataset with each sheet name we want the USGS Data
usgs_dataset_list <- read_excel("../Data_Support_Files/USGS_Data_List.xlsx")

path = "../Data/USGS_2014/USGS_2014_Data.xlsx" ; i_loop <- 1

for (i_dataset in (1:length(usgs_dataset_list$Sheet))){
  temp_data <- read_excel(sheet = usgs_dataset_list$Sheet[i_dataset],
                          path = path, 
                          skip = usgs_dataset_list$Skip[i_dataset],
                          col_types = "text")
  
  if (i_loop == 1) USGS_2014 <- temp_data
  else USGS_2014 <- left_join(USGS_2014,temp_data, by = "Well_ID")  
  
  i_loop <- i_loop + 1
}

# Remove unnecessary columns

USGS_2014 <- USGS_2014 %>% 
  dplyr::select(-c(Lithology_Code,
                   Aquifer_amt,
                   Water_use_code,
                   nawqa_sucode1,	
                   nawqa_suc1_type,	
                   nawqa_sucode2,	
                   nawqa_suc2_type,	
                   nawqa_sucode3,	
                   nawqa_suc3_type,	
                   number_sucodes, 
                   Alk_method,
                   Bicarbonate_method,
                   NH3_Organic_N__mgL,
                   LocDiv_State)) %>% # Accounted for In Postal Code which is the form we want
  dplyr::rename(LocDiv_State = LocDiv_Postal_Code) %>% 
  dplyr::mutate(Sample_ID= paste(Study_ID, row_number(), sep = "-")) %>% 
  dplyr::select(Country, Water_Source, Study_ID,Sample_ID, everything())

# Output this finished USGS dataset to be added into the full dataset

write_csv(USGS_2014, '../Data/USGS_2014.csv', na = "")

rm(usgs_dataset_list, temp_data, i_dataset, i_loop, path)
