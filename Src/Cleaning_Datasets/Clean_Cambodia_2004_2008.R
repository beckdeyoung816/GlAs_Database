library(tidyverse)
library(readxl)



# Replace all negatives with bdl
bdl_replace <- function(x) {
  # return (as.data.frame(lapply(x,function(x){x[x<=0] = min(x[x>0]);x})))
  return (x %>% mutate_if(is.numeric, funs(if_else(.<0, "bdl", as.character(.)))))
}

# 2004 
combine_2004 <- function(){
  
  path <- "../Data/Cambodia_2004_2008/2004 Kandal/"
  
  concs <- read_excel(path = paste0(path,"EAWAG_Groundwaters_Kandal_30-11-05 BB.xls"), sheet = "Concs Modified", skip = 3, col_types = "text")
  sites <- read_excel(path = paste0(path,"EAWAG_Groundwaters_Kandal_30-11-05 BB.xls"), sheet = "Sampling Sites Modified", skip = 3, col_types = "text")
  
  Kandal_GW_2004 <- full_join(concs, sites) %>% mutate(Study_ID = "Kandal_2004",
                                                       Country = "Cambodia",
                                                       Water_Source = "GW")
  write_csv(Kandal_GW_2004, paste0(path,"/Kandal_2004.csv", na = ""))
  write_csv(Kandal_GW_2004, "../Data/Cambodia_2004_2008/Cleaned_Files/Kandal_2004.csv", na = "")
}

# 2005

combine_2005 <- function(){
  path <- "../Data/Cambodia_2004_2008/2005 Hadzima/"
  Hadzima_2005 <- read_excel(path = paste0(path,"water_solids_concentrations whole dataset_from jmp.xls"), sheet = "Modified", skip = 1, col_types = "text")
  Hadzima_2005 <- Hadzima_2005[1:39] # Non Solids data
  Hadzima_2005[14:34] <- bdl_replace(type_convert(Hadzima_2005[14:34]))
  
  Hadzima_2005 <- Hadzima_2005 %>% mutate(Study_ID = "Hadzima_2005",
                                          Water_Source = "GW",
                                          Country = "Cambodia",
                                          Sample_year = "2005")
  
  write_csv(Hadzima_2005, paste0(path,"/Hadzima_2005.csv", na = ""))
  write_csv(Hadzima_2005, "../Data/Cambodia_2004_2008/Cleaned_Files/Hadzima_2005.csv", na = "")
}

# 2006

combine_icp_group1_2006 <- function(){
  
  path <- "../Data/Cambodia_2004_2008/2006 August Papacostas/"
  input<- "Input/ICP Group 1.xlsx"
  
  doc <- read_excel(sheet = "DOC Modified",
                    path = paste0(path,input), 
                    skip = 1, col_types = "text")
  
  dic <- read_excel(sheet = "DIC Modified",
                    path = paste0(path,input),  
                    skip = 1, col_types = "text")
  
  icp <- read_excel(sheet = "ICP Modified",
                    path = paste0(path,input), 
                    skip = 1, col_types = "text")
  
  icp <- bdl_replace(type_convert(icp))
  
  icp_group1 <- full_join(doc,dic, by = "Well_ID")
  icp_group1 <- full_join(icp_group1,icp, by = "Well_ID") %>% mutate(Study_ID3 = "Group 1")
  
  write_csv(icp_group1, paste0(path,"/Output/ICP_Group1.csv"), na = "")
  }


combine_icp_group2_2006 <- function(){
  
  path <- "../Data/Cambodia_2004_2008/2006 August Papacostas/"
  input<- "Input/ICP Group 2.xlsx"
  
  icp <- read_excel(sheet = "ICP group2 Modified",
                    path = paste0(path,input), 
                    skip = 1, col_types = "text")
  
  locations <- read_excel(sheet = "locations group2 Modified",
                          path = paste0(path,input), 
                    skip = 1, col_types = "text")
  
  icp[6:22] <- bdl_replace(type_convert(icp[6:22]))
  
  icp_group2 <- left_join(icp,locations, by = "Well_ID") %>% mutate(Study_ID3 = "Group 2")
  
  write_csv(icp_group2, paste0(path,"/Output/ICP_Group2.csv"), na = "")
  
  }


combine_2006 <- function(){
  combine_icp_group1_2006()
  combine_icp_group2_2006()
  
  path <- "../Data/Cambodia_2004_2008/2006 August Papacostas/"
  
  quicksall <- read_excel(sheet = "Modified",
                          path = paste0(path,"Input/Water Composition Quicksall.xlsx"), col_types = "text") %>% 
    mutate(Study_ID3 = "Water Composition Quicksall")
  
  group1 <- read_csv(paste0(path,"Output/ICP_Group1.csv"),col_types = cols(.default = "c"))
  group2 <- read_csv(paste0(path,"Output/ICP_Group2.csv"),col_types = cols(.default = "c"))
  
  
  August_Papacostas_2006 <- full_join(group1, group2)
  August_Papacostas_2006 <- full_join(August_Papacostas_2006, quicksall)
  
  August_Papacostas_2006 <- August_Papacostas_2006 %>% mutate(Study_ID = "August_Papacostas_2006",
                                                              Country = "Cambodia",
                                                              Water_Source = "GW",
                                                              Sample_year = if_else(is.na(Sample_year),"2006", Sample_year))
  
  write_csv(August_Papacostas_2006, paste0(path,"Output/August_Papacostas_2006.csv"), na = "")
  write_csv(August_Papacostas_2006, "../Data/Cambodia_2004_2008/Cleaned_Files/August_Papacostas_2006.csv", na = "")
  }

# 2007

combine_2007 <- function(){
  
 path <- "../Data/Cambodia_2004_2008/2007 Fall Quicksall/"
  
  
 icp <- read_excel(path = paste0(path, "ICP data FINAL DATA TO INCORPORATE.xlsx"), sheet = "Modified",skip = 3, col_types = "text")
 icp <- bdl_replace(type_convert(icp))
 
 sites <- read_excel(path = paste0(path, "WaterSamples_fall07.xls"), sheet = "Modified",skip = 1, col_types = "text")
 
 doc <- read_excel(path = paste0(path, "C isotope analysis.xlsx"), sheet = "Modified",skip = 1, col_types = "text")

 quicksall_2007 <- full_join(icp,sites)
 quicksall_2007 <- left_join(quicksall_2007, doc) %>% mutate(Study_ID = "Quicksall_2007",
                                                             Country = "Cambodia",
                                                             Water_Source = "GW")
 
 write_csv(quicksall_2007, paste0(path,"Quicksall_2007.csv"), na = "")
 write_csv(quicksall_2007, "../Data/Cambodia_2004_2008/Cleaned_Files/Quicksall_2007.csv", na = "")
}

# 2008

combine_2008_fall <- function(){
  path <- "../Data/Cambodia_2004_2008/2008 Fall Sampling, Hanh/"
  field_sw <- read_excel(path = paste0(path,"Field Data - computer lab 12-25.xls"),
                         sheet = "Surface Modified", skip = 1, col_types = "text") %>% 
    mutate(Sample_date = as.character(Sample_date))
  
  field_gw <- read_excel(path = paste0(path,"Field Data - computer lab 12-25.xls"),
                         sheet = "Ground Modified", skip = 1, col_types = "text")
  field <- full_join(field_sw,field_gw)
  
  lab <- read_excel(path = paste0(path,"ICP FINAL.xls"),
                    sheet = "Master Modified", skip = 1, col_types = "text")
  
  Hanh_Fall_2008 <- full_join(field,lab, by = "Well_ID") %>% mutate(Study_ID = "Hanh_Fall_2008",
                                                                    Country = "Cambodia",
                                                                    Sample_year = if_else(is.na(Sample_year),"2008", Sample_year))
  write_csv(Hanh_Fall_2008, paste0(path,"Hanh_Fall_2008.csv"), na = "")
  write_csv(Hanh_Fall_2008, "../Data/Cambodia_2004_2008/Cleaned_Files/Hanh_Fall_2008.csv", na = "")
}

combine_2008_spring <- function(){
  
  path <- "../Data/Cambodia_2004_2008/2008 Spring sampling data Eli/"
  input <- "tracemetals_april_complete_with means_May13.xlsx"
  sheets <- c("groundwater Modified", "open Modified", "surface Modified")
  
  for (sheet in sheets){
    tmp <- read_excel(sheet = sheet, path = paste0(path,input), skip = 1, col_types = "text")
    #tmp[6:ncol(tmp)] <- as.data.frame(lapply(tmp[6:ncol(tmp)], as.numeric))
    
    if (sheet == "groundwater Modified") {Eli_Spring_2008 <- tmp}
    else { Eli_Spring_2008 <- full_join(Eli_Spring_2008,tmp)}
  }
  
  Eli_Spring_2008 <- Eli_Spring_2008 %>% mutate(Study_ID = "Eli_Spring_2008",
                          Sample_year = "2008",
                          Country = "Cambodia")
  
  Eli_Spring_2008[5:20] <- bdl_replace(type_convert(Eli_Spring_2008[5:20]))
  write_csv(Eli_Spring_2008, paste0(path,"Eli_Spring_2008.csv"), na = "")
  write_csv(Eli_Spring_2008, "../Data/Cambodia_2004_2008/Cleaned_Files/Eli_Spring_2008.csv", na = "")
}




combine_all <- function()
{
  combine_2004()
  combine_2005()
  combine_2006()
  combine_2007()
  combine_2008_fall()
  combine_2008_spring()
  
  path <- "../Data/Cambodia_2004_2008/Cleaned_Files/"
  
  files <- c("Kandal_2004",
             "Hadzima_2005",
             "August_Papacostas_2006",
             "Quicksall_2007",
             "Hanh_Fall_2008",
             "Eli_Spring_2008"
             )
  
  for (file in files) {
  
    tmp <- read_csv(paste0(path,file,".csv"),col_types = cols(.default = "c"))
    #tmp <- data.frame(lapply(tmp, as.character), stringsAsFactors=FALSE)
    
    if(file == "Kandal_2004") {Cambodia_2004_2008 <- tmp}
    else{Cambodia_2004_2008 <- full_join(Cambodia_2004_2008, tmp)}
  }
  
  Cambodia_2004_2008 <- Cambodia_2004_2008 %>% 
    group_by(Study_ID) %>% 
    dplyr::mutate(Sample_ID = paste0(Study_ID, "-", seq(1:n())),
           Study_ID2 = "Cambodia_2004_2008") %>% 
    dplyr::select(Study_ID2, Study_ID, Sample_ID, Water_Source, Country, everything())
  
  write_csv(Cambodia_2004_2008, "../Data/Cambodia_2004_2008.csv", na = "")
}

combine_all()
