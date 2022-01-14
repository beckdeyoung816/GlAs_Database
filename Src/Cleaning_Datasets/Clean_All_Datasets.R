# Reclean all datasets
Cleaning_files = list.files("Cleaning_Datasets", pattern = "*.R")[-1] # Currently, Clean_All_Datasets.R is in position 1
sapply(paste0("Cleaning_Datasets/",Cleaning_files), source)
