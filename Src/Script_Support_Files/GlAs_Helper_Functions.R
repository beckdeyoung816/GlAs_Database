
load_and_clean_data <- function(Dataset, Skip, Sheet, IP, SMUDGE){
  # Load and clean the data
  # @Dataset: Filename of dataset
  # @Skip: Integer of how many rows to skip
  # @Sheet: Sheet name
  # @SMUDGE: 'Precise' or 'Smudged'
  if (str_ends(Dataset, '.csv')){
    
    Dataset <- str_replace(Dataset, "LOCATIONS", SMUDGE) # Specify whether we want smudged or precise
    
    df <- make_path(IP, Dataset, T) %>% read_csv(col_types = cols(.default = "c"))
    
  } else {
    
    df <- read_excel(path = make_path(IP, Dataset, T), 
                     sheet = Sheet,
                     skip = Skip,
                     col_types = "text")
  }
  
  # Transform each dataframe from wide to long
  # Final df will have the six columns below
  df %>% gather(key = 'Parameter',
                value = 'Value',
                -Country,-Study_ID,-Sample_ID,-Water_Source)
}


### Harmonize Parameters

create_param_group <- function(x, dict){
  # Create a parameter group for harmonization
  # This makes a df of original parameters and the parameter it should be renamed as
  # @x: Integer of which parameter group to create from the dictionary
  # @d: dictionary of parameter groups
  
  harm_dict[x] %>% data.frame() %>% gather(key = 'Harmonized_Param', value = 'Parameter')
}


convert <- function(units, df){
  # Convert desired units into millimolar
  # @units: units to convert
  # @df: original dataframe to convert
  
  # Filter for only the observations with the desired units where the is a molar mass
  # The extra molar mass conditions removes TDS which is measured in mgL but cannot be converted to mM
  # The Sample_ID, Parameter, and Unit are stored to keep track of the observation while separated from the original df 
  
  df_filt <- conv_units %>% filter(Unit %in% units) %>% filter(!is.na(Molar_Mass)) %>% 
    dplyr::select(Sample_ID, Parameter, Unit, Value, Molar_Mass)
  
  # As there is a lot of data, it is best to convert to a numeric matrix for calculations to reduce runtime drastically
  mat <- df_filt[,4:5] %>% as.matrix()
  
  # Determine the factor of multiplication based on the unit
  Factor <- case_when(identical(units, c('gL', 'ppt')) | units == 'M' ~ 1e3,
                      identical(units, c('mgL', 'ppm')) ~ 1,
                      identical(units, c('ugL', 'ppb')) | units == 'uM' ~ 1e-3,
                      units == 'nM' ~ 1e-6)[1]
  
  # If the units are already in Molar (M, nM, uM), we do not need to divide by the molar mass, hence we make MM = 1
  # If the units are not already in Molar, then we divide by the molar mass to put it into Molar
  MM <- if(str_detect(units, 'M')[1]) 1 else{ mat[,2]}
  
  conv_value <- mat[,1] / MM  * Factor # Compute converted values
  
  # Attach new values to filtered df and add new units
  return(cbind(df_filt, conv_value) %>% mutate(conv_unit = 'mM')) 
}

### Dominant redox process
# Determine dominant redox process using the approach of McMahon and Chapelle (2007, DOI: 10.1111/j.1745-6584.2007.00385.x)

get_redox_process <- function(data_in){
  
  redox_thresh_DO <- 0.5/sum(mass(c("O","O"))) # DO threshold mM
  redox_thresh_NO3_N <- 0.5/sum(mass(c("N"))) # NO3-N threshold mM
  redox_thresh_Mn <- 0.05/sum(mass(c("Mn"))) # Mn threshold mM
  redox_thresh_Fe <- 0.1/sum(mass(c("Fe"))) # Fe threshold mM
  redox_thresh_SO4 <- 0.5/sum(mass(c("S","O","O","O","O"))) # SO4 threshold mM
  
  data_in <- data_in %>% 
    mutate(Redox_process = case_when(
      DO__mM >= redox_thresh_DO & Mn__mM < redox_thresh_Mn & Fe__mM < redox_thresh_Fe ~ "O2 reduction",
      DO__mM < redox_thresh_DO & NO3__mM < redox_thresh_NO3_N & Mn__mM < redox_thresh_Mn & Fe__mM < redox_thresh_Fe ~ "Suboxic",
      DO__mM < redox_thresh_DO & NO3__mM >= redox_thresh_NO3_N & Mn__mM < redox_thresh_Mn & Fe__mM < redox_thresh_Fe ~ "NO3 reduction",
      DO__mM < redox_thresh_DO & NO3__mM < redox_thresh_NO3_N & Mn__mM >= redox_thresh_Mn & Fe__mM < redox_thresh_Fe ~ "Mn reduction",
      DO__mM < redox_thresh_DO & NO3__mM < redox_thresh_NO3_N & Fe__mM >= redox_thresh_Fe & SO4__mM >= redox_thresh_SO4 ~ "Fe/SO4 reduction",
      DO__mM < redox_thresh_DO & NO3__mM < redox_thresh_NO3_N & Fe__mM >= redox_thresh_Fe & SO4__mM < redox_thresh_SO4 ~ "Methanogenesis"
    ))
  
  data_in <- data_in %>% 
    mutate(Redox_process = if_else(is.na(Redox_process) & 
                                     !is.na(DO__mM) & 
                                     !is.na(NO3__mM) & 
                                     !is.na(Mn__mM) &
                                     !is.na(Fe__mM) &
                                     !is.na(SO4__mM),
                                   "Mixed", Redox_process))
}


### pe and Eh calculation

get_pe_Eh <- function(data_in){
  
  data_in <- data_in %>% 
    mutate(pE = if_else(is.na(pE) & !is.na(Eh__mV), (Eh__mV/1000)/0.059, pE)) # See EQ 9.23 (Postma and Appelo, 2nd edition)
  
  data_in <- data_in %>% 
    mutate(Eh__mV = if_else(is.na(Eh__mV) & !is.na(pE), 0.059*1000*pE, Eh__mV))
}



### Calculated iron speciation 

get_Fe_speciation <- function(data_in){
  K_Fe3_Fe2 <- 10^-13.05 # See Table 9.5 (Postma and Appelo, 2nd edition
  
  data_in <- data_in %>% 
    mutate(Fe_3_calc__mM = if_else(!is.na(Fe__mM) & !is.na(pE), K_Fe3_Fe2*Fe__mM/(K_Fe3_Fe2 + (10^-pE)) , NA_real_)) %>% 
    mutate(Fe_2_calc__mM = Fe__mM - Fe_3_calc__mM)
}


### Function to convert As in ppb (ug/L) to mM

As_ppb_to_mM <- function(in_value){
  Atomic_mass_As <- 74.92519
  
  out_value <- in_value*(1/1000)*(1/Atomic_mass_As)
}



### Prepare PHREEQC files and Run PHREEQC
#### Define function to create input files for PHREEQC

# solution_data: dataframe with a single sample's chemistry data that is used to generate phreeqC input
# solution_number: integer that serves as an identifier for a given solution (defaults to 1 if user does not supply a value)
# si_output_selections: a string containing the names of the minerals to have their modeled saturation indices output
# molalities_output_selections: a string containing the names of the chemical species to have their modeled molalities output

get_phreeqC_input <- function(solution_data, 
                              solution_number = 1, 
                              si_output_selections = c(""),
                              molalities_output_selections = c(""),
                              totals_output_selections = c("")){
  
  i_temperature <- if_else(!is.na(solution_data$Temp__C), solution_data$Temp__C, 20)
  i_pH <- solution_data$pH
  i_pe <- if_else(!is.na(solution_data$pE), solution_data$pE, 4.0000001) 
  i_HCO3 <- solution_data$HCO3__mM
  i_Fe <- solution_data$Fe__mM
  i_Mn <- solution_data$Mn__mM
  i_Ca <- solution_data$Ca__mM
  i_K <- solution_data$K__mM
  i_Si <- solution_data$Si__mM
  i_Na <- solution_data$Na__mM
  i_Mg <- solution_data$Mg__mM
  i_SO4 <- solution_data$SO4__mM
  
  i_number <- solution_number
  i_sample_ID <- solution_data$Sample_ID
  
  solution_current <- solution(.number = i_number,
                               .name = i_sample_ID,
                               temperature = i_temperature,
                               units = "mmol/l",
                               pH = i_pH,
                               pe = i_pe,
                               "Alkalinity" = i_HCO3,
                               "Fe" = i_Fe,
                               "Mn" = i_Mn,
                               "Ca" = i_Ca,
                               "K" = i_K,
                               "Si" = i_Si,
                               "Na" = i_Na,
                               "Mg" = i_Mg,
                               "S(6)" = i_SO4
                               # "N(5)" = i_NO3,
                               # "P" = i_PO4
  ) +
    
    selected_output(si = si_output_selections, 
                    molalities = molalities_output_selections,
                    totals = totals_output_selections)
}


# Get PhreeqC output 

get_phreeqC_output <- function(data_in)
{
  
j_sample <- 1

for(i_sample in 1:nrow(data_in)){
  
  i_solution <- data_in[i_sample, ]
  i_phreeqC_input <- get_phreeqC_input(solution_data = i_solution, 
                                       solution_number = i_sample,
                                       si_output_selections = si_output_list,
                                       molalities_output_selections = molalities_output_list,
                                       totals_output_selections = totals_output_list)
  tryCatch(
    {
      i_output <- phreeqc(i_phreeqC_input)
      i_output$Sample_ID <- i_solution$Sample_ID
      i_output$Study_ID <- i_solution$Study_ID
      
      if(j_sample == 1){all_phreeqC_output <- i_output} 
      else { all_phreeqC_output <- bind_rows(all_phreeqC_output, i_output) }
      
      j_sample <- j_sample + 1
    }, 
    
    error = function(e) {print("error")},
    warning = function(w){
      i_output <- phreeqc(i_phreeqC_input)
      
      if(j_sample == 1){
        all_phreeqC_output <- i_output
        i_output$Sample_ID <- i_solution$Sample_ID
        i_output$Study_ID <- i_solution$Study_ID
        
      } else{ all_phreeqC_output <- bind_rows(all_phreeqC_output, i_output) }
      
      j_sample <- j_sample + 1
    }
  )  
}

return(all_phreeqC_output)
}

