library(tidyverse)
library(sf)
library(sp)
library(raster)
library(stars)

args <- commandArgs(trailingOnly = F)

if (length(args) < 3) {
  stop("Please supply the recent date of gw_data_spread_DATE.rds, and the Output and Script_Support_Files folder locations", call.=FALSE)
} else if (length(args) > 3) {
  stop("Please provide only three arguments", call.=FALSE)
}

recentRunDate <- args[[1]]
OP_loc <- args[[2]]
SS_loc <- args[[3]]

print("Loading in Data and Support Files")
gw_data_spread <- readRDS(file = paste0(OP_loc, "/gw_data_spread_", recentRunDate, ".rds"))

watertable_depth_global <- raster(paste0(SS_loc,"/Global_model_hires_wtd.tif"))

basin_shape <- st_read(paste0(SS_loc,"/Basins.shp"))

print("Done Loading")

print("Getting Coordinates and creating spatial df")
# Create data frame with coordinates for each sample site
gw_coords_df <- gw_data_spread %>%
  dplyr::select("Sample_ID","Study_ID","Lat","Long") %>%
  filter(!is.na(Lat), !is.na(Long))


# create spatial object containing the sample site coordinates
gw_coords = st_as_sf(gw_coords_df, coords = c("Long", "Lat"), dim = "XY",
                     crs = st_crs(watertable_depth_global, asText = TRUE))


gw_coords <- st_transform(gw_coords, crs = st_crs(basin_shape))


print('Simplifying and Extracting Settings')

basin_shape_simplify <- st_simplify(basin_shape, preserveTopology = TRUE, dTolerance = 5000) %>%
  st_make_valid()
proc.time() - t_start

rm(watertable_depth_global, basin_shape, gw_coords_df) # Clear up RAM for intersection

#tectonic_setting <- st_intersection(gw_coords, basin_shape_simplify)

print('Saving Output')

# tectonic_setting %>% saveRDS(file = paste0(OP_loc, "/tectonic_setting_", Sys.Date(), ".rds"))




