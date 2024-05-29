## Download and process raster data of temperature and precipitation
## Download and process travel time data
## Download elevation data and calculate a topographic complexity index

# 0. Load libraries
library(sf) # for working with vector data
library(raster) # for working with rasters
library(sp) # for working with spatial (vector) data
library(geodata) # this package removed from CRAN May 27, 2024...
library(terra) # for working with rasters
library(tigris) # needed for state/CONUS boundaries
library(dplyr) # for manipulating dataframes 
library(dismo) # needed to calculate biovars

# Steps:
# 0. Load libraries
# 1. Download the rasters (vars = "bio")
# 3. Aggregate or disaggregate to 1.5km and 3km resolution
# 2. Crop to contiguous US
# 4. Set to analysis projection

# 1. Download the data using the geodata package

r_prec <- geodata::worldclim_country(country = "USA", var = "prec", res = 0.5, path = here::here("data/toy/original/"))
r_tmin <- geodata::worldclim_country(country = "USA", var = "tmin", res = 0.5, path = here::here("data/toy/original"))
r_tmax <- geodata::worldclim_country(country = "USA", var = "tmax", res = 0.5, path = here::here("data/toy/original"))
r_ele <- geodata::elevation_30s(country = "US", path = here::here("data/toy/original/"))
r_tt <- geodata::travel_time(to = "city", size = 7, up = TRUE, path = here::here("data/toy/original/"))

# 2. Crop to ID, MT, WA, and OR
## Using tigris, download the state boundaries
states <- tigris::states(cb = TRUE)

## Filter for ID, MT, WA, and OR
states <- states %>%
  filter(STUSPS == "ID" | STUSPS == "MT" | STUSPS == "WA" | STUSPS == "OR")

## Function to crop to states extents, then reproject and crop again with mask = TRUE

crop_project <- function(raster, states){
  r_crop <- crop(raster, ext(states)) # First, crop to the extents of states
  tmp <- project(r_crop, states) # Then, reproject the raster to crs of states
  tmp_conus <- crop(tmp, states, mask = TRUE) # Finally, crop again with mask = TRUE
}

r_prec_conus <- crop_project(r_prec, states)
r_ele_conus <- crop_project(r_ele, states)
r_tt_conus <- crop_project(r_tt, states)
r_tmin_conus <- crop_project(r_tmin, states)
r_tmax_conus <- crop_project(r_tmax, states)

# 3. Resample to 3km resolution
## Using the WHP raster as a reference
ref_rast3 <- rast(here::here("data/toy/processed/merged/WHP_merge3000m.tif"))

resamp <- function(raster, ref_raster, method){
  rast_proj <- project(raster, crs(ref_raster))
  rast_resamp <- resample(rast_proj, ref_raster, method)
}

r_tt_3000 <- resamp(r_tt_conus, ref_rast3, "bilinear")
r_prec_3000 <- resamp(r_prec_conus, ref_rast3, "bilinear")
r_tmin_3000 <- resamp(r_tmin_conus, ref_rast3, "bilinear")
r_tmax_3000 <- resamp(r_tmax_conus, ref_rast3, "bilinear")

## Calculate the elevation "roughness" and resample
rough <- terrain(r_ele_conus, v = "roughness")

r_rough_3000 <- resamp(rough, ref_rast3, "bilinear")

## Calculate the "biovars". Only want precip (bio15) and temp (bio4) seasonality
bio_conus_3000 <- biovars(brick(r_prec_3000), brick(r_tmin_3000), brick(r_tmax_3000))

r_prec_seas_3000 <- bio_conus_3000$bio15
r_temp_seas_3000 <- bio_conus_3000$bio4

# Write rasters (or create a stack and then write?)
writeRaster(r_tt_3000, here::here("data/toy/processed/trav_time_3000m.tif"), overwrite = TRUE)
writeRaster(r_rough_3000, here::here("data/toy/processed/roughness_3000m.tif"), overwrite = TRUE)
writeRaster(r_prec_seas_3000, here::here("data/toy/processed/prec_seas_3000m.tif"), overwrite = TRUE)
writeRaster(r_temp_seas_3000, here::here("data/toy/processed/temp_seas_3000m.tif"), overwrite = TRUE)
