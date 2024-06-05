## Download and process raster data of temperature and precipitation
## Download and process travel time data
## Download elevation data and calculate a topographic complexity index

# 0. Load libraries and set projection
library(sf) # for working with vector data
library(raster) # for working with rasters
library(sp) # for working with spatial (vector) data
library(geodata) # this package removed from CRAN May 27, 2024...
library(terra) # for working with rasters
library(tigris) # needed for state/CONUS boundaries
library(dplyr) # for manipulating dataframes 
library(dismo) # needed to calculate biovars

projection <- "epsg:5070"
#proj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83"

# Steps:
# 0. Load libraries and set projection
# 1. Download the rasters (vars = "bio")
# 3. Aggregate or disaggregate to 1.5km and 3km resolution
# 2. Crop to contiguous US
# 4. Set to analysis projection

# 1. Download the data using the geodata package

r_prec <- geodata::worldclim_country(country = "USA", var = "prec", res = 0.5, path = here::here("data/original/"))
r_tmin <- geodata::worldclim_country(country = "USA", var = "tmin", res = 0.5, path = here::here("data/original"))
r_tmax <- geodata::worldclim_country(country = "USA", var = "tmax", res = 0.5, path = here::here("data/original"))
r_ele <- geodata::elevation_30s(country = "US", path = here::here("data/original/"))
r_tt <- geodata::travel_time(to = "city", size = 7, up = TRUE, path = here::here("data/original/"))

# 2. Crop to CONUS
## Using tigris, download the state boundaries
states <- tigris::states(cb = TRUE)

## Filter out Alaska, Hawaii, DC, and territories
states <- states %>%
  filter(STUSPS != "AK" & STUSPS != "HI" & STUSPS != "DC") %>%
  filter(GEOID < 60)

# Set projection for states
states_proj <- st_transform(states, projection)

## Function to crop to states extents, then reproject and crop again with mask = TRUE

r_tt_crop1 <- crop(r_tt, ext(states))
r_tt_proj <- project(r_tt_crop1, projection)
r_tt_crop <- crop(r_tt_proj, states_proj, mask = TRUE)

crop_project <- function(raster, states, states_proj){
  r_crop <- crop(raster, ext(states)) # First, crop to the extents of states
  tmp <- project(r_crop, projection) # Then, reproject the raster to crs of states
  tmp_conus <- crop(tmp, states_proj, mask = TRUE) # Finally, crop again with mask = TRUE
}

r_prec_conus <- crop_project(r_prec, states, states_proj)
r_ele_conus <- crop_project(r_ele, states, states_proj)
r_tt_conus <- crop_project(r_tt, states, states_proj)
r_tmin_conus <- crop_project(r_tmin, states, states_proj)
r_tmax_conus <- crop_project(r_tmax, states, states_proj)

# check the plots
plot(r_prec_conus)
plot(r_ele_conus)
plot(r_tt_conus)
plot(r_tmin_conus)
plot(r_tmax_conus)

# calculate elevation roughness and biovars
rough_conus <- terrain(r_ele_conus, v = "roughness")

bio_conus <- biovars(brick(r_prec_conus), brick(r_tmin_conus), brick(r_tmax_conus))

r_temp_seas_conus <- bio_conus$bio4
r_prec_seas_conus <- bio_conus$bio15

# 3. Aggregate to 3km resolution

# 3. Resample to 1.5km and 3km resolution
## Using the WHP raster as a reference
ref_rast1.5 <- rast(here::here("data/processed/merged/WHP_merge1500m.tif"))
ref_rast3 <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))

resamp <- function(raster, ref_raster, method){
  rast_proj <- project(raster, crs(ref_raster))
  rast_resamp <- resample(rast_proj, ref_raster, method)
}

r_tt_1500 <- resamp(r_tt_conus, ref_rast1.5, "bilinear")
r_tt_3000 <- resamp(r_tt_conus, ref_rast3, "bilinear")
r_prec_1500 <- resamp(r_prec_conus, ref_rast1.5, "bilinear")
r_prec_3000 <- resamp(r_prec_conus, ref_rast3, "bilinear")
r_tmin_1500 <- resamp(r_tmin_conus, ref_rast1.5, "bilinear")
r_tmin_3000 <- resamp(r_tmin_conus, ref_rast3, "bilinear")
r_tmax_1500 <- resamp(r_tmax_conus, ref_rast1.5, "bilinear")
r_tmax_3000 <- resamp(r_tmax_conus, ref_rast3, "bilinear")
r_temp_1500 <- resamp(r_temp_conus, ref_rast1.5, "bilinear")
r_temp_3000 <- resamp(r_temp_conus, ref_rast3, "bilinear")

## Calculate the elevation "roughness" and resample
rough <- terrain(r_ele_conus, v = "roughness")

r_rough_1500 <- resamp(rough, ref_rast1.5, "bilinear")
r_rough_3000 <- resamp(rough, ref_rast3, "bilinear")

## Calculate the "biovars". Only want precip (bio15) and temp (bio4) seasonality
bio_conus_1500 <- biovars(brick(r_prec_1500), brick(r_tmin_1500), brick(r_tmax_1500))
bio_conus_3000 <- biovars(brick(r_prec_3000), brick(r_tmin_3000), brick(r_tmax_3000))

r_prec_seas_1500 <- bio_conus_1500$bio15
r_temp_seas_1500 <- bio_conus_1500$bio4
r_prec_seas_3000 <- bio_conus_3000$bio15
r_temp_seas_3000 <- bio_conus_3000$bio4

# Write rasters (or create a stack and then write?)
writeRaster(r_tt_1500, here::here("data/processed/trav_time_1500m.tif"), overwrite = TRUE)
writeRaster(r_tt_3000, here::here("data/processed/trav_time_3000m.tif"), overwrite = TRUE)
writeRaster(r_rough_1500, here::here("data/processed/roughness_1500m.tif"), overwrite = TRUE)
writeRaster(r_rough_3000, here::here("data/processed/roughness_3000m.tif"), overwrite = TRUE)
writeRaster(r_prec_seas_1500, here::here("data/processed/prec_seas_1500m.tif"), overwrite = TRUE)
writeRaster(r_prec_seas_3000, here::here("data/processed/prec_seas_3000m.tif"), overwrite = TRUE)
writeRaster(r_temp_seas_1500, here::here("data/processed/temp_seas_1500m.tif"), overwrite = TRUE)
writeRaster(r_temp_seas_3000, here::here("data/processed/temp_seas_3000m.tif"), overwrite = TRUE)

rast_stack_1500 <- c(r_tt_1500, r_rough_1500, r_prec_1500, r_temp_1500)
rast_stack_3000 <- c(r_tt_3000, r_rough_3000, r_prec_3000, r_temp_3000)
writeRaster(rast_stack_1500, here::here("data/processed/clim_tt_1500.tif"), overwrite = TRUE)
writeRaster(rast_stack_3000, here::here("data/processed/clim_tt_3000.tif"), overwrite = TRUE)

