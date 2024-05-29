## some code came from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html

library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)

#---Load the data-----
ref_rast <- rast(here::here("data/toy/processed/merged/WHP_merge3000m.tif"))
arch_attri <- rast(here::here("data/toy/processed/arch_attri_2024-05-29.tif"))
mill_change_cap <- rast(here::here("data/toy/processed/millchangecap_interp-2.tif"))
prec_seas <- rast(here::here("data/toy/processed/prec_seas_3000m.tif"))
temp_seas <- rast(here::here("data/toy/processed/temp_seas_3000m.tif"))
roughness <- rast(here::here("data/toy/processed/roughness_3000m.tif"))
trav_time <- rast(here::here("data/toy/processed/trav_time_3000m.tif"))

# Resample data if needed
resamp <- function(raster, ref_raster, method){
  rast_proj <- project(raster, crs(ref_raster))
  rast_resamp <- resample(rast_proj, ref_raster, method)
}

mill_change_resamp <- resamp(mill_change_cap, ref_rast, "bilinear")

# Check alignment and stack the rasters
rast_stack <- c(arch_attri, ref_rast, mill_change_resamp, prec_seas, temp_seas, 
                roughness, trav_time)

## Save the raster
writeRaster(x = rast_stack, filename = paste0(here::here("data/toy/processed/"), "full_rast_stack_attributes_", Sys.Date(), ".tif"), overwrite = TRUE)

#---Process raster for use with geocmeans----

# Update the names (optional) 
names(rast_stack)

# Select Variables for FCmeans
rst_fcm <- rast_stack[[c("pct_pay", "pct_frs", "R_NET_M",
                         "HSEF", "HBF_PFS", "PM25F_PFS", "coverag",
                         "E", "WHP", "millcap_5yr.pred", "prec_seas_3000m",
                         "temp_seas_3000m", "roughness", "travel_time_to_cities_1")]]

names(rst_fcm) <- c("pct_for_pay", "pct_forest", "net_mig", 
                    "less_hs", "house_burd", "pm25", "fed_area", "fed_evenness", 
                    "wildfire_haz", "mill_cap_5yr_chng", "precip_seas", "temp_seas",
                    "roughness", "travel_time_to_cities")

writeRaster(rst_fcm, paste0("data/toy/processed/toy_rast_fcm_", Sys.Date(), ".tif"), overwrite = TRUE)

