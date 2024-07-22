## some code came from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html
## Edit this script so that it is for processing the rasters and making a raster stack
## the fcm bit can go in a separate script

library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(raster)
library(exactextractr) # I want this package but cannot seem to install because I was spelling it wrong :-/
library(tigris)

projection <- "epsg:5070"

#---Load the data-----
whp_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
forgain_rast <- rast(here::here("data/processed/forestgain_merged/forestgain_merge3000m.tif"))
arch_attri <- rast(here::here("data/processed/arch_attri_2024-07-19.tif"))
mill_change_cap <- rast(here::here("data/processed/millchangecap_interp-2.tif"))
prec_seas <- rast(here::here("data/processed/prec_seas_3000m.tif"))
temp_seas <- rast(here::here("data/processed/temp_seas_3000m.tif"))
roughness <- rast(here::here("data/processed/roughness_3000m.tif"))
trav_time <- rast(here::here("data/processed/trav_time_3000m.tif"))

## original rasters
#tree_cover <- rast(here::here("data/original/nlcd_tcc_CONUS_2016_v2021-4.tif"))
#tree_age <- rast(here::here("data/original/NA_TreeAge_1096/data/conus_age06_1km.tif"))
#for_own <- rast(here::here("data/original/Data/forest_own1/forest_own1.tif"))

# reproject whp_rast which will be used as the reference raster

whp_rast_proj <- project(whp_rast, projection)

# Create a resample function
resamp <- function(raster, ref_raster, method){
  rast_proj <- project(raster, crs(ref_raster))
  rast_resamp <- resample(rast_proj, ref_raster, method, threads = TRUE)
}

forgain_resamp <- resamp(forgain_rast, whp_rast_proj, "bilinear")
forgain_resamp_crop <- crop(forgain_resamp, whp_rast_proj, mask = TRUE)
arch_attri_resamp <- resamp(arch_attri, whp_rast_proj, "bilinear")
mill_change_resamp <- resamp(mill_change_cap, whp_rast_proj, "bilinear")
prec_seas_resamp <- resamp(prec_seas, whp_rast_proj, "bilinear")
temp_seas_resamp <- resamp(temp_seas, whp_rast_proj, "bilinear")
roughness_resamp <- resamp(roughness, whp_rast_proj, "bilinear")
trav_time_resamp <- resamp(trav_time, whp_rast_proj, "bilinear")

# select the attributes from arch_attri_resamp
#arch_attri_select <- arch_attri %>%
#  dplyr::select()


#---Process the original rasters----

# reproject and aggregate the og rasters
# tree cover
#tree_cover
#tree_cover_proj <- project(tree_cover, projection)
#tree_cover_proj_subs <- subst(tree_cover_proj, 254:255, 0)
#tree_cover_proj_resamp_ave <- resample(tree_cover_proj_subs, whp_rast_proj, "average", threads = TRUE)
#tree_cover_proj_crop <- crop(tree_cover_proj_resamp_ave, whp_rast_proj, mask = TRUE)
#plot(tree_cover_proj_crop)
#writeRaster(tree_cover_proj_crop, here::here("data/processed/tree_cover_cover_resamp.tif"))

tree_cover_proj_crop <- rast(here::here("data/processed/tree_cover_cover_resamp.tif"))

# stand age
#tree_age_proj <- project(tree_age, projection)
#tree_age_agg_proj <- aggregate(tree_age_proj, fact = 3, fun = "mean", na.rm = TRUE)
#tree_age_resamp <- resamp(tree_age_agg_proj, whp_rast_proj, "bilinear")
#writeRaster(tree_age_resamp, here::here("data/processed/tree_age_conus_resamp.tif"))

tree_age_resamp <- rast(here::here("data/processed/tree_age_conus_resamp.tif"))
# Check alignment and stack the rasters
rast_stack <- c(arch_attri_resamp, whp_rast_proj, mill_change_resamp, prec_seas_resamp, 
                temp_seas_resamp, roughness_resamp, trav_time_resamp, tree_age_resamp, 
                tree_cover_proj_crop, forgain_resamp_crop)

## Save the raster
writeRaster(x = rast_stack, filename = paste0(here::here("data/processed/"), "rast_stack_attributes_fill_", Sys.Date(), ".tif"), overwrite = TRUE)

## Load in previous raster stack (no areafed, no fedrich, no privfor)
rast_stack_no <- rast(here::here("data/processed/rast_stack_attributes_no_..._.tif"))
## Stack on the final attributes
rast_stack <- c(rast_stack_no, fedarea_rst, fedrich_rst, privfor_rst)

