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
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
arch_attri <- rast(here::here("data/processed/arch_attri_2024-05-29.tif"))
mill_change_cap <- rast(here::here("data/processed/millchangecap_interp-2.tif"))
prec_seas <- rast(here::here("data/processed/prec_seas_3000m.tif"))
temp_seas <- rast(here::here("data/processed/temp_seas_3000m.tif"))
roughness <- rast(here::here("data/processed/roughness_3000m.tif"))
trav_time <- rast(here::here("data/processed/trav_time_3000m.tif"))

## original rasters
tree_age <- rast(here::here("data/original/NA_TreeAge_1096/data/conus_age06_1km.tif"))
for_type <- rast(here::here("data/original/conus_foresttype.img"))

#---Process the original rasters----

## For forest type group into either hardwood or softwood
hardwood <- c("Oak - Pine Group", "Oak - Hickory Group", "Oak - Gum - Cypress Group", 
              "Elm - Ash - Cottonwood Group", "Maple - Beech - Birch Group", 
              "Tropical Hardwoods Group", "Aspen - Birch Group", "Exotic Hardwoos Group", 
              "Alder - Maple Group", "Wester Oak Group", "Tanoak - Laurel Group", 
              "Other Western Hardwoods Group")
softwood <- c("White - Red - Jack Pine Group", "Spruce - Fir Group", "Longleaf - Slash Pine Group",
              "Loblolly - Shortleaf Pine Group", "Exotic Softwoods Group", "Douglas-fir Group",
              "Ponderosa Pine Group", "Pinyon - Juniper Group", "Western White Pine Group", 
              "Fir - Spruce - Mountain Hemlock Group", "Lodgepole Pine Group", 
              "Hemlock - Sitka Spruce Group", "Western Larch Group", "Redwood Group", 
              "Other Western Softwoods Group", "California Mixed Conifer Group")

identical(crs(ref_rast), crs(tree_age))
identical(crs(ref_rast), crs(for_type))

resamp <- function(raster, ref_raster, method){
  rast_proj <- project(raster, crs(ref_raster))
  rast_resamp <- resample(rast_proj, ref_raster, method)
}

tree_age_resamp <- resamp(tree_age, ref_rast, "bilinear")
for_type_resamp <- resamp(for_type, ref_rast, "bilinear")
mill_change_resamp <- resamp(mill_change_cap, ref_rast, "bilinear")

# Check alignment and stack the rasters
rast_stack <- c(arch_attri, ref_rast, mill_change_resamp, prec_seas, temp_seas, 
                roughness, trav_time, tree_age_resamp)

## Save the raster
writeRaster(x = rast_stack, filename = paste0(here::here("data/toy/processed/"), "full_rast_stack_attributes_", Sys.Date(), ".tif"), overwrite = TRUE)

#---Process raster for use with geocmeans----

# Update the names (optional) 
names(rast_stack)

# Select Variables for FCmeans
#rst_fcm <- rast_stack[[c("R_NET_M", "pct_pay", "sghts_p", "gov_p",
#                         "ave_dem", "lsscll_", "WHP", "CL_ELEV", 
#                         "distance_to_wilderness_m", "last")]]
rst_fcm <- rast_stack
names(rst_fcm) <- c("pct_for_pay", "pct_forest", "forest_depend", "net_mig", 
                    "less_hs", "house_burd", "ener_burd", "pm25", 
                    "pct_sightsee_pay", "pct_gov_pay", "fed_area", "fed_evenness",
                    "wildfire_haz", "mill_cap_5yr_chng", "precip_seas", "temp_seas",
                    "roughness", "travel_time_to_cities", "tree_age")

writeRaster(rst_fcm, paste0("data/toy/processed/rast_fcm_", Sys.Date(), ".tif"), overwrite = TRUE)

