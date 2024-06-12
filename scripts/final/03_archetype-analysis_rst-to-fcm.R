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
arch_attri <- rast(here::here("data/processed/arch_attri_2024-06-11.tif"))
mill_change_cap <- rast(here::here("data/processed/millchangecap_interp-2.tif"))
prec_seas <- rast(here::here("data/processed/prec_seas_3000m.tif"))
temp_seas <- rast(here::here("data/processed/temp_seas_3000m.tif"))
roughness <- rast(here::here("data/processed/roughness_3000m.tif"))
trav_time <- rast(here::here("data/processed/trav_time_3000m.tif"))

## original rasters
tree_cover <- rast(here::here("data/original/nlcd_tcc_CONUS_2016_v2021-4.tif"))
tree_age <- rast(here::here("data/original/NA_TreeAge_1096/data/conus_age06_1km.tif"))
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
tree_cover
tree_cover_proj <- project(tree_cover, projection)
tree_cover_proj_subs <- subst(tree_cover_proj, 254:255, 0)
tree_cover_proj_resamp_ave <- resample(tree_cover_proj_subs, whp_rast_proj, "average", threads = TRUE)
tree_cover_proj_crop <- crop(tree_cover_proj_resamp_ave, whp_rast_proj, mask = TRUE)
plot(tree_cover_proj_crop)
writeRaster(tree_cover_proj_crop, here::here("data/processed/tree_cover_cover_resamp.tif"))
# stand age
tree_age_proj <- project(tree_age, projection)
tree_age_agg_proj <- aggregate(tree_age_proj, fact = 3, fun = "mean", na.rm = TRUE)
tree_age_resamp <- resamp(tree_age_agg_proj, whp_rast_proj, "bilinear")
writeRaster(tree_age_resamp, here::here("data/processed/tree_age_conus_resamp.tif"))
# Check alignment and stack the rasters
rast_stack <- c(arch_attri_resamp, whp_rast_proj, mill_change_resamp, prec_seas_resamp, 
                temp_seas_resamp, roughness_resamp, trav_time_resamp, tree_age_resamp, 
                tree_cover_proj_crop)

## Save the raster
writeRaster(x = rast_stack, filename = paste0(here::here("data/processed/"), "rast_stack_attributes_no_areafed_no_fedrich_no_privfor_", Sys.Date(), ".tif"), overwrite = TRUE)

## Load in previous raster stack (no areafed, no fedrich, no privfor)
rast_stack_no <- rast(here::here("data/processed/rast_stack_attributes_no_..._.tif"))
## Stack on the final attributes
rast_stack <- c(rast_stack_no, fedarea_rst, fedrich_rst, privfor_rst)

#---Process raster for use with geocmeans----

# Update the names (optional) 
names(rast_stack)

# Select Variables for FCmeans
rst_fcm <- rast_stack[[c("pct_pay", "pct_frs", "R_NET_M", "HSEF",
                         "HBF_PFS", "PM25F_PFS", "coverag", "E", 
                         "WHP", "last")]]
rst_fcm <- rast_stack
names(rst_fcm) <- c("pct_for_pay", "pct_forest", "forest_depend", "net_mig", 
                    "less_hs", "house_burd", "ener_burd", "pm25", 
                    "pct_sightsee_pay", "pct_gov_pay", "fed_area", "fed_evenness",
                    "wildfire_haz", "mill_cap_5yr_chng", "precip_seas", "temp_seas",
                    "roughness", "travel_time_to_cities", "tree_age")
writeRaster(rst_fcm, paste0("data/processed/rast_fcm_", Sys.Date(), ".tif"), overwrite = TRUE)

# Scale the data
rst_fcm_sc <- scale(rst_fcm)

# Quickly investigate the correlation between the attributes
correlation <- layerCor(rst_fcm, "pearson", na.rm = TRUE)
fcm_cor <- as.data.frame(correlation$correlation)
#write_csv(fcm_cor, here::here("outputs/fcm_correlation.csv"), append = FALSE)

# Convert to a simple list of SpatRaster
dataset <- lapply(names(rst_fcm_sc), function(n){
  aband <- rst_fcm_sc[[n]]
  return(aband)
})

names(dataset) <- names(rst_fcm_sc)

#---Find appropriate parameter values----
### Code below from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html
# finding an appropriate k and m values
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Silhouette.index"),
                                  verbose = TRUE)

# plotting the silhouette index values
sil.idx <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx
ggsave(here::here("figures/FCM_sil_idx.png"), sil.idx, 
       width = 12, height = 12, dpi = 300)

# plotting the explained inertia
ex.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert
ggsave(here::here("figures/FCM_ex_inert.png"), ex.inert, 
       width = 12, height = 12, dpi = 300)

#---Run the FCM---- 
FCM_result <- CMeans(dataset, k = 5, m = 1.7, standardize = FALSE)
map.res <- rast(FCM_result$rasters)
writeRaster(map.res[["Groups"]], filename = paste0("data/processed/FCM_", Sys.Date(), ".tif"))


##---save the iteration, k, m as a dataframe----
aa_iteration <- data.frame(iteration_name = character(),
                           attris = character(),
                           k = numeric(),
                           m = numeric())

iteration_name <- "FCM_"
attris <- paste(names(rst_fcm), collapse= ", ")
k <- 5
m <- 1.7

aa_iteration[nrow(aa_iteration) + 1,] <- list(iteration_name, attris, k, m)

write_csv(aa_iteration, here::here("outputs/aa_iteration.csv"), append = TRUE)






