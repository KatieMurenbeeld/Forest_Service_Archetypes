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

projection <- "epsg:5070"

#---Load the data-----
whp_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
forgain_rast <- rast(here::here("data/processed/forestgain_merged/forestgain_merge3000m.tif"))
arch_attri <- rast(here::here("data/processed/arch_attri_2024-05-31.tif"))
mill_change_cap <- rast(here::here("data/processed/millchangecap_interp-2.tif"))
prec_seas <- rast(here::here("data/processed/prec_seas_3000m.tif"))
temp_seas <- rast(here::here("data/processed/temp_seas_3000m.tif"))
roughness <- rast(here::here("data/processed/roughness_3000m.tif"))
trav_time <- rast(here::here("data/processed/trav_time_3000m.tif"))

## original rasters
tree_cover <- rast(here::here("data/original/nlcd_tcc_CONUS_2016_v2021-4.tif"))
tree_age <- rast(here::here("data/original/NA_TreeAge_1096/data/conus_age06_1km.tif"))
for_own <- rast(here::here("data/original/Data/forest_own1/forest_own1.tif"))

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

#---Process the original rasters----

# reproject and aggregate the og rasters
# tree cover
tree_cover_agg <- aggregate(tree_cover, fact = 100, na.rm = TRUE)
tree_cover_agg
plot(tree_cover_agg)

tree_cover_resamp <- resamp(tree_cover$test, whp_rast_proj, "bilinear")
tree_cover_resamp
plot(tree_cover_resamp)

# stand age
tree_age_resamp <- resamp(tree_age, whp_rast_proj, "bilinear")

tree_age_proj <- project(tree_age, projection)
tree_age_agg <- aggregate(tree_age_proj, fact = 3, fun = "mean", na.rm = TRUE)

tree_age_resamp <- resamp(tree_age_proj, whp_rast_proj, "bilinear")
plot(tree_age_proj)
plot(tree_age_agg)
plot(tree_age_resamp)
saveRDS(tree_age_proj, file = here::here("data/processed/tree_age_proj.rds"))
saveRDS(tree_age_agg, file = here::here("data/processed/tree_age_agg.rds"))
saveRDS(tree_age_resamp, file = here::here("data/processed/tree_age_resamp.rds"))

# forest ownership
# Before resampling forest ownership calculate the percent area that is private
## reclassify the raster
### make reclassification matrix
m <- c(1, 4, 1, 
       4, 8, 0) 
#       NA, NA, 0)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)

### reclassify using matrix, make NA = 0
for_own_rc <- classify(for_own, rclmat, include.lowest=TRUE, others = 0)
plot(for_own)
plot(for_own_rc)
for_own_rc_agg <- aggregate(for_own_rc, fact = 100, fun = "mean", na.rm = TRUE)
for_own_rc_agg_naf <- aggregate(for_own_rc, fact = 100, fun = "mean", na.rm = FALSE)
for_own_rc_resamp <- resamp(for_own_rc, whp_rast_proj, "near") # this took about 2 hours to run. Reprojecting the forest owner tif seems to take a very long time. 
for_own_rc_resamp_ave <- resamp(for_own_rc, whp_rast_proj, "average")
saveRDS(for_own_rc_resamp, file = here::here("data/processed/for_own_rc_resamp.rds"))
saveRDS(for_own_rc_resamp_ave, file = here::here("data/processed/for_own_rc_resamp_ave.rds"))
saveRDS(for_own_rc, file = here::here("data/processed/for_own_rc.rds"))
saveRDS(for_own_rc_agg, file = here::here("data/processed/for_own_rc_agg.rds"))
saveRDS(for_own_rc_agg_naf, file = here::here("data/processed/for_own_rc_agg_naf.rds"))

for_own_rc_resamp <- readRDS(here::here("data/processed/for_own_rc_resamp.rds"))
for_own_rc_resamp_ave <- readRDS(here::here("data/processed/for_own_rc_resamp_ave.rds"))
for_own_rc <- readRDS(here::here("data/processed/for_own_rc.rds"))
for_own_rc_agg <- readRDS(here::here("data/processed/for_own_rc_agg.rds"))
for_own_rc_agg_naf <- readRDS(here::here("data/processed/for_own_rc_agg_naf.rds"))

for_own_rc_agg
for_own_rc_agg_naf
for_own_rc
for_own_rc_resamp
plot(for_own_rc_agg)
plot(for_own_rc_agg_naf)
plot(for_own_rc)
plot(for_own_rc_resamp)

#for_own_rc[is.na(for_own_rc[])] <- 0 

### double check plot
plot(for_own_rc)

## try using terra::extract() or zonal() to get fraction of county covered by raster
# test using exactextractr::frac()? Make a sf of gridcells like in the PADUS script?


# Check alignment and stack the rasters
rast_stack <- c(arch_attri, ref_rast, mill_change_resamp, prec_seas, temp_seas, 
                roughness, trav_time, tree_age_resamp, privfor_resamp)

## Save the raster
writeRaster(x = rast_stack, filename = paste0(here::here("data/processed/"), "full_rast_stack_attributes_", Sys.Date(), ".tif"), overwrite = TRUE)

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






