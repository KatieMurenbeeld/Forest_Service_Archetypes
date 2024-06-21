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
fed_rich <- rast(here::here("data/processed/conus_fed_rich_2024-06-12.tif"))
fed_pct_area <- rast(here::here("data/processed/conus_fed_pctarea_2024-06-20.tif"))
forgain_rast <- rast(here::here("data/processed/forestgain_merged/forestgain_merge3000m.tif"))
## Load in previous raster stack (no areafed, no fedrich, no privfor)
rast_stack_no <- rast(here::here("data/processed/rast_stack_attributes_no_areafed_no_fedrich_no_privfor_2024-06-12.tif"))

# reproject whp_rast which will be used as the reference raster

whp_rast_proj <- project(whp_rast, projection)

# Create a resample function
resamp <- function(raster, ref_raster, method){
  rast_proj <- project(raster, crs(ref_raster))
  rast_resamp <- resample(rast_proj, ref_raster, method, threads = TRUE)
}

forgain_resamp <- resamp(forgain_rast, whp_rast_proj, "bilinear")
forgain_resamp_crop <- crop(forgain_resamp, whp_rast_proj, mask = TRUE)

fed_rich[is.na(fed_rich)] <- 0
fed_rich_resamp <- resamp(fed_rich, whp_rast_proj, "near")
fed_rich_crop <- crop(fed_rich_resamp, rast_stack_no$pct_pay, mask = TRUE)

fed_pct_area[is.na(fed_pct_area)] <- 0
fed_pct_area_resamp <- resamp(fed_pct_area, whp_rast_proj, "near")
fed_pct_area_crop <- crop(fed_pct_area_resamp, rast_stack_no$pct_pay, mask = TRUE)

## Stack on the final attributes
rast_stack <- c(rast_stack_no, fed_rich_crop, fed_pct_area_crop, forgain_resamp_crop)

stand_age <- rast_stack$conus_age06_1km
stand_age[is.na(stand_age)] <- 0 
stand_age_crop <- crop(stand_age, rast_stack$pct_pay, mask = TRUE)
names(stand_age_crop)
names(stand_age_crop) <- "stand_age"
rast_stack_treeage <- c(rast_stack, stand_age_crop)


# I think I may want to run a focal on all layers to get rid of more NAs? make sure to rename columns
names(rast_stack_treeage)
s <- sapp(rast_stack_treeage, fun = function(x, ...) {focal(x, fun = "mean", w = matrix(1, 3, 3), na.rm = TRUE, na.policy="only")})

for (i in 1:length(names(rast_stack_treeage))) {
  #print(names(rast_stack_treea_forprod[[i]]))
  names(s[[i]]) <- paste0(names(rast_stack_treeage[[i]]), "_focal_mean")
}
#names(s)

forest_prod <- rast_stack$mn_prd_
forest_prod_focal <- focal(forest_prod, w = matrix(1, 27, 27), fun = "mean", na.rm = TRUE)
forest_prod_crop <- crop(forest_prod_focal, rast_stack$pct_pay, mask = TRUE)
names(forest_prod_crop) <- "forest_prod_focal_mean"

# create final raster stack
rast_stack_treea_forprod <- c(s, forest_prod_crop)

rast_stack_fcm <- rast_stack_treea_forprod[[-17]]
names(rast_stack_fcm)  
rast_stack_fcm <- rast_stack_fcm[[-8]]
plot(rast_stack_fcm[[1:10]])
plot(rast_stack_fcm[[11:19]])

## save the raster stack for use in 04_archetype-analysis_fcm-analysis.R
writeRaster(rast_stack_fcm, here::here(paste0("data/processed/rast_stack_noprivfor_", Sys.Date(), ".tif")), overwrite = TRUE)

#---Process raster for use with geocmeans----

# Update the names (optional) 
names(rast_stack_fcm)

# Select Variables for FCmeans
rst_fcm <- rast_stack_fcm[[c("pct_pay_focal_mean", "COMMCAP_focal_mean", "R_NET_M_focal_mean",
                "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean",                 
                "PM25F_PFS_focal_mean", "distance_to_wilderness_m_focal_mean",
                "distance_to_crithab_m_focal_mean", "WHP_focal_mean", "millcap_5yr.pred_focal_mean",
                "prec_seas_3000m_focal_mean", "temp_seas_3000m_focal_mean", "roughness_focal_mean",
                "travel_time_to_cities_1_focal_mean", "numfed_focal_mean", "Layer_1_focal_mean",
                "stand_age_focal_mean", "forest_prod_focal_mean")]]

rst_fcm_socecon <- rast_stack_fcm[[c("pct_pay_focal_mean", "COMMCAP_focal_mean", "R_NET_M_focal_mean",
                                     "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean",                 
                                     "PM25F_PFS_focal_mean", "distance_to_wilderness_m_focal_mean",
                                     "distance_to_crithab_m_focal_mean", "millcap_5yr.pred_focal_mean",
                                     "travel_time_to_cities_1_focal_mean")]]

rst_fcm_soc <- rast_stack_fcm[[c("pct_pay_focal_mean", "COMMCAP_focal_mean", "R_NET_M_focal_mean",
                                 "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean",                 
                                 "PM25F_PFS_focal_mean", "travel_time_to_cities_1_focal_mean")]]

rst_fcm_forecol <- rast_stack_fcm[[c("WHP_focal_mean",
                             "prec_seas_3000m_focal_mean", "temp_seas_3000m_focal_mean", "roughness_focal_mean",
                             "Layer_1_focal_mean",
                             "stand_age_focal_mean", "forest_prod_focal_mean")]]
# rename

names(rst_fcm) <- c("pct_pay_focal_mean", "COMMCAP_focal_mean", "R_NET_M_focal_mean",
                      "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean",                 
                      "PM25F_PFS_focal_mean", "distance_to_wilderness_m_focal_mean",
                      "distance_to_crithab_m_focal_mean", "WHP_focal_mean", "millcap_5yr.pred_focal_mean",
                      "prec_seas_3000m_focal_mean", "temp_seas_3000m_focal_mean", "roughness_focal_mean",
                      "travel_time_to_cities_focal_mean", "numfed_focal_mean", "tree_cov_focal_mean",
                      "stand_age_focal_mean", "for_prod_focal_mean")

names(rst_fcm_socecon) <- c("pct_pay_focal_mean", "COMMCAP_focal_mean", "R_NET_M_focal_mean",
                    "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean",                 
                    "PM25F_PFS_focal_mean", "distance_to_wilderness_m_focal_mean",
                    "distance_to_crithab_m_focal_mean", "millcap_5yr.pred_focal_mean",
                    "travel_time_to_cities_focal_mean")

names(rst_fcm_ecol) <- c("WHP_focal_mean",
                    "prec_seas_3000m_focal_mean", "temp_seas_3000m_focal_mean", "roughness_focal_mean",
                    "tree_cov_focal_mean",
                    "stand_age_focal_mean", "for_prod_focal_mean")

writeRaster(rst_fcm_ecol, paste0("data/rast_fcm_10_", Sys.Date(), ".tif"), overwrite = TRUE)

# Scale the data
rst_fcm_ecol_sc <- scale(rst_fcm_ecol)

# Quickly investigate the correlation between the attributes
correlation <- layerCor(rst_fcm_ecol, "pearson", na.rm = TRUE)
fcm_cor <- as.data.frame(correlation$pearson)
write_csv(fcm_cor, here::here("outputs/fcm_10_correlation.csv"), append = FALSE)

# Convert to a simple list of SpatRaster
dataset <- lapply(names(rst_fcm_ecol_sc), function(n){
  aband <- rst_fcm_ecol_sc[[n]]
  return(aband)
})

names(dataset) <- names(rst_fcm_ecol_sc)
names(dataset)

#---Find appropriate parameter values----
### Code below from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html
# finding an appropriate k and m values
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                      k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                      indices = c("Explained.inertia",
                                                  "Silhouette.index"),
                                      verbose = TRUE)

# plotting the silhouette index values
sil.idx <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx
ggsave(paste0("figures/FCM_10_sil_idx_", Sys.Date(), ".png"), sil.idx, 
       width = 12, height = 12, dpi = 300)

# plotting the explained inertia
ex.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert
ggsave(paste0("figures/FCM_10_ex_inert_", Sys.Date(), ".png"), ex.inert, 
       width = 12, height = 12, dpi = 300)

#---Run the FCM---- 
FCM_result <- CMeans(dataset, k = 8, m = 1.625, standardize = FALSE, seed = 6891) #creates spider plot! plot(FCM_result)
saveRDS(FCM_result, here::here("data/FCM_result_16_standFALSE.rds"))
map.res <- rast(FCM_result$rasters)
writeRaster(map.res[["Groups"]], filename = paste0("data/FCM_16_", Sys.Date(), ".tif"), overwrite = TRUE)

plot(map.res[["Groups"]])
plot(FCM_result)
#fcm_belong <- as.data.frame(FCM_result$Belongings)
#hist(fcm_belong$V1)

#df <- as.data.frame(rst_fcm, na.rm = TRUE)
#violinPlots(df, FCM_result$Groups[[1]])

#---Run a bootstrap validation
validation_fcm <- boot_group_validation.mc(FCM_result, nsim = 100, maxiter = 100,
                                       tol = 0.01, init = "random")
val_grp_con <- validation_fcm$group_consistency


#map.groups <- FCM_result$Groups

##---save the iteration, k, m as a dataframe----
aa_iteration <- data.frame(iteration_name = character(),
                           attris = character(),
                           k = numeric(),
                           m = numeric())

iteration_name <- "FCM_15"
attris <- paste(names(rst_fcm), collapse= ", ")
k <- 10
m <- 1.625

aa_iteration[nrow(aa_iteration) + 1,] <- list(iteration_name, attris, k, m)

write_csv(aa_iteration, here::here("outputs/aa_iteration.csv"), append = TRUE)






