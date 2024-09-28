library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(tmap)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(future)
library(spdep)
library(classInt)

#----Load the data----
rst_fcm_pmrc_poli_sc <- rast(here::here("data/processed/rst_fcm_pmrc_poli_sc_2024-08-12.tif"))
rst_fcm_pmrc_poli <- rast(here::here("data/processed/rast_fcm_pmrc2024_ploi_2024-08-12.tif"))

# Format for use in geocmeans
dataset_all_sc <- lapply(names(rst_fcm_pmrc_poli_sc), function(n){
  aband <- rst_fcm_pmrc_poli_sc[[n]]
  return(aband)
})
names(dataset_all_sc) <- names(rst_fcm_pmrc_poli_sc)

# Create the social raster stack
soc_rast_sc <- raster::subset(rst_fcm_pmrc_poli_sc, c("pct_forpay_fill", "commcap_fill", 
                                                      "net_mig_fill", "less_hs_fill",
                                                      "houseburd_fill", "enerburd_fill", 
                                                      "pm25_fill", "millcap_5yr", 
                                                      "travel_time_to_cities_20k",
                                                      "num_federal"))
soc_rast_sc_crop <- crop(soc_rast_sc, rst_fcm_pmrc_poli_sc, mask = TRUE)

# Format for use in geocmeans
dataset_soc_sc <- lapply(names(soc_rast_sc), function(n){
  aband <- soc_rast_sc[[n]]
  return(aband)
})
names(dataset_soc_sc) <- names(soc_rast_sc)

# Create the ecological raster stack
eco_rast_sc <- raster::subset(rst_fcm_pmrc_poli_sc, c("mean_forprod_fia", "WHP",
                                                      "prec_seas_3000m", "temp_seas_3000m",
                                                      "roughness", "tree_cover", 
                                                      "forest_gain", "stand_age"))

eco_rast_sc_crop <- crop(eco_rast_sc, rst_fcm_pmrc_poli_sc, mask = TRUE)

# Format for use in geocmeans
dataset_eco_sc <- lapply(names(eco_rast_sc), function(n){
  aband <- eco_rast_sc[[n]]
  return(aband)
})
names(dataset_eco_sc) <- names(eco_rast_sc)

# Create the SGFCM results for all, social, and ecological attributes

# Set the window size
w1 <- matrix(1, nrow = 3, ncol = 3)

# Create the clusters
SGFCM_all_result <- SGFCMeans(dataset_all_sc, k = 8, m = 1.6, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 1.3, beta = 0.1,
                          seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")


SGFCM_eco_result <- SGFCMeans(dataset_eco_sc, k = 8, m = 1.6, standardize = FALSE,
                              lag_method = "mean",
                              window = w1, alpha = 1.3, beta = 0.1,
                              seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")


SGFCM_soc_result <- SGFCMeans(dataset_soc_sc, k = 8, m = 1.6, standardize = FALSE,
                              lag_method = "mean",
                              window = w1, alpha = 1.3, beta = 0.1,
                              seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")

map_SGFCM_all_result <- rast(SGFCM_all_result$rasters)
map_SGFCM_eco_result <- rast(SGFCM_eco_result$rasters)
map_SGFCM_soc_result <- rast(SGFCM_soc_result$rasters)

plot(map_SGFCM_all_result)
plot(map_SGFCM_eco_result)
plot(map_SGFCM_soc_result)

plot(map_SGFCM_all_result$group1)
plot(map_SGFCM_eco_result$group1)
plot(map_SGFCM_soc_result$group1)

plot(map_SGFCM_all_result$group2)
plot(map_SGFCM_eco_result$group2)
plot(map_SGFCM_soc_result$group2)

plot(map_SGFCM_all_result$group3)
plot(map_SGFCM_eco_result$group3)
plot(map_SGFCM_soc_result$group3)

plot(map_SGFCM_all_result$group4)
plot(map_SGFCM_eco_result$group4)
plot(map_SGFCM_soc_result$group4)


ordered_sgfcm_eco <- groups_matching(SGFCM_all_result, SGFCM_eco_result)
ordered_sgfcm_soc <- groups_matching(SGFCM_all_result, SGFCM_soc_result)
#ordered_sgfcm3 <- groups_matching(SGFCM_eco_result, SGFCM_soc_result)


#----Look at the Belongings----

all_belong <- SGFCM_all_result$Belongings
eco_belong <- SGFCM_eco_result$Belongings
soc_belong <- SGFCM_soc_result$Belongings
