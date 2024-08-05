library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(raster)

# Load the data
rst_fcm_pmrc_poli_sc <- rast(here::here("data/processed/rst_fcm_pmrc_poli_sc.tif"))

# Format for use in geocmeans
dataset_pmrc_poli <- lapply(names(rst_fcm_pmrc_poli_sc), function(n){
  aband <- rst_fcm_pmrc_poli_sc[[n]]
  return(aband)
})
names(dataset_pmrc_poli) <- names(rst_fcm_pmrc_poli_sc)

##----Test with noise cluster = TRUE----
GFCM_result_noise_del05 <- GCMeans(dataset_pmrc_poli, k = 8, m = 1.625, beta = 0.125, 
                       standardize = FALSE, verbose = FALSE, seed = 6891, tol = 0.001,
                       noise_cluster = TRUE, delta = 0.5)
#saveRDS(GFCM_result_noise_del05, here::here(paste0("data/processed/GFCM_result_noise_del05", Sys.Date(), ".rds")))
map_gfcm <- rast(GFCM_result_noise_del05$rasters)
plot(map_gfcm[["Groups"]])

GFCM_result_noise_del1 <- GCMeans(dataset_pmrc_poli, k = 8, m = 1.625, beta = 0.125, 
                                  standardize = FALSE, verbose = FALSE, seed = 6891, tol = 0.001,
                                  noise_cluster = TRUE, delta = 1)
#saveRDS(GFCM_result_noise_del1, here::here(paste0("data/processed/GFCM_result_noise_del1", Sys.Date(), ".rds")))
map_gfcm1 <- rast(GFCM_result_noise_del1$rasters)
plot(map_gfcm1[["Groups"]])

GFCM_result_noise_del2 <- GCMeans(dataset_pmrc_poli, k = 8, m = 1.625, beta = 0.125, 
                                  standardize = FALSE, verbose = FALSE, seed = 6891, tol = 0.001,
                                  noise_cluster = TRUE, delta = 2)
#saveRDS(GFCM_result_noise_del2, here::here(paste0("data/processed/GFCM_result_noise_del2", Sys.Date(), ".rds")))
map_gfcm2 <- rast(GFCM_result_noise_del2$rasters)
plot(map_gfcm2[["Groups"]])



