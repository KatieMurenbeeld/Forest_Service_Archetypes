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
FCM_result_pmrc_poli_noise_del1 <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, 
                                          standardize = FALSE, seed = 6891, 
                                          noise_cluster = TRUE, delta = 1)
#saveRDS(FCM_result_pmrc_poli_noise_del1, here::here(paste0("data/processed/FCM_pmrc_poli_noise_del1", Sys.Date(), ".rds")))
map.res.pmrc.poli.noise1 <- rast(FCM_result_pmrc_poli_noise_del1$rasters)
plot(map.res.pmrc.poli.noise1[["Groups"]])

FCM_result_pmrc_poli_noise_del5 <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, 
                                          standardize = FALSE, seed = 6891, 
                                          noise_cluster = TRUE, delta = 5)
#saveRDS(FCM_result_pmrc_poli_noise_del5, here::here(paste0("data/processed/FCM_pmrc_poli_noise_del5", Sys.Date(), ".rds")))
map.res.pmrc.poli.noise5 <- rast(FCM_result_pmrc_poli_noise_del5$rasters)
plot(map.res.pmrc.poli.noise5[["Groups"]])

FCM_result_pmrc_poli_noise_del05 <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, 
                                           standardize = FALSE, seed = 6891, 
                                           noise_cluster = TRUE, delta = 0.5)
#saveRDS(FCM_result_pmrc_poli_noise_del05, here::here(paste0("data/processed/FCM_pmrc_poli_noise_del05", Sys.Date(), ".rds")))
map.res.pmrc.poli.noise05 <- rast(FCM_result_pmrc_poli_noise_del05$rasters)
plot(map.res.pmrc.poli.noise05[["Groups"]])