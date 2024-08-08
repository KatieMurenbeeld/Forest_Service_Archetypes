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

#---Run the FCMs standardize is set to FALSE because the data is scaled------
FCM_result_pmrc_poli <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, standardize = FALSE, seed = 6891)
saveRDS(FCM_result_pmrc_poli, here::here(paste0("data/processed/FCM_pmrc_poli_sc_", Sys.Date(), ".rds")))
map.res.pmrc.poli <- rast(FCM_result_pmrc_poli$rasters)
plot(map.res.pmrc.poli[["Groups"]])
writeRaster(map.res.pmrc.poli[["Groups"]], filename = paste0("data/processed/FCM_pmrc_poli_sc_", Sys.Date(), ".tif"))

##----Check the probability of belonging for each cluster----
### using different undecided thresholds
maps1 <- mapClusters(object = FCM_result_pmrc_poli, undecided = 0.45)
maps1$ClusterPlot + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Set3")

maps1$ProbaMaps[[1]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[2]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[3]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[4]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[5]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[6]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[7]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[8]] + theme(legend.position = "bottom")
