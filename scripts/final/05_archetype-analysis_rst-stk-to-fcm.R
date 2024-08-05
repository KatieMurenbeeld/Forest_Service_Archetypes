library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(raster)

#---Process raster for use with geocmeans----

# load the raster stack created in 03_archetype-analysis_rst-to-fcmstck.R
rast_stack <- rast(here::here("data/processed/rast_stack_full_2024-07-22.tif"))

# Check the layers and update the names (optional) 
names(rast_stack)
names(rast_stack) <- c("pct_forpay_fill", "commcap_fill", "net_mig_fill",
                       "less_hs_fill", "houseburd_fill", "enerburd_fill",
                       "pm25_fill", "mean_forprod_fia", "aip", "demshare",
                       "distance_to_wilderness_m", "distance_to_crithab_m",
                       "WHP", "millcap_5yr", "prec_seas_3000m", "temp_seas_3000m",
                       "roughness", "travel_time_to_cities_20k", "tree_cover",
                       "forest_gain", "num_federal", "fed_area_pct", "stand_age")

# Select Variables for FCmeans
## First, keep all variables/attributes but rename

rst_fcm_all <- rast_stack
writeRaster(rst_fcm_all, here::here(paste0("data/processed/rast_fcm_all_", Sys.Date(), ".tif")), overwrite = TRUE)

## Attributes used in PMRC
rst_fcm_pmrc <- rast_stack[[c("pct_forpay_fill", "commcap_fill", "net_mig_fill",
                              "less_hs_fill", "houseburd_fill", "enerburd_fill",
                              "pm25_fill", "mean_forprod_fia",
                              "distance_to_wilderness_m", "distance_to_crithab_m",
                              "WHP", "millcap_5yr", "prec_seas_3000m", "temp_seas_3000m",
                              "roughness", "travel_time_to_cities_20k", "tree_cover",
                              "forest_gain", "num_federal", "stand_age")]]

writeRaster(rst_fcm_pmrc, here::here(paste0("data/processed/rast_fcm_pmrc2024_", Sys.Date(), ".tif")), overwrite = TRUE)

## Attributes used in PMRC plus a political metric
rst_fcm_pmrc_poli <- rast_stack[[c("pct_forpay_fill", "commcap_fill", "net_mig_fill",
                                   "less_hs_fill", "houseburd_fill", "enerburd_fill",
                                   "pm25_fill", "mean_forprod_fia", "aip", 
                                   "distance_to_wilderness_m", "distance_to_crithab_m",
                                   "WHP", "millcap_5yr", "prec_seas_3000m", "temp_seas_3000m",
                                   "roughness", "travel_time_to_cities_20k", "tree_cover",
                                   "forest_gain", "num_federal","stand_age")]]
writeRaster(rst_fcm_pmrc_poli, here::here(paste0("data/processed/rast_fcm_pmrc2024_ploi_", Sys.Date(), ".tif")), overwrite = TRUE)

## No Federal (GS) with poli
rst_fcm_poli_nogs <- rast_stack[[c("pct_forpay_fill", "commcap_fill", "net_mig_fill",
                                   "less_hs_fill", "houseburd_fill", "enerburd_fill",
                                   "pm25_fill", "mean_forprod_fia", "aip", 
                                   "WHP", "millcap_5yr", "prec_seas_3000m", "temp_seas_3000m",
                                   "roughness", "travel_time_to_cities_20k", "tree_cover",
                                   "forest_gain", "stand_age")]]

writeRaster(rst_fcm_poli_nogs, here::here(paste0("data/processed/rast_fcm_poli_nogs_", Sys.Date(), ".tif")), overwrite = TRUE)

# Scale the data (z-score, mean = 0, std = 1)
rst_fcm_all_zsc <- scale(rst_fcm_all)
rst_fcm_pmrc_zsc <- scale(rst_fcm_pmrc)
rst_fcm_pmrc_poli_zsc <- scale(rst_fcm_pmrc_poli)
rst_fcm_poli_nogs_zsc <- scale(rst_fcm_poli_nogs)

# Scale the data from 0-1
rst_fcm_all_sc <- (rst_fcm_all - global(rst_fcm_all, "min", na.rm=TRUE)[,1])/(global(rst_fcm_all, "max", na.rm=TRUE)[,1] - global(rst_fcm_all, "min", na.rm=TRUE)[,1])
rst_fcm_pmrc_sc <- (rst_fcm_pmrc - global(rst_fcm_pmrc, "min", na.rm=TRUE)[,1])/(global(rst_fcm_pmrc, "max", na.rm=TRUE)[,1] - global(rst_fcm_pmrc, "min", na.rm=TRUE)[,1])
rst_fcm_pmrc_poli_sc <- (rst_fcm_pmrc_poli - global(rst_fcm_pmrc_poli, "min", na.rm=TRUE)[,1])/(global(rst_fcm_pmrc_poli, "max", na.rm=TRUE)[,1] - global(rst_fcm_pmrc_poli, "min", na.rm=TRUE)[,1])
rst_fcm_poli_nogs_sc <- (rst_fcm_poli_nogs - global(rst_fcm_poli_nogs, "min", na.rm=TRUE)[,1])/(global(rst_fcm_poli_nogs, "max", na.rm=TRUE)[,1] - global(rst_fcm_poli_nogs, "min", na.rm=TRUE)[,1])

writeRaster(rst_fcm_pmrc_poli_sc, here::here("data/processed/rst_fcm_pmrc_poli_sc.tif"))

# Investigate the correlation between the attributes
correlation_all <- layerCor(rst_fcm_all, "pearson", na.rm = TRUE)
fcm_cor_all <- as.data.frame(correlation_all$correlation)
write_csv(fcm_cor_all, here::here(paste0("outputs/fcm_all_cor_", Sys.Date(), ".csv")), append = FALSE)

correlation_pmrc <- layerCor(rst_fcm_pmrc, "pearson", na.rm = TRUE)
fcm_cor_pmrc <- as.data.frame(correlation_pmrc$correlation)
write_csv(fcm_cor_pmrc, here::here(paste0("outputs/fcm_pmrc_cor_", Sys.Date(), ".csv")), append = FALSE)

correlation_pmrc_poli <- layerCor(rst_fcm_pmrc_poli, "pearson", na.rm = TRUE)
fcm_cor_pmrc_poli <- as.data.frame(correlation_pmrc_poli$correlation)
write_csv(fcm_cor_pmrc_poli, here::here(paste0("outputs/fcm_pmrc_poli_cor_", Sys.Date(),".csv")), append = FALSE)

correlation_poli_nogs <- layerCor(rst_fcm_poli_nogs, "pearson", na.rm = TRUE)
fcm_cor_poli_nogs <- as.data.frame(correlation_poli_nogs$correlation)
write_csv(fcm_cor_poli_nogs, here::here(paste0("outputs/fcm_poli_nogs_cor_", Sys.Date(), ".csv")), append = FALSE)

# Convert to a simple list of SpatRaster
dataset_all <- lapply(names(rst_fcm_all_sc), function(n){
  aband <- rst_fcm_all_sc[[n]]
  return(aband)
})
names(dataset_all) <- names(rst_fcm_all_sc)

dataset_pmrc <- lapply(names(rst_fcm_pmrc_sc), function(n){
  aband <- rst_fcm_pmrc_sc[[n]]
  return(aband)
})
names(dataset_pmrc) <- names(rst_fcm_pmrc_sc)

dataset_pmrc_poli <- lapply(names(rst_fcm_pmrc_poli_sc), function(n){
  aband <- rst_fcm_pmrc_poli_sc[[n]]
  return(aband)
})
names(dataset_pmrc_poli) <- names(rst_fcm_pmrc_poli_sc)
saveRDS(dataset_pmrc_poli, here::here("data/processed/dataset_pmrc_poli.RDS"))

dataset_poli_nogs <- lapply(names(rst_fcm_poli_nogs_sc), function(n){
  aband <- rst_fcm_poli_nogs_sc[[n]]
  return(aband)
})
names(dataset_poli_nogs) <- names(rst_fcm_poli_nogs_sc)

#---Find appropriate parameter values----
### Code below from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html
# finding an appropriate k and m values
FCMvalues_all <- select_parameters.mc(algo = "FCM", data = dataset_all, 
                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Silhouette.index"),
                                  verbose = TRUE)
write_csv(FCMvalues_all, here::here(paste0("outputs/fcm_all_indices_", Sys.Date(), ".csv")), append = FALSE)

FCMvalues_pmrc <- select_parameters.mc(algo = "FCM", data = dataset_pmrc, 
                                        k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                        indices = c("XieBeni.index", "Explained.inertia",
                                                    "Silhouette.index"),
                                        verbose = TRUE)
write_csv(FCMvalues_pmrc, here::here(paste0("outputs/fcm_pmrc_indices_", Sys.Date(), ".csv")), append = FALSE)

FCMvalues_pmrc_poli <- select_parameters.mc(algo = "FCM", data = dataset_pmrc_poli, 
                                      k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Silhouette.index"),
                                      verbose = TRUE)
write_csv(FCMvalues_pmrc_poli, here::here(paste0("outputs/fcm_pmrc_poli_indices_", Sys.Date(), ".csv")), append = FALSE)

FCMvalues_poli_nogs <- select_parameters.mc(algo = "FCM", data = dataset_poli_nogs, 
                                          k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                          indices = c("XieBeni.index", "Explained.inertia",
                                                      "Silhouette.index"),
                                          verbose = TRUE)
write_csv(FCMvalues_poli_nogs, here::here(paste0("outputs/fcm_poli_nogs_indices_", Sys.Date(), ".csv")), append = FALSE)

## plotting the silhouette index values
## with all variables
sil.idx.all <- ggplot(FCMvalues_all) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx.all
ggsave(here::here("figures/FCM_all_sil_idx.png"), sil.idx.all, 
       width = 12, height = 12, dpi = 300)

## plotting the explained inertia
ex.inert.all <- ggplot(FCMvalues_all) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert.all
ggsave(here::here("figures/FCM_all_ex_inert.png"), ex.inert.all, 
       width = 12, height = 12, dpi = 300)

## PMRC
sil.idx.pmrc <- ggplot(FCMvalues_pmrc) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx.pmrc
ggsave(here::here("figures/FCM_pmrc_sil_idx.png"), sil.idx.pmrc, 
       width = 12, height = 12, dpi = 300)

## plotting the explained inertia
ex.inert.pmrc <- ggplot(FCMvalues_pmrc) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert.pmrc
ggsave(here::here("figures/FCM_pmrc_ex_inert.png"), ex.inert.pmrc, 
       width = 12, height = 12, dpi = 300)

## PMRC with poli
sil.idx.pmrc.poli <- ggplot(FCMvalues_pmrc_poli) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx.pmrc.poli
ggsave(here::here("figures/FCM_pmrc_poli_sil_idx.png"), sil.idx.pmrc.poli, 
       width = 12, height = 12, dpi = 300)

## plotting the explained inertia
ex.inert.pmrc.poli <- ggplot(FCMvalues_pmrc_poli) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert.pmrc.poli
ggsave(here::here("figures/FCM_pmrc_poli_ex_inert.png"), ex.inert.pmrc.poli, 
       width = 12, height = 12, dpi = 300)

## Poli but no GS
sil.idx.poli.nogs <- ggplot(FCMvalues_poli_nogs) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx.poli.nogs
ggsave(here::here("figures/FCM_poli.nogs_sil_idx.png"), sil.idx.poli.nogs, 
       width = 12, height = 12, dpi = 300)

## plotting the explained inertia
ex.inert.poli.nogs <- ggplot(FCMvalues_poli_nogs) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert.poli.nogs
ggsave(here::here("figures/FCM_poli_nogs_ex_inert.png"), ex.inert.poli.nogs, 
       width = 12, height = 12, dpi = 300)


#---Run the FCMs standardize is set to FALSE because the data is scaled------
FCM_result_all <- CMeans(dataset_all, k = 3, m = 1.5, standardize = FALSE, seed = 6891)
saveRDS(FCM_result_all, here::here(paste0("data/processed/FCM_all_", Sys.Date(), ".rds")))
map.res.all <- rast(FCM_result_all$rasters)
plot(map.res.all[["Groups"]])
writeRaster(map.res.all[["Groups"]], filename = paste0("data/processed/FCM_all_", Sys.Date(), ".tif"))

FCM_result_pmrc <- CMeans(dataset_pmrc, k = 8, m = 1.625, standardize = FALSE, seed = 6891)
saveRDS(FCM_result_pmrc, here::here(paste0("data/processed/FCM_pmrc_", Sys.Date(), ".rds")))
map.res.pmrc <- rast(FCM_result_pmrc$rasters)
plot(map.res.pmrc[["Groups"]])
writeRaster(map.res.pmrc[["Groups"]], filename = paste0("data/processed/FCM_pmrc_", Sys.Date(), ".tif"), overwrite = TRUE)

FCM_result_pmrc_poli <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, standardize = FALSE, seed = 6891)
saveRDS(FCM_result_pmrc_poli, here::here(paste0("data/processed/FCM_pmrc_poli_", Sys.Date(), ".rds")))
map.res.pmrc.poli <- rast(FCM_result_pmrc_poli$rasters)
plot(map.res.pmrc.poli[["Groups"]])
writeRaster(map.res.pmrc.poli[["Groups"]], filename = paste0("data/processed/FCM_pmrc_poli_", Sys.Date(), ".tif"))

FCM_result_poli_nogs_01 <- CMeans(dataset_poli_nogs, k = 3, m = 1.625, standardize = FALSE, seed = 6891)
saveRDS(FCM_result_poli_nogs_01, here::here(paste0("data/processed/FCM_poli_nogs_01_", Sys.Date(), ".rds")))
map.res.poli.nogs.01 <- rast(FCM_result_poli_nogs_01$rasters)
plot(map.res.poli.nogs.01[["Groups"]])
writeRaster(map.res.poli.nogs.01[["Groups"]], filename = paste0("data/processed/FCM_poli_nogs_01_", Sys.Date(), ".tif"))

FCM_result_poli_nogs_02 <- CMeans(dataset_poli_nogs, k = 5, m = 1.5, standardize = FALSE, seed = 6891)
saveRDS(FCM_result_poli_nogs_02, here::here(paste0("data/processed/FCM_poli_nogs_02_", Sys.Date(), ".rds")))
map.res.poli.nogs.02 <- rast(FCM_result_poli_nogs_02$rasters)
plot(map.res.poli.nogs.02[["Groups"]])
writeRaster(map.res.poli.nogs.02[["Groups"]], filename = paste0("data/processed/FCM_poli_nogs_02_", Sys.Date(), ".tif"))

##----Run a KNN (no fuzzifier) cluster analysis----
set.seed(1986)
km_pmrc_poli <- k_means(rst_fcm_pmrc_poli_sc, centers=8, iter.max = 100, algorithm = "Lloyd")
plot(km_pmrc_poli)

##----Test with noise cluster = TRUE----
FCM_result_pmrc_poli_noise_del1 <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, 
                                           standardize = FALSE, seed = 6891, 
                                           noise_cluster = TRUE, delta = 1)
saveRDS(FCM_result_pmrc_poli_noise_del1, here::here(paste0("data/processed/FCM_pmrc_poli_noise_del1", Sys.Date(), ".rds")))
map.res.pmrc.poli.noise1 <- rast(FCM_result_pmrc_poli_noise_del1$rasters)
plot(map.res.pmrc.poli.noise1[["Groups"]])

FCM_result_pmrc_poli_noise_del5 <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, 
                                          standardize = FALSE, seed = 6891, 
                                          noise_cluster = TRUE, delta = 5)
saveRDS(FCM_result_pmrc_poli_noise_del5, here::here(paste0("data/processed/FCM_pmrc_poli_noise_del5", Sys.Date(), ".rds")))
map.res.pmrc.poli.noise5 <- rast(FCM_result_pmrc_poli_noise_del5$rasters)
plot(map.res.pmrc.poli.noise5[["Groups"]])

FCM_result_pmrc_poli_noise_del05 <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, 
                                          standardize = FALSE, seed = 6891, 
                                          noise_cluster = TRUE, delta = 0.5)
saveRDS(FCM_result_pmrc_poli_noise_del05, here::here(paste0("data/processed/FCM_pmrc_poli_noise_del05", Sys.Date(), ".rds")))
map.res.pmrc.poli.noise05 <- rast(FCM_result_pmrc_poli_noise_del05$rasters)
plot(map.res.pmrc.poli.noise05[["Groups"]])

##---save the iteration, k, m as a dataframe----
aa_iteration <- data.frame(iteration_name = character(),
                           attris = character(),
                           k = numeric(),
                           m = numeric(), 
                           sil_idx = numeric())

iteration_name <- "FCM_eco"
attris <- paste(names(rst_fcm_eco), collapse= ", ")
k <- 2
m <- 1.125
sil_idx <- 0.58

aa_iteration[nrow(aa_iteration) + 1,] <- list(iteration_name, attris, k, m, sil_idx)

write_csv(aa_iteration, here::here("outputs/aa_iteration_2024-06-20.csv"), append = TRUE)


##----Check the probability of belonging for each cluster----
### using different undecided thresholds
maps1 <- mapClusters(object = FCM_result_pmrc_poli, undecided = 0.25)

# plotting membership values each cluster
maps1$ClusterPlot + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Set2")

maps1$ProbaMaps[[1]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[2]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[3]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[4]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[5]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[6]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[7]] + theme(legend.position = "bottom")

maps1$ProbaMaps[[8]] + theme(legend.position = "bottom")

GFCMvalues <- select_parameters.mc(algo = "GFCM", data = dataset_pmrc_poli,
                                   k = 8, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                   spconsist = FALSE, verbose = TRUE, init = "kpp",
                                   indices = c("XieBeni.index", "Explained.inertia",
                                               "Negentropy.index", "Silhouette.index"))

ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = beta, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

GFCM_result <- GCMeans(dataset_pmrc_poli, k = 8, m = 1.625, beta = 0.125, standardize = FALSE,
                       verbose = FALSE, seed = 6891, tol = 0.001)
  
maps2 <- mapClusters(object = GFCM_result, undecided = 0.3)
maps2$ClusterPlot + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Set3")


test_dataset <- readRDS(here::here("data/processed/dataset_pmrc_poli.RDS"))

