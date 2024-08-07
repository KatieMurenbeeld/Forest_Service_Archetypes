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