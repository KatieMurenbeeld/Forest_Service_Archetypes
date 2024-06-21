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
rast_stack <- rast(here::here("data/processed/rast_stack_noprivfor_2024-06-20.tif"))

# Update the names (optional) 
names(rast_stack)

# Select Variables for FCmeans
## First, keep all variables/attributes but rename

rst_fcm_all <- rast_stack
names(rst_fcm_all) <- c("pct_pay_focal_mean", "COMMCAP_focal_mean", "R_NET_M_focal_mean",
                    "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean",                 
                    "PM25F_PFS_focal_mean", "distance_to_wilderness_m_focal_mean",
                    "distance_to_crithab_m_focal_mean", "WHP_focal_mean", "millcap_5yr.pred_focal_mean",
                    "prec_seas_3000m_focal_mean", "temp_seas_3000m_focal_mean", "roughness_focal_mean",
                    "travel_time_to_cities_focal_mean", "tree_cov_focal_mean", "numfed_focal_mean", 
                    "pct_area_fed_focal_mean", "forestgain_focal_mean", "stand_age_focal_mean", "for_prod_focal_mean")

writeRaster(rst_fcm_all, here::here(paste0("data/processed/rast_fcm_all_", Sys.Date(), ".tif")), overwrite = TRUE)

## Second, only RU, RS, and U(A) attributes 
rst_fcm_ru_rs_u <- rast_stack[[c("pct_pay_focal_mean", "COMMCAP_focal_mean",
                                 "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean",                 
                                 "travel_time_to_cities_1_focal_mean", "Layer_1_focal_mean",
                                 "forestgain_focal_mean", "stand_age_focal_mean", "forest_prod_focal_mean")]]

names(rst_fcm_ru_rs_u) <- c("pct_pay_focal_mean", "COMMCAP_focal_mean",
                        "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean",                 
                        "travel_time_to_cities_focal_mean", "tree_cov_focal_mean",
                        "forestgain_focal_mean", "stand_age_focal_mean", "for_prod_focal_mean")

writeRaster(rst_fcm_ru_rs_u, here::here(paste0("data/processed/rast_fcm_ru_rs_u", Sys.Date(), ".tif")), overwrite = TRUE)

## Third, only for S and ECO, no change to names
rst_fcm_s_eco <- rast_stack[[c("R_NET_M_focal_mean", "PM25F_PFS_focal_mean",
                               "WHP_focal_mean", "millcap_5yr.pred_focal_mean",
                               "prec_seas_3000m_focal_mean", "temp_seas_3000m_focal_mean",
                               "roughness_focal_mean", "numfed_focal_mean")]]

writeRaster(rst_fcm_s_eco, here::here(paste0("data/processed/rast_fcm_s_eco", Sys.Date(), ".tif")), overwrite = TRUE)

## Fourth, only for GS
rst_fcm_gs <- rast_stack[[c("percent_area_focal_mean", 
                            "distance_to_wilderness_m_focal_mean",
                            "distance_to_crithab_m_focal_mean")]]
names(rst_fcm_gs) <- c("percent_area_fed_focal_mean", 
                       "dist_to_wilderness_m_focal_mean",
                       "dist_to_crithab_m_focal_mean")

writeRaster(rst_fcm_gs, here::here(paste0("data/processed/rast_fcm_gs", Sys.Date(), ".tif")), overwrite = TRUE)

# Fifth, all but gs
rst_fcm_no_gs <- rast_stack[[c("pct_pay_focal_mean", "COMMCAP_focal_mean", "R_NET_M_focal_mean",
                               "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean", 
                               "PM25F_PFS_focal_mean", "distance_to_wilderness_m_focal_mean",
                               "distance_to_crithab_m_focal_mean", "WHP_focal_mean",
                               "millcap_5yr.pred_focal_mean", "prec_seas_3000m_focal_mean", 
                               "temp_seas_3000m_focal_mean", "roughness_focal_mean", 
                               "travel_time_to_cities_1_focal_mean", "Layer_1_focal_mean",
                               "numfed_focal_mean", "forestgain_focal_mean", "stand_age_focal_mean",
                               "forest_prod_focal_mean")]]

names(rst_fcm_no_gs) <- c("pct_pay_focal_mean", "COMMCAP_focal_mean", "R_NET_M_focal_mean",
                          "HSEF_focal_mean", "HBF_PFS_focal_mean", "EBF_PFS_focal_mean", 
                          "PM25F_PFS_focal_mean", "dist_to_wilderness_m_focal_mean",
                          "dist_to_crithab_m_focal_mean", "WHP_focal_mean",
                          "millcap_5yr.pred_focal_mean", "prec_seas_3000m_focal_mean", 
                          "temp_seas_3000m_focal_mean", "roughness_focal_mean", 
                          "travel_time_to_cities_1_focal_mean", "tree_cover_focal_mean",
                          "numfed_focal_mean", "forestgain_focal_mean", "stand_age_focal_mean",
                          "forest_prod_focal_mean")

writeRaster(rst_fcm_no_gs, here::here(paste0("data/processed/rast_fcm_no_gs_", Sys.Date(), ".tif")), overwrite = TRUE)

# Sixth, only for ECO 
rst_fcm_eco <- rast_stack[[c("PM25F_PFS_focal_mean",
                               "WHP_focal_mean",
                               "prec_seas_3000m_focal_mean", "temp_seas_3000m_focal_mean",
                               "roughness_focal_mean")]]

writeRaster(rst_fcm_eco, here::here(paste0("data/processed/rast_fcm_eco", Sys.Date(), ".tif")), overwrite = TRUE)

# Scale the data
rst_fcm_all_sc <- scale(rst_fcm_all)
rst_fcm_ru_rs_u_sc <- scale(rst_fcm_ru_rs_u)
rst_fcm_s_eco_sc <- scale(rst_fcm_s_eco)
rst_fcm_gs_sc <- scale(rst_fcm_gs)
rst_fcm_no_gs_sc <- scale(rst_fcm_no_gs)
rst_fcm_eco_sc <- scale(rst_fcm_eco)

# Investigate the correlation between the attributes
correlation_all <- layerCor(rst_fcm_all, "pearson", na.rm = TRUE)
fcm_cor_all <- as.data.frame(correlation_all$correlation)
write_csv(fcm_cor_all, here::here("outputs/fcm_all_cor.csv"), append = FALSE)

correlation_ru_rs_u <- layerCor(rst_fcm_ru_rs_u, "pearson", na.rm = TRUE)
fcm_cor_ru_rs_u <- as.data.frame(correlation_ru_rs_u$correlation)
write_csv(fcm_cor_ru_rs_u, here::here("outputs/fcm_ru_rs_u_cor.csv"), append = FALSE)

correlation_s_eco <- layerCor(rst_fcm_s_eco, "pearson", na.rm = TRUE)
fcm_cor_s_eco <- as.data.frame(correlation_s_eco$correlation)
write_csv(fcm_cor_s_eco, here::here("outputs/fcm_s_eco_cor.csv"), append = FALSE)

correlation_gs <- layerCor(rst_fcm_gs, "pearson", na.rm = TRUE)
fcm_cor_gs <- as.data.frame(correlation_gs$correlation)
write_csv(fcm_cor_gs, here::here("outputs/fcm_gs_cor.csv"), append = FALSE)

correlation_no_gs <- layerCor(rst_fcm_no_gs, "pearson", na.rm = TRUE)
fcm_cor_no_gs <- as.data.frame(correlation_no_gs$correlation)
write_csv(fcm_cor_no_gs, here::here("outputs/fcm_no_gs_cor.csv"), append = FALSE)

correlation_eco <- layerCor(rst_fcm_eco, "pearson", na.rm = TRUE)
fcm_cor_eco <- as.data.frame(correlation_eco$correlation)
write_csv(fcm_cor_eco, here::here("outputs/fcm_eco_cor.csv"), append = FALSE)

# Convert to a simple list of SpatRaster
dataset_all <- lapply(names(rst_fcm_all_sc), function(n){
  aband <- rst_fcm_all_sc[[n]]
  return(aband)
})
names(dataset_all) <- names(rst_fcm_all_sc)

dataset_ru_rs_u <- lapply(names(rst_fcm_ru_rs_u_sc), function(n){
  aband <- rst_fcm_ru_rs_u_sc[[n]]
  return(aband)
})
names(dataset_ru_rs_u) <- names(rst_fcm_ru_rs_u_sc)

dataset_s_eco <- lapply(names(rst_fcm_s_eco_sc), function(n){
  aband <- rst_fcm_s_eco_sc[[n]]
  return(aband)
})
names(dataset_s_eco) <- names(rst_fcm_s_eco_sc)

dataset_gs <- lapply(names(rst_fcm_gs_sc), function(n){
  aband <- rst_fcm_gs_sc[[n]]
  return(aband)
})
names(dataset_gs) <- names(rst_fcm_gs_sc)

dataset_no_gs <- lapply(names(rst_fcm_no_gs_sc), function(n){
  aband <- rst_fcm_no_gs_sc[[n]]
  return(aband)
})
names(dataset_no_gs) <- names(rst_fcm_no_gs_sc)

dataset_eco <- lapply(names(rst_fcm_eco_sc), function(n){
  aband <- rst_fcm_eco_sc[[n]]
  return(aband)
})
names(dataset_eco) <- names(rst_fcm_eco_sc)

#---Find appropriate parameter values----
### Code below from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html
# finding an appropriate k and m values
FCMvalues_all <- select_parameters.mc(algo = "FCM", data = dataset_all, 
                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Silhouette.index"),
                                  verbose = TRUE)

FCMvalues_ru_rs_u <- select_parameters.mc(algo = "FCM", data = dataset_ru_rs_u, 
                                      k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Silhouette.index"),
                                      verbose = TRUE)


FCMvalues_s_eco <- select_parameters.mc(algo = "FCM", data = dataset_s_eco, 
                                          k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                          indices = c("XieBeni.index", "Explained.inertia",
                                                      "Silhouette.index"),
                                          verbose = TRUE)

FCMvalues_gs <- select_parameters.mc(algo = "FCM", data = dataset_gs, 
                                        k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                        indices = c("XieBeni.index", "Explained.inertia",
                                                    "Silhouette.index"),
                                        verbose = TRUE)

FCMvalues_no_gs <- select_parameters.mc(algo = "FCM", data = dataset_no_gs, 
                                     k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                     indices = c("XieBeni.index", "Explained.inertia",
                                                 "Silhouette.index"),
                                     verbose = TRUE)

FCMvalues_eco <- select_parameters.mc(algo = "FCM", data = dataset_eco, 
                                        k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                        indices = c("XieBeni.index", "Explained.inertia",
                                                    "Silhouette.index"),
                                        verbose = TRUE)

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

## with RU RS and U(A) variables
sil.idx.ru.rs.u <- ggplot(FCMvalues_ru_rs_u) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx.ru.rs.u
ggsave(here::here("figures/FCM_ru_rs_u_sil_idx.png"), sil.idx.ru.rs.u, 
       width = 12, height = 12, dpi = 300)

## plotting the explained inertia
ex.inert.ru.rs.u <- ggplot(FCMvalues_ru_rs_u) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert.ru.rs.u
ggsave(here::here("figures/FCM_ru_rs_u_ex_inert.png"), ex.inert.ru.rs.u, 
       width = 12, height = 12, dpi = 300)

## with S and ECO variables
sil.idx.s.eco <- ggplot(FCMvalues_s_eco) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx.s.eco
ggsave(here::here("figures/FCM_s_eco_sil_idx.png"), sil.idx, 
       width = 12, height = 12, dpi = 300)

## plotting the explained inertia
ex.inert.s.eco <- ggplot(FCMvalues_s_eco) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert.s.eco
ggsave(here::here("figures/FCM_s_eco_ex_inert.png"), ex.inert, 
       width = 12, height = 12, dpi = 300)

## with GS variables
sil.idx.gs <- ggplot(FCMvalues_gs) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx.gs
ggsave(here::here("figures/FCM_gs_sil_idx.png"), sil.idx, 
       width = 12, height = 12, dpi = 300)

## plotting the explained inertia
ex.inert.gs <- ggplot(FCMvalues_gs) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert.gs
ggsave(here::here("figures/FCM_gs_ex_inert.png"), ex.inert, 
       width = 12, height = 12, dpi = 300)

## all but GS (really just all but precent forest area covered by feds)
sil.idx.no.gs <- ggplot(FCMvalues_no_gs) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx.no.gs
ggsave(here::here("figures/FCM_no_gs_sil_idx.png"), sil.idx.no.gs, 
       width = 12, height = 12, dpi = 300)

## plotting the explained inertia
ex.inert.no.gs <- ggplot(FCMvalues_no_gs) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert.no.gs
ggsave(here::here("figures/FCM_no_gs_ex_inert.png"), ex.inert.no.gs, 
       width = 12, height = 12, dpi = 300)

## with ECO variables
sil.idx.eco <- ggplot(FCMvalues_eco) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx.eco
ggsave(here::here("figures/FCM_eco_sil_idx.png"), sil.idx.eco, 
       width = 12, height = 12, dpi = 300)

## plotting the explained inertia
ex.inert.eco <- ggplot(FCMvalues_eco) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert.eco
ggsave(here::here("figures/FCM_eco_ex_inert.png"), ex.inert.eco, 
       width = 12, height = 12, dpi = 300)


#---Run the FCMs standardize is set to FALSE because the data is scaled------
FCM_result_all <- CMeans(dataset_all, k = 5, m = 1.875, standardize = FALSE)
saveRDS(FCM_result_all, here::here(paste0("data/processed/FCM_all_", Sys.Date(), ".rds")))
map.res.all <- rast(FCM_result_all$rasters)
plot(map.res.all[["Groups"]])
writeRaster(map.res.all[["Groups"]], filename = paste0("data/processed/FCM_all_", Sys.Date(), ".tif"))

FCM_result_ru_rs_u_01 <- CMeans(dataset_ru_rs_u, k = 7, m = 1.625, standardize = FALSE)
saveRDS(FCM_result_ru_rs_u_01, here::here(paste0("data/processed/FCM_ru_rs_u_01_", Sys.Date(), ".rds")))
map.res.ru.rs.u.01 <- rast(FCM_result_ru_rs_u_01$rasters)
plot(map.res.ru.rs.u.01[["Groups"]])
writeRaster(map.res.ru.rs.u.01[["Groups"]], filename = paste0("data/processed/FCM_ru_rs_u_01_", Sys.Date(), ".tif"))

FCM_result_ru_rs_u_02 <- CMeans(dataset_ru_rs_u, k = 2, m = 1.875, standardize = FALSE)
saveRDS(FCM_result_ru_rs_u_02, here::here(paste0("data/processed/FCM_ru_rs_u_02_", Sys.Date(), ".rds")))
map.res.ru.rs.u.02 <- rast(FCM_result_ru_rs_u_02$rasters)
plot(map.res.ru.rs.u.02[["Groups"]])
writeRaster(map.res.ru.rs.u.02[["Groups"]], filename = paste0("data/processed/FCM_ru_rs_u_02_", Sys.Date(), ".tif"))

FCM_result_s_eco <- CMeans(dataset_s_eco, k = 5, m = 1.7, standardize = FALSE)
saveRDS(FCM_result_s_eco, here::here(paste0("data/process/FCM_s_eco_", Sys.Date(), ".rds")))
map.res.s.eco <- rast(FCM_result_s_eco$rasters)
writeRaster(map.res.s.eco[["Groups"]], filename = paste0("data/processed/FCM_s_eco_", Sys.Date(), ".tif"))

FCM_result_gs <- CMeans(dataset_gs, k = 5, m = 1.7, standardize = FALSE)
saveRDS(FCM_result_gs, here::here(paste0("data/process/FCM_gs_", Sys.Date(), ".rds")))
map.res.gs <- rast(FCM_result_gs$rasters)
writeRaster(map.res.gs[["Groups"]], filename = paste0("data/processed/FCM_gs_", Sys.Date(), ".tif"))

FCM_result_no_gs <- CMeans(dataset_no_gs, k = 5, m = 1.875, standardize = FALSE)
saveRDS(FCM_result_no_gs, here::here(paste0("data/processed/FCM_no_gs_", Sys.Date(), ".rds")))
map.res.no.gs <- rast(FCM_result_no_gs$rasters)
plot(map.res.no.gs[["Groups"]])
writeRaster(map.res.no.gs[["Groups"]], filename = paste0("data/processed/FCM_no_gs_", Sys.Date(), ".tif"))

FCM_result_eco <- CMeans(dataset_eco, k = 2, m = 1.125, standardize = FALSE)
saveRDS(FCM_result_eco, here::here(paste0("data/processed/FCM_eco_", Sys.Date(), ".rds")))
map.res.eco <- rast(FCM_result_eco$rasters)
plot(map.res.eco[["Groups"]])
writeRaster(map.res.eco[["Groups"]], filename = paste0("data/processed/FCM_eco_", Sys.Date(), ".tif"))

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
