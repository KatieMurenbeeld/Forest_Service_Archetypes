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

# Create the social raster stack
soc_rast_sc <- raster::subset(rst_fcm_pmrc_poli_sc, c("pct_forpay_fill", "commcap_fill", 
                                                      "net_mig_fill", "less_hs_fill",
                                                      "houseburd_fill", "enerburd_fill", 
                                                      "pm25_fill", "millcap_5yr", 
                                                      "travel_time_to_cities_20k",
                                                      "num_federal"))
# Format for use in geocmeans
dataset_soc_sc <- lapply(names(soc_rast_sc), function(n){
  aband <- soc_rast_sc[[n]]
  return(aband)
})
names(dataset_soc_sc) <- names(soc_rast_sc)

# Create a dataframe for ease of mapping and looking at results
df_all <- as.data.frame(rst_fcm_pmrc_poli, xy = TRUE, na.rm = TRUE)
df_all_sc <- as.data.frame(rst_fcm_pmrc_poli_sc, xy = TRUE, na.rm = TRUE)
df_all_zsc <- as.data.frame(scale(rst_fcm_pmrc_poli), xy = TRUE, na.rm = TRUE)

# Format for use with Kmeans
df <- as.data.frame(soc_rast_sc, xy=TRUE)
df_nogeo <- df %>%
  dplyr::select(-c("x", "y")) %>%
  drop_na()

# finding the best k by using the r2 of the classification
# trying for k from 2 to 10
R2s <- sapply(2:10,function(k){
  Clust <- kmeans(df_nogeo, 
                  centers = k,
                  iter.max = 150)
  R2 <- Clust$betweenss / Clust$totss
  return(R2)
})

kmeans_df <- data.frame(K=2:10,
                        R2 = R2s)

# create an elbow plot of R2s and K
ggplot(kmeans_df)+
  geom_line(aes(x=K,y=R2s))+
  geom_point(aes(x=K,y=R2s),color="red")+
  xlab("Number of groups")+
  ylab("R2 of classification")

# no real elbow shows up, maybe 4?

# Use FCM to select the k and m values.
future::plan(future::multisession(workers = 2))
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset_soc_sc, 
                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 6891, verbose = TRUE)
#write_csv(FCMvalues, here::here(paste0("outputs/fcm_soc-attri_params_", Sys.Date(), ".csv")), append = FALSE)
FCMvalues <- read_csv(here::here("outputs/fcm_soc-attri_params_2024-09-03.csv"))
knitr::kable(FCMvalues[c("k","m","Silhouette.index","XieBeni.index")],
             col.names = c("k","m","silhouette index",
                           "Xie and Beni index"),digits = 3)

# plotting the silhouette  and Xie Beni index
ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = m, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = XieBeni.index)) + 
  geom_text(aes(x = k, y = m, label = round(XieBeni.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# I think k = 3 and m = 1.8 or k = 6 and m = 2.0
## Based on the k and m values, select the value for beta using GFCM
GFCMvalues <- select_parameters.mc(algo = "GFCM", data = dataset_soc_sc,
                                   k = 3, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                   spconsist = FALSE, verbose = TRUE, init = "kpp",
                                   indices = c("XieBeni.index", "Explained.inertia",
                                               "Negentropy.index", "Silhouette.index")) 
#write_csv(GFCMvalues, here::here(paste0("outputs/gfcm_soc-attri_params_k3_", Sys.Date(), ".csv")), append = FALSE)
write_csv(GFCMvalues, here::here(paste0("outputs/gfcm_soc-attri_params_k6_", Sys.Date(), ".csv")), append = FALSE)

# plotting the silhouette index
ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = m, y = beta, label = round(XieBeni.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# when k = 3 beta = 0.4 and m = 1.3, without beta k = 3 and m = 1.8
## Then select a value for alpha or both alpha and beta using SFCM and SGFCM
# Spatial FCM
w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

SFCMvalues <- select_parameters.mc(algo = "SFCM", data = dataset_soc_sc, 
                                   k = 3, m = 1.8,
                                   alpha = seq(0.5,2,0.1),
                                   window = list(w1,w2,w3),
                                   spconsist = TRUE, nrep = 5, 
                                   verbose = TRUE, chunk_size = 4,
                                   seed = 6891, init = "kpp",
                                   indices = c("XieBeni.index", "Explained.inertia",
                                               "Negentropy.index", "Silhouette.index"))
dict <- data.frame(
  w = c(1,2,3),
  window = c("3x3","5x5","7x7")
)

SFCMvalues$window <- dict$window[match(SFCMvalues$window,dict$w)]
#write_csv(SFCMvalues, here::here(paste0("outputs/sfcm_soc-attri_params_m13_", Sys.Date(), ".csv")), append = FALSE)
write_csv(SFCMvalues, here::here(paste0("outputs/sfcm_soc-attri_params_m18_", Sys.Date(), ".csv")), append = FALSE)

# plot the silhouette index
ggplot(SFCMvalues) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2.0)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# window = 3x3 and alpha = 1.0 look the best for k = 3 and m = 1.3
# silhouette values seemed higher with k = 3 and m = 1.8 window = 3x3 alpha = 0.5

# Spatial Generalized FCM 
# beta only
SGFCMbeta <- select_parameters.mc(algo = "SGFCM", data = dataset_soc_sc, 
                                   k = 3, m = 1.8,
                                   beta = seq(0.1,0.9,0.1), alpha = 0.5,
                                   window = list(w1,w2,w3),
                                   spconsist = TRUE, nrep = 5, 
                                   verbose = TRUE, chunk_size = 4,
                                   seed = 6891, init = "kpp",
                                   indices = c("XieBeni.index", "Explained.inertia",
                                               "Negentropy.index", "Silhouette.index"))
dict <- data.frame(
  w = c(1,2,3),
  window = c("3x3","5x5","7x7")
)

SGFCMbeta$window <- dict$window[match(SGFCMbeta$window,dict$w)]
# v1: k = 3, m = 1.3, alpha = 1.0  
# v2: k = 3, m = 1.8, alpha = 0.5
#write_csv(SGFCMbeta, here::here(paste0("outputs/sgfcm_soc-attri_params-beta_v1_", Sys.Date(), ".csv")), append = FALSE)
write_csv(SGFCMbeta, here::here(paste0("outputs/sgfcm_soc-attri_params-beta_v2_", Sys.Date(), ".csv")), append = FALSE)

# for v1: k = 3, m = 1.3, alpha = 1.0, window = 5x5, and beta = 0.4
# for v2: k = 3, m = 1.8, alpha = 0.5, window = 3x3, and beta = 0.1
# plot the silhouette and xie beni index
ggplot(SGFCMbeta) + 
  geom_raster(aes(x = beta, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = beta, y = window, label = round(Silhouette.index,2)), size = 2.0)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

ggplot(SGFCMbeta) + 
  geom_raster(aes(x = beta, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = beta, y = window, label = round(XieBeni.index,2)), size = 2.0)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# v1: k = 3, m = 1.3, window = 5x5
# v2: k = 3, m = 1.8, window = 3x3
# alpha and beta
future::plan(future::multisession(workers=2))
SGFCMalphabeta <- select_parameters.mc(algo = "SGFCM", data = dataset_soc_sc,
                                       k = 3, m = 1.8, 
                                       beta = seq(0,1.0,0.1), alpha = seq(0,2,0.1),
                                       window = w1, spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 6891, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))

dict <- data.frame(
  w = c(1,2,3),
  window = c("3x3","5x5","7x7")
)

SGFCMalphabeta$window <- dict$window[match(SGFCMalphabeta$window,dict$w)]
#write_csv(SGFCMalphabeta, here::here(paste0("outputs/sgfcm_soc-attri_params-alphabeta_v1_", Sys.Date(), ".csv")), append = FALSE)
write_csv(SGFCMalphabeta, here::here(paste0("outputs/sgfcm_soc-attri_params-alphabeta_v2_", Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
ggplot(SGFCMalphabeta) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.0)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

ggplot(SGFCMalphabeta) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(XieBeni.index,2)), size = 2.0)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.5)

## Based on the parameter optimization run the final SGFCM for the soc attributes
### k = 3, m = 1.3, window = 5x5, alpha = 0.5, beta = 0.3 sil.idx = 0.30
### k = 3, m = 1.8, window = 3x3, alpha = 0.1, beta = 0.8, sil.idx = 0.31
SGFCM_result <- SGFCMeans(dataset_soc_sc, k = 3, m = 1.8, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 0.1, beta = 0.8,
                          seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")
#saveRDS(SGFCM_result, here::here(paste0("data/processed/SGFCM_result_soc_", Sys.Date(), ".rds")))

map_SGFCM_result <- rast(SGFCM_result$rasters)
plot(map_SGFCM_result[["Groups"]])
writeRaster(map_SGFCM_result[["Groups"]], filename = paste0("data/processed/SGFCM_result_soc_", Sys.Date(), ".tif"))

maps_sgfcm <- mapClusters(object = SGFCM_result, undecided = 0.2)

# Fuzzy ELSA
fuzzy_elsa_rast <- calcFuzzyELSA(SGFCM_result, window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 7, "Blues")
vals <- terra::values(fuzzy_elsa_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_rast, col = cols, breaks = limits$brks)

#writeRaster(fuzzy_elsa_rast, here::here(paste0("data/processed/SGFCM_soc_felsa_", 
#                                               Sys.Date(), ".tif")))
fuzzy_elsa_soc <- rast(here::here("data/processed/SGFCM_soc_felsa_2024-09-04.tif"))

# Map with the USFS boundaries
# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(rst_fcm_pmrc_poli_sc))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(rst_fcm_pmrc_poli_sc))

#plot(fuzzy_elsa_rast, col = cols, breaks = limits$brks)
#plot(fs_nf.crop$geometry, add = TRUE)



