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

# Create the ecological raster stack
eco_rast_sc <- raster::subset(rst_fcm_pmrc_poli_sc, c("mean_forprod_fia", "WHP",
                                                      "prec_seas_3000m", "temp_seas_3000m",
                                                      "roughness", "tree_cover", 
                                                      "forest_gain", "stand_age"))

# Format for use in geocmeans
dataset_eco_sc <- lapply(names(eco_rast_sc), function(n){
  aband <- eco_rast_sc[[n]]
  return(aband)
})
names(dataset_eco_sc) <- names(eco_rast_sc)

# Create a dataframe for ease of mapping and looking at results
df_all <- as.data.frame(rst_fcm_pmrc_poli, xy = TRUE, na.rm = TRUE)
df_all_sc <- as.data.frame(rst_fcm_pmrc_poli_sc, xy = TRUE, na.rm = TRUE)
df_all_zsc <- as.data.frame(scale(rst_fcm_pmrc_poli), xy = TRUE, na.rm = TRUE)

# Format for use with Kmeans
df <- as.data.frame(eco_rast_sc, xy=TRUE)
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

# possible elbows at k = 3 and k = 6

# Use FCM to select the k and m values.
future::plan(future::multisession(workers = 2))
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset_eco_sc, 
                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  seed = 6891, verbose = TRUE)
write_csv(FCMvalues, here::here(paste0("outputs/fcm_eco-attri_params_", Sys.Date(), ".csv")), append = FALSE)

knitr::kable(FCMvalues[c("k","m","Silhouette.index","XieBeni.index")],
             col.names = c("k","m","silhouette index",
                           "Xie and Beni index"),digits = 3)

# plotting the silhouette index
ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = Silhouette.index)) + 
  geom_text(aes(x = k, y = m, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# plotting the Xie Beni index
ggplot(FCMvalues) + 
  geom_raster(aes(x = k, y = m, fill = XieBeni.index)) + 
  geom_text(aes(x = k, y = m, label = round(XieBeni.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# From the FCM select parameters I think that k = 3 and m = 1.6 or m = 2.0 would be good
## Based on the k and m values, select the value for beta using GFCM
GFCMvalues <- select_parameters.mc(algo = "GFCM", data = dataset_eco_sc,
                                      k = 3, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                      spconsist = FALSE, verbose = TRUE, init = "kpp",
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Negentropy.index", "Silhouette.index")) 
write_csv(GFCMvalues, here::here(paste0("outputs/gfcm_eco-attri_params_", Sys.Date(), ".csv")), append = FALSE)

knitr::kable(GFCMvalues[c("m","beta","Silhouette.index","XieBeni.index")],
             col.names = c("m","beta","silhouette index",
                           "Xie and Beni index"),digits = 3)

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

## From the GFCM I think that m = 2.0 and beta = 0.1
## Then select a value for alpha or both alpha and beta using SFCM and SGFCM
# Spatial FCM
w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

SFCMvalues <- select_parameters.mc(algo = "SFCM", data = dataset_eco_sc, 
                                       k = 3, m = 2.0,
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
write_csv(SFCMvalues, here::here(paste0("outputs/sfcm_eco-attri_params_", Sys.Date(), ".csv")), append = FALSE)

# plot the silhouette index
ggplot(SFCMvalues) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

ggplot(SFCMvalues) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# From the SFCM a window of 3x3 and alpha = 1.1 would be good

# Spatial Generalized FCM 
# beta only
SGFCMbeta <- select_parameters.mc(algo = "SGFCM", data = dataset_eco_sc, 
                                        k = 3, m = 2.0,
                                        beta = seq(0.1,0.9,0.1), alpha = 1.1,
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
write_csv(SGFCMbeta, here::here(paste0("outputs/sgfcm_eco-attri_params-beta_", Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
ggplot(SGFCMalpha) + 
  geom_raster(aes(x = beta, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = beta, y = window, label = round(Silhouette.index,2)), size = 2.0)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

ggplot(SGFCMalpha) + 
  geom_raster(aes(x = beta, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = beta, y = window, label = round(XieBeni.index,2)), size = 2.0)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# still looks like window = 3x3 and beta = 0.1

# alpha and beta
future::plan(future::multisession(workers=2))
SGFCMalphabeta <- select_parameters.mc(algo = "SGFCM", data = dataset_eco_sc,
                                        k = 3, m = 2.0, 
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
write_csv(SGFCMalphabeta, here::here(paste0("outputs/sgfcm_eco-attri_params-alphabeta_", Sys.Date(), ".csv")), append = FALSE)

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

## Based on the parameter optimization run the final SGFCM for the eco attributes
### k = 3, m = 2.0, window = 3x3, alpha = 0.6, beta = 0.1 (or 0)
SGFCM_result <- SGFCMeans(dataset_eco_sc, k = 3, m = 2.0, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 0.6, beta = 0.1,
                          seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")
saveRDS(SGFCM_result, here::here(paste0("data/processed/SGFCM_result_eco_", Sys.Date(), ".rds")))

map_SGFCM_result <- rast(SGFCM_result$rasters)
plot(map_SGFCM_result[["Groups"]])
writeRaster(map_SGFCM_result[["Groups"]], 
            filename = paste0("data/processed/SGFCM_result_eco_", 
                              Sys.Date(), ".tif"))

maps_sgfcm <- mapClusters(object = SGFCM_result, undecided = 0.2)

# Fuzzy ELSA
fuzzy_elsa_rast <- calcFuzzyELSA(SGFCM_result, window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 7, "Blues")
vals <- terra::values(fuzzy_elsa_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_rast, col = cols, breaks = limits$brks)

writeRaster(fuzzy_elsa_rast, here::here(paste0("data/processed/SGFCM_eco_felsa_", 
                                               Sys.Date(), ".tif")))
