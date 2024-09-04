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
dataset_pmrc_poli <- lapply(names(rst_fcm_pmrc_poli_sc), function(n){
  aband <- rst_fcm_pmrc_poli_sc[[n]]
  return(aband)
})
names(dataset_pmrc_poli) <- names(rst_fcm_pmrc_poli_sc)

# Format for use with Kmeans
df <- as.data.frame(rst_fcm_pmrc_poli_sc, xy=TRUE)
df_nogeo <- df %>%
  dplyr::select(-c("x", "y")) %>%
  drop_na()

# Create a dataframe for ease of mapping and looking at results
df_all <- as.data.frame(rst_fcm_pmrc_poli, xy = TRUE, na.rm = TRUE)
df_all_sc <- as.data.frame(rst_fcm_pmrc_poli_sc, xy = TRUE, na.rm = TRUE)
df_all_zsc <- as.data.frame(scale(rst_fcm_pmrc_poli), xy = TRUE, na.rm = TRUE)

#----Use Classical Kmeans to explore the data---- 
#----and choose the right number of clusters k

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

# Looks like a good number of K would be 4 

# Look at a map of the groups from the kmeans 
KMeanClust <-  kmeans(df_nogeo,
                      centers = 4,
                      iter.max = 150)
df_nogeo$Cluster <-paste("cluster",
                   KMeanClust$cluster,
                   sep = "_")

df_kmean_map <- full_join(df, df_nogeo)

ggplot(df_kmean_map, aes(x = x, y = y)) + 
  geom_point(aes(colour = Cluster))

#----Since using Rasters will switch over to the raster vignette----
# to select the k and m values. Previously we selected k = 8 and m = 0.1625
future::plan(future::multisession(workers = 2))
#FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset_pmrc_poli, 
#                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
#                                  indices = c("XieBeni.index", "Explained.inertia",
#                                              "Negentropy.index", "Silhouette.index"),
#                                  verbose = TRUE)

GFCMvalues_k8 <- select_parameters.mc(algo = "GFCM", data = dataset_pmrc_poli,
                                   k = 8, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                   spconsist = FALSE, verbose = TRUE, init = "kpp",
                                   indices = c("XieBeni.index", "Explained.inertia",
                                               "Negentropy.index", "Silhouette.index"))  
write_csv(GFCMvalues_k8, here::here(paste0("outputs/gfcm_pmrc_poli_indices_", Sys.Date(), ".csv")), append = FALSE)
  
GFCMvalues_k4 <- select_parameters.mc(algo = "GFCM", data = dataset_pmrc_poli,
                                      k = 4, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                      spconsist = FALSE, verbose = TRUE, init = "kpp",
                                      indices = c("XieBeni.index", "Explained.inertia",
                                                  "Negentropy.index", "Silhouette.index"))  

# plotting the silhouette index
ggplot(GFCMvalues_k8) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

ggplot(GFCMvalues_k4) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# plotting the explained inertia
ggplot(GFCMvalues_k8) + 
  geom_raster(aes(x = m, y = beta, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = beta, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# plotting the explained inertia
ggplot(GFCMvalues_k4) + 
  geom_raster(aes(x = m, y = beta, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = beta, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# Spatial FCM
w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

SFCMvalues_m16 <- select_parameters.mc(algo = "SFCM", data = dataset_pmrc_poli, 
                                          k = 8, m = 1.6,
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

SFCMvalues_m16$window <- dict$window[match(SFCMvalues_m16$window,dict$w)]
write_csv(SFCMvalues_m16, here::here(paste0("outputs/sfcm_pmrc_poli_indices_", Sys.Date(), ".csv")), append = FALSE)


SFCM_result <- SFCMeans(dataset_pmrc_poli, k = 8, m = 1.6, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 1.3,
                          seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")
saveRDS(SFCM_result, here::here(paste0("data/processed/SFCM_result_pmrc_poli_", Sys.Date(), ".rds")))

map_SFCM_result <- rast(SFCM_result$rasters)
plot(map_SFCM_result[["Groups"]])
writeRaster(map_SFCM_result[["Groups"]], filename = paste0("data/processed/SFCM_result_pmrc_poli_", Sys.Date(), ".tif"))

maps_sfcm_undecide02 <- mapClusters(object = SFCM_result, undecided = 0.2)

# plotting the most likely categories
maps_sfcm_undecide02$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set3")

# Spatial GFCM
# Start here when you get home. 
future::plan(future::multisession(workers = 2))
SGFCMvalues_m16 <- select_parameters.mc(algo = "SGFCM", data = dataset_pmrc_poli, 
                                    k = 8, m = 1.6,
                                    beta = 0.1, alpha = seq(0.5,2,0.1),
                                    window = list(w1,w2,w3),
                                    spconsist = TRUE, nrep = 5, 
                                    verbose = TRUE, chunk_size = 4,
                                    seed = 456, init = "kpp",
                                    indices = c("XieBeni.index", "Explained.inertia",
                                                "Negentropy.index", "Silhouette.index"))


dict <- data.frame(
  w = c(1,2,3),
  window = c("3x3","5x5","7x7")
)

SGFCMvalues_m16$window <- dict$window[match(SGFCMvalues_m16$window,dict$w)]

write_csv(SGFCMvalues_m16, here::here(paste0("outputs/sgfcm_pmrc_poli_indices_", Sys.Date(), ".csv")), append = FALSE)

# showing the silhouette index
ggplot(SGFCMvalues_m16) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 1.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# showing the explained inertia
ggplot(SGFCMvalues_m16) + 
  geom_raster(aes(x = alpha, y = window, fill = Explained.inertia)) + 
  geom_text(aes(x = alpha, y = window, label = round(Explained.inertia,2)), size = 1.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# showing the spatial inconsistency
ggplot(SGFCMvalues_m16) + 
  geom_raster(aes(x = alpha, y = window, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = window, label = round(spConsistency,2)), size = 1.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# showing the Xie Beni index
ggplot(SGFCMvalues_m1625) + 
  geom_raster(aes(x = alpha, y = window, fill = XieBeni.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(XieBeni.index,2)), size = 1.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# alpha of 1.3 will be good
SGFCM_result <- SGFCMeans(dataset_pmrc_poli, k = 8, m = 1.6, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 1.3, beta = 0.1,
                          seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")
#saveRDS(SGFCM_result, here::here(paste0("data/processed/SGFCM_result_pmrc_poli_", Sys.Date(), ".rds")))
#SGFCM_result <- readRDS(here::here("data/processed/SGFCM_result_pmrc_poli_2024-08-26.rds"))

map_SGFCM_result <- rast(SGFCM_result$rasters)
plot(map_SGFCM_result[["Groups"]])
#writeRaster(map_SGFCM_result[["Groups"]], filename = paste0("data/processed/SGFCM_result_pmrc_poli_", Sys.Date(), ".tif"))

maps_sgfcm <- mapClusters(object = SGFCM_result, undecided = 0.2)

# plotting the most likely categories
sgfcm_undecided <- maps_sgfcm$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set3")
ggsave(filename = here::here(paste0("figures/sgfcm_undecided_cluster_", Sys.Date(), ".png")),
       plot = sgfcm_undecided, 
       width = 12, height = 12, dpi = 300)

# violin plots
violinPlots(df_all_zsc, SGFCM_result$Groups)

## looking at alpha and beta for SGFCM
future::plan(future::multisession(workers=2))
DFindices_SGFCM <- select_parameters.mc(algo = "SGFCM", data = dataset_pmrc_poli,
                                       k = 8, m = 1.625, 
                                       beta = seq(0,1.0,0.1), alpha = seq(0,2,0.1),
                                       window = w1, spconsist = TRUE, nrep = 5, 
                                       verbose = TRUE, chunk_size = 4,
                                       seed = 456, init = "kpp",
                                       indices = c("XieBeni.index", "Explained.inertia",
                                                   "Negentropy.index", "Silhouette.index"))
#write_csv(DFindices_SGFCM, here::here(paste0("outputs/sgfcm_pmrc_poli_indices_alpha_beta_", Sys.Date(), ".csv")), append = FALSE)
DFindices_SGFCM <- read_csv(here::here("outputs/sgfcm_pmrc_poli_indices_alpha_beta_2024-08-22.csv"))

ggplot(DFindices_SGFCM) + 
  geom_raster(aes(x = alpha, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = beta, label = round(Silhouette.index,2)), size = 2.0)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)
# alpha = 1.3 and beta = 0.1 look good. Could also try alpha of 1.6
ggplot(DFindices_SFGCM) + 
  geom_raster(aes(x = alpha, y = beta, fill = XieBeni.index),  size = 5) + 
  scale_fill_viridis() +
  coord_fixed(ratio=1)
ggplot(DFindices_SFGCM) + 
  geom_raster(aes(x = alpha, y = beta, fill = spConsistency),  size = 5) + 
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# want to try alpha of 1.6 too (will result in more spatial smoothing)
SGFCM_result_a1.6 <- SGFCMeans(dataset_pmrc_poli, k = 8, m = 1.625, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 1.6, beta = 0.1,
                          seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")

maps_a1.6 <- mapClusters(object = SGFCM_result_a1.6, undecided = 0.2)

# plotting the most likely categories
maps_a1.6$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set3")


# Compare belongings between SFCM and SGFCM

SFCMMaps <- mapClusters(object = SFCM_result, undecided = 0.45)
SGFCMMaps <- mapClusters(object = SGFCM_result, undecided = 0.45)

require(gridExtra)
grid.arrange(SFCMMaps$ProbaMaps[[1]],SGFCMMaps$ProbaMaps[[1]],
             ncol = 2)
grid.arrange(SFCMMaps$ProbaMaps[[2]],SGFCMMaps$ProbaMaps[[2]],
             ncol = 2)

#----more finished looking maps and comparing with FCM----

# Compare to FCM 
FCM_result_pmrc_poli <- CMeans(dataset_pmrc_poli, k = 8, m = 1.625, 
                               standardize = FALSE, seed = 6891, 
                               tol = 0.001)

maps_fcm <- mapClusters(object = FCM_result_pmrc_poli, undecided = 0.2)
maps_fcm$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set3")

# Spatial Diagnostic
diagSGFCM <- spatialDiag(SGFCM_result, window = matrix(1, nrow = 3, ncol = 3), nrep = 5)

knitr::kable(diagSGFCM$MoranValues, digits = 3, row.names = FALSE)

# Shannon entropy index
df_all$entropyidx  <- calcUncertaintyIndex(SGFCM_result$Belongings)

ggplot(df_all, aes(x = x, y = y)) + 
  geom_point(aes(colour = entropyidx))

# calculating the local Moran I values
loc_moran1 <- calc_local_moran_raster(SGFCM_result$rasters$group1,w1)
loc_moran2 <- calc_local_moran_raster(SGFCM_result$rasters$group2,w1)
loc_moran3 <- calc_local_moran_raster(SGFCM_result$rasters$group3,w1)
loc_moran4 <- calc_local_moran_raster(SGFCM_result$rasters$group4,w1)
loc_moran5 <- calc_local_moran_raster(SGFCM_result$rasters$group5,w1)
loc_moran6 <- calc_local_moran_raster(SGFCM_result$rasters$group6,w1)
loc_moran7 <- calc_local_moran_raster(SGFCM_result$rasters$group7,w1)
loc_moran8 <- calc_local_moran_raster(SGFCM_result$rasters$group8,w1)

# mapping the values
cols <- rev(RColorBrewer::brewer.pal(n = 8, "Spectral"))
vals <- terra::values(loc_moran8, mat = FALSE) 
limits <- classIntervals(vals,  n = 8, style = "kmeans") 
plot(loc_moran8, col = cols, breaks = limits$brks)
#sgfcm_lm1 <- plot(loc_moran1, col = cols, breaks = limits$brks)

# mapping the values using ELSA
cols <- RColorBrewer::brewer.pal(n = 7, "Greys")
vals <- terra::values(diagSGFCM$Elsa, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(diagSGFCM$Elsa, col = cols, breaks = limits$brks)

# Fuzzy ELSA
fuzzy_elsa_rast <- calcFuzzyELSA(SGFCM_result,window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 7, "Greys")
vals <- terra::values(fuzzy_elsa_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_rast, col = cols, breaks = limits$brks)

# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(rst_fcm_pmrc_poli_sc))


fcm.pmrc.poli.df <- FCM_result_pmrc_poli$Groups %>% as.data.frame(xy = TRUE)

pmrc_poli_rg_nf_map <- ggplot() +
  geom_raster(aes(x = fcm.pmrc.poli.df$x, y = fcm.pmrc.poli.df$y, fill = as.factor(fcm.pmrc.poli.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "PMRC with AIP: k=8, m=1.625", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

pmrc_poli_rg_nf_map

sgfcm.pmrc.poli.df <- SGFCM_result$Groups %>% as.data.frame(xy = TRUE)

sgfcm_nfreg_map <- ggplot() +
  geom_raster(aes(x = sgfcm.pmrc.poli.df$x, y = sgfcm.pmrc.poli.df$y, fill = as.factor(sgfcm.pmrc.poli.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "PMRC with AIP: k=8, m=1.625", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

sgfcm_nfreg_map



