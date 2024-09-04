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

# Create the SGFCM
w1 <- matrix(1, nrow = 3, ncol = 3)

SGFCM_result <- SGFCMeans(dataset_pmrc_poli, k = 8, m = 1.6, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 1.3, beta = 0.1,
                          seed = 6891, tol = 0.001, verbose = FALSE, init = "kpp")
#SGFCM_results <- readRDS(here::here("data/processed/SGFCM_result_pmrc_poli_2024-08-26.rds"))

map_SGFCM_result <- rast(SGFCM_result$rasters)
plot(map_SGFCM_result[["Groups"]])
#writeRaster(map_SGFCM_result[["Groups"]], filename = paste0("data/processed/SGFCM_result_pmrc_poli_", Sys.Date(), ".tif"))

#map_SGFCM_result <- rast(here::here("data/processed/SGFCM_result_pmrc_poli_2024-08-26.tif"))
maps_sgfcm <- mapClusters(object = SGFCM_result, undecided = 0.2)


maps_sgfcm$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set3")

# Fuzzy ELSA
fuzzy_elsa_rast <- calcFuzzyELSA(SGFCM_result, window = matrix(1,nrow = 3, ncol = 3))

cols <- RColorBrewer::brewer.pal(n = 3, "Blues")
vals <- terra::values(fuzzy_elsa_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 3, style = "kmeans") 
plot(fuzzy_elsa_rast, col = cols, breaks = limits$brks)
plot(fs_nf.pr)

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

felsa.df <- fuzzy_elsa_rast$pct_forpay_fill %>% as.data.frame(xy = TRUE)

felsa_rg_nf_map <- ggplot() +
  geom_raster(aes(x = felsa.df$x, y = felsa.df$y, fill = felsa.df$pct_forpay_fill)) +
  #geom_raster(fuzzy_elsa_rast) + 
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  labs(title = "Fuzzy ELSA") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

felsa_rg_nf_map

