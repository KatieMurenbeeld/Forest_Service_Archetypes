library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(distributional)
library(ggdist)
library(ggsci)

# Load the data
ref_rast <- rast(here::here("data/toy/processed/merged/WHP_merge3000m.tif"))
fs_nf <- st_read(here::here("data/toy/original/S_USA.AdministrativeForest.shp"))
fs_reg <- st_read(here::here("data/toy/original/S_USA.AdministrativeRegion.shp"))
conus_attri <- rast(here::here("data/toy/processed/toy_rast_fcm_2024-05-29.tif"))
map.conus <- rast(here::here("data/toy/processed/FCM_2024-05-29.tif"))

## Reproject the forest service shapes to NAD83
fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=crs(ref_rast))
fs_nf.crop <- st_crop(fs_nf.proj, ext(ref_rast))

fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=crs(ref_rast))
fs_reg.crop <- st_crop(fs_reg.proj, ext(ref_rast))

## Crop the data to the 

## Create a map of the clusters with the National Forest boundaries
group.df <- map.conus$Groups %>% as.data.frame(xy = TRUE)

fcm_nf_map <- ggplot() +
  geom_raster(aes(x = group.df$x, y = group.df$y, fill = as.factor(group.df$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black", size = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Fuzzy Cluster Map: k=5, m=1.7") +
  theme(legend.position = "bottom")

fcm_nf_map
ggsave(here::here("figures/toy/fcm_nf_map.png"), plot = fcm_nf_map, width = 12, height = 12, dpi = 300)  

### And the Forest Region boundaries

fcm_reg_map <- ggplot() +
  geom_raster(aes(x = group.df$x, y = group.df$y, fill = as.factor(group.df$Groups))) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", size = 150) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Fuzzy Cluster Map: k=5, m=1.7") +
  theme(legend.position = "bottom")

fcm_reg_map
ggsave(here::here("figures/toy/fcm_region_map.png"), plot = fcm_reg_map, width = 12, height = 12, dpi = 300)  
