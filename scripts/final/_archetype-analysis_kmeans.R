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

# Using k_means() from Terra package
set.seed(1986)
km_pmrc_poli <- k_means(rst_fcm_pmrc_poli_sc, centers=8, iter.max = 100, algorithm = "Lloyd")
plot(km_pmrc_poli)

# Using kmeans() from stats package?
df <- km_pmrc_poli$lyr1 %>% as.data.frame(xy = TRUE)

kmeans_map <- ggplot() +
  geom_raster(aes(x = df$x, y = df$y, fill = as.factor(df$lyr1))) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

kmeans_map
