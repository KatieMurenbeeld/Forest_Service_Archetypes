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

