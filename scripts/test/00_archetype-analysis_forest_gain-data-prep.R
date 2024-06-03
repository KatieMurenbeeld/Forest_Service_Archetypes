library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Set the timeout to 100 minutes (6000 seconds)
options(timeout=6000)


#---Note---------
# This data is downloaded from 
# https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/download.html
# one has to download the desire 10 x 10 degree boxes
# For the contiguous US this includes 
# 50N, 130W; 50N, 120W; 50N, 110W; 50N, 100W; 50N, 90W; 50N, 80W; 50N, 70W
# 40N, 130W; 40N, 120W; 40N, 110W; 40N, 100W; 40N, 90W; 40N, 80W
# 30N, 120W; 30N, 110W; 30N, 100W; 30N, 90W; 30N, 80W

granules <- c("50N_130W", "50N_120W", "50N_110W", "50N_100W", "50N_090W", 
              "50N_080W", "50N_070W", "40N_130W", "40N_120W", "40N_110W",
              "40N_100W", "40N_090W", "40N_080W", "30N_120W", "30N_110W",
              "30N_100W", "30N_090W", "30N_080W")

#granules <- c("50N_130W", "50N_120W")

# Download the data
download_forgain <- function(grans){    
  exdir <- here::here("data/original/forest_gain/")
  exfile <- paste0(exdir, "Hansen_GFC-2023-v1.11_gain_", grans, ".tif")
  gain.url <- paste0("https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_gain_", grans, ".tif")
  download.file(gain.url, exfile)
  fnames <- list.files(exdir)
  return(fnames)
}

for (gran in granules){
download_forgain(gran)
}

fnames_list <- list.files(here::here("data/original/forest_gain"), 
                          #pattern = "WHP", 
                          full.names = TRUE)

# check the crs and res of the rasters
# from website This global dataset is divided into 10x10 degree tiles, consisting of seven files per tile. 
# All files contain unsigned 8-bit values and have a spatial resolution of 1 arc-second per pixel, or approximately 30 meters per pixel at the equator.
# So I think I can keep the same factor? Could also load in whp rast as a reference raster?
test_rast <- rast(here::here("data/original/forest_gain/Hansen_GFC-2023-v1.11_gain_30N_080W.tif"))
test_rast

# For next time update this function to aggregate at 3km-3000m (fact = 100) and 1.5km-1500m (fact = 50)
agg_forgain <- function(ogrst, fac, res){
  rasters <- rast(ogrst)
  fnames.process <- paste0("data/processed/forestgain_aggregated/",names(rasters), "_", res, ".tif")
  rasters.agg <- aggregate(rasters, fact=fac, cores = 2)
  writeRaster(rasters.agg, fnames.process, overwrite=TRUE)
  return(fnames.process) 
}

agg_forgain(here::here("data/original/forest_gain/Hansen_GFC-2023-v1.11_gain_30N_080W.tif"),
            fac = 100, res = "3000m")

for (rst in fnames_list) {
  agg_forgain(rst, 100, "3000m")
}

prefix <- "forestgain"
res <- "3000m" 

merge_all_rst <- function(res){
  file.list <- list.files(here::here("data/processed/forestgain_aggregated"), pattern = res, full.names = TRUE)
  rasters <- lapply(file.list, function(x) rast(x))
  rst.sprc <- sprc(rasters)
  m <- merge(rst.sprc)
  names(m) <- prefix
  fnames.merge <- paste0(prefix, "_merge", res, ".tif")
  writeRaster(m, filename = paste0("data/processed/forestgain_merged/", fnames.merge), overwrite=TRUE)
  return(paste0("data/processed/forestgain_merged/", fnames.merge))
}

for (r in res) {
  merge_all_rst(r)
}

forgain_rst <- rast(here::here("data/processed/forestgain_merged/forestgain_merge3000m.tif"))
plot(forgain_rst)
