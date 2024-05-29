# Download wildfire hazard potential data from USGS
# Aggregate to desired resolution and merge the selected state rasters

# 0. Load libraries and set timeout
library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Set the timeout to 100 minutes (6000 seconds)
options(timeout=6000)

# Steps:
# 0. Load libraries and set timeout
# 1. Download the rasters for ID, MT, WA, and OR
# 2. Aggregate to 3km resolution
# 3. Merge state rasters
# 4. Save raster file

# 1. Down load the the wildfire hazard potential (WHP) rasters

# Create function
download_fire <- function(st){    
  tmp <- tempfile()
  fs.url <- paste0("https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2020-0016/RDS-2020-0016_",st,".zip")
  download.file(fs.url, tmp)
  tmp2 <- tempfile()
  unzip(zipfile=tmp, exdir = tmp2 )
  dir.name <- list.files(tmp2)
  rast.file <- list.files(paste0(tmp2,"/", dir.name), pattern="*.tif$", full.names = TRUE)
  whp.rast.file <- rast.file[grepl("WHP", rast.file)]
  rasters <- rast(whp.rast.file)
  fnames <- paste0("data/toy/original/fire/",names(rasters), ".tif")
  print(fnames)
  writeRaster(rasters, filename = fnames, overwrite = TRUE)
  return(fnames)
}

# Create state list, only include Idaho (16), Montana (30), Washington (53), and Oregon (41)
states <- st_drop_geometry(states())
st_list <- states %>%
  select(GEOID, NAME) %>%
  mutate(NAME = gsub(" ", "", NAME)) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  filter(GEOID == 16 | GEOID == 30 | GEOID == 53 | GEOID == 41) %>%
  select(NAME)

# download fire data by indexing st_list
for (state in st_list$NAME) {
  print(state)
  download_fire(state)
}

print("WHP fire data downloaded by state.")

# 2. Aggregate the state rasters to 3km resolution
# Function to aggregate: for 3km-3000m (fact = 100)
agg_fire <- function(ogrst, fac, res){
  rasters <- rast(ogrst)
  fnames.process <- paste0("data/toy/processed/aggregated/",names(rasters), "_", res, ".tif")
  rasters.agg <- aggregate(rasters, fact=fac, cores = 2)
  writeRaster(rasters.agg, fnames.process, overwrite=TRUE)
  return(fnames.process) 
}

## Create a list of the downloaded WHP files
fnames_list <- list.files(here::here("data/toy/original/fire/"), pattern = "WHP", full.names = TRUE)

for (rst in fnames_list) {
  agg_fire(rst, 100, "3000m")
}

prefix <- "WHP"
res <- "3000m" 

print(paste0("WHP data aggregated to ", res))

# 3. & 4. Merge the state rasters into one raster and save
# Function to merge
merge_all_rst <- function(res){
  file.list <- list.files(here::here("data/toy/processed/aggregated"), pattern = res, full.names = TRUE)
  rasters <- lapply(file.list, function(x) rast(x))
  rst.sprc <- sprc(rasters)
  m <- merge(rst.sprc)
  names(m) <- prefix
  fnames.merge <- paste0(prefix, "_merge", res, ".tif")
  writeRaster(m, filename = paste0("data/toy/processed/merged/", fnames.merge), overwrite=TRUE)
  return( paste0("data/toy/processed/merged/", fnames.merge))
}

for (r in res) {
  merge_all_rst(r)
}

print("WHP data merged and saved.")
