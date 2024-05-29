library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Set the timeout to 100 minutes (6000 seconds)
options(timeout=6000)

# Only want the wildfire hazard potential (WHP)

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
  fnames <- paste0("data/original/",names(rasters), ".tif")
  print(fnames)
  writeRaster(rasters, filename = fnames, overwrite = TRUE)
  return(fnames)
}

# Create state list, excluding Alaska, DC, HI, and territories
states <- st_drop_geometry(states())
st_list <- states %>%
  select(GEOID, NAME) %>%
  mutate(NAME = gsub(" ", "", NAME)) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  filter(GEOID != 2 & GEOID != 11 & GEOID != 15 & GEOID < 60) %>%
  select(NAME)

for (state in st_list[31:48,]) {
  download_fire(state)
}

fnames_list <- list.files(here::here("data/original/"), pattern = "WHP", full.names = TRUE)

# For next time update this function to aggregate at 3km-3000m (fact = 100) and 1.5km-1500m (fact = 50)
agg_fire <- function(ogrst, fac, res){
  rasters <- rast(ogrst)
  fnames.process <- paste0("data/processed/aggregated/",names(rasters), "_", res, ".tif")
  rasters.agg <- aggregate(rasters, fact=fac, cores = 2)
  writeRaster(rasters.agg, fnames.process, overwrite=TRUE)
  return(fnames.process) 
}

for (rst in fnames_list) {
  agg_fire(rst, 100, "3000m")
}

prefix <- "WHP"
res <- c("1500m", "3000m") 

merge_all_rst <- function(res){
  file.list <- list.files(here::here("data/processed/aggregated"), pattern = res, full.names = TRUE)
  rasters <- lapply(file.list, function(x) rast(x))
  rst.sprc <- sprc(rasters)
  m <- merge(rst.sprc)
  names(m) <- prefix
  fnames.merge <- paste0(prefix, "_merge", res, ".tif")
  writeRaster(m, filename = paste0("data/processed/merged/", fnames.merge), overwrite=TRUE)
  return( paste0("data/processed/merged/", fnames.merge))
}

for (r in res) {
  merge_all_rst(r)
}
