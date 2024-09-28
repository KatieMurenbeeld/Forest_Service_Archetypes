library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)
library(gstat)

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
  dplyr::select(GEOID, NAME) %>%
  mutate(NAME = gsub(" ", "", NAME)) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  filter(GEOID != 2 & GEOID != 11 & GEOID != 15 & GEOID < 60) %>%
  dplyr::select(NAME)

# may need to complete in chunks by indexing st_list
for (state in st_list[31:48,]) {
  download_fire(state)
}

fnames_list <- list.files(here::here("data/original/fire"), pattern = "WHP", full.names = TRUE)

# For next time update this function to aggregate at 3km-3000m (fact = 100) and 1.5km-1500m (fact = 50)
agg_fire <- function(ogrst, fac, res){
  rasters <- rast(ogrst)
  fnames.process <- paste0("data/processed/aggregated/",names(rasters), "_", res, "_", Sys.Date(), ".tif")
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
  fnames.merge <- paste0(prefix, Sys.Date(), "_merge", res, ".tif")
  writeRaster(m, filename = paste0("data/processed/merged/", fnames.merge), overwrite=TRUE)
  return( paste0("data/processed/merged/", fnames.merge))
}

for (r in res) {
  merge_all_rst(r)
}

merge_all_rst("3000m")
plot(rast(here::here("data/processed/merged/WHP2024-08-09_merge3000m.tif")))

#===================
# merge then aggregate in order to avoid gaps between the states

merge_all_rst <- function(prefix){
  fnames_list <- list.files(here::here("data/original/fire"), pattern = "WHP", full.names = TRUE)
  rasters <- lapply(fnames_list, function(x) rast(x))
  rst.sprc <- sprc(rasters)
  m <- merge(rst.sprc)
  names(m) <- prefix
  fnames.merge <- paste0(prefix, "_", Sys.Date(), "_merged.tif")
  writeRaster(m, filename = paste0("data/processed/merged/", fnames.merge), overwrite=TRUE)
  return(paste0("data/processed/merged/", fnames.merge))
}

merge_all_rst(prefix)

# check the plot
plot(rast(here::here("data/processed/merged/WHP_2024-08-09_merged.tif")))
# read in the new raster
whp_merged <- rast(here::here("data/processed/merged/WHP_2024-08-09_merged.tif"))
# aggregate to 3km
conus_whp_3km_agg <- aggregate(whp_merged,
                               fact = 100,
                               cores = 2)

conus_whp_1.5km_agg <- aggregate(whp_merged,
                               fact = 50,
                               cores = 2)
# Check the plot
plot(conus_whp_3km_agg)
# save the new merged and aggregated WHP raster
writeRaster(conus_whp_3km_agg, paste0(here::here("data/processed/merged/"), "conus_whp_3km_agg_", Sys.Date(), ".tif"))
writeRaster(conus_whp_1.5km_agg, paste0(here::here("data/processed/merged/"), "conus_whp_15km_agg_", Sys.Date(), ".tif"))

## fill in missing data through IDW
### inverse distance weighted (IDW)
conus_whp_3km_agg <- rast(here::here("data/processed/merged/conus_whp_3km_agg_2024-08-09.tif"))
plot(conus_whp_3km_agg)
whp_df <- as.data.frame(conus_whp_3km_agg, xy=TRUE)

mod <- gstat(id = "WHP", formula = WHP~1, locations = ~x+y, data = whp_df,
            nmax = 7, set = list(idp = 0.5))

whp_interp <- interpolate(conus_whp_3km_agg, mod, debug.level = 0, index = 1)
writeRaster(whp_interp, paste0(here::here("data/processed/merged/"), "conus_whp_3km_agg_interp_", Sys.Date(), ".tif"))

whp_focal <- focal(conus_whp_3km_agg, w=3, fun=mean, na.policy="only", na.rm = TRUE)

# Crop interpolated (focal and IDW) to conus states

### Load the states from tigris
states <- tigris::states(cb = TRUE)
### Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI",] #only CONUS

### Filter tigris states for conus states and set crs to crs of raster
conus_states <- states %>%
  filter(STUSPS %in% continental.states$state) %>%
  dplyr::select(STUSPS, GEOID, geometry) %>%
  st_transform(., crs = crs(conus_whp_3km_agg))

plot(conus_states$geometry)
plot(crop(whp_focal, conus_states))
plot(crop(whp_interp, conus_states, mask = TRUE))

whp_focal_crop <- crop(whp_focal, conus_states)
whp_interp_crop <- crop(whp_interp, conus_states, mask = TRUE)

writeRaster(whp_interp_crop, paste0(here::here("data/processed/merged/"), "conus_whp_3km_agg_interp_crop_", Sys.Date(), ".tif"))
writeRaster(whp_focal_crop, paste0(here::here("data/processed/merged/"), "conus_whp_3km_agg_focal_crop_", Sys.Date(), ".tif"))



