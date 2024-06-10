library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(raster)
library(exactextractr) # I want this package but cannot seem to install because I was spelling it wrong :-/
library(tigris)

projection <- "epsg:5070"

whp_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
for_own <- rast(here::here("data/original/Data/forest_own1/forest_own1.tif"))

whp_rast_proj <- project(whp_rast, projection)
for_own_proj <- readRDS(here::here("data/processed/for_own_proj.rds"))

## reclassify the raster
### make reclassification matrix
m <- c(1, 4, 1, 
       4, 8, 0) 
#       NA, NA, 0)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)

### reclassify using matrix, make NA = 0
for_own_proj_rc <- classify(for_own_proj, rclmat, include.lowest=TRUE, others = 0)

for_own_proj_rc[is.na(for_own_proj_rc)] <- 0 

# test for idaho
## Get list of states in the CONUS
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

## Download the county boundaries and filter by states
counties <- tigris::counties(state = continental.states$state, cb = TRUE)
counties_proj <- counties %>% st_transform(., crs = projection)

## test for Idaho
id_counties <- counties_proj %>%
  filter(STUSPS == "ID")

## create sf of cells for Idaho?
id_cells_sf <- st_make_grid(id_counties, cellsize = 30000, what = "polygons")
id_cells_sf <- st_sf(id_cells_sf) 
plot(st_geometry(id_cells_sf))
plot(st_geometry(id_counties), add = TRUE)

for_own_proj_rc_id <- crop(for_own_proj_rc, st_geometry(id_cells_sf))
plot(for_own_proj_rc_id)
plot(st_geometry(id_cells_sf), add = TRUE)
plot(id_cells_rst, add = TRUE, alpha = 0.5)

# this is quick but I don't like going from raster to sf and back to raster
id_cells_frac <- cbind(id_cells_sf, exact_extract(for_own_rc_id, id_cells_sf, "frac"))

##----Create a template raster for the shapefiles----
XMIN <- ext(id_cells_sf)$xmin
XMAX <- ext(id_cells_sf)$xmax
YMIN <- ext(id_cells_sf)$ymin
YMAX <- ext(id_cells_sf)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 30000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
id_cells_rst <- rast(ncol=NCOLS, nrow=NROWS, 
                     xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                     vals=1, crs=projection)
#------------------------------------------------
id_cells_frac_rast <- rasterize(id_cells_frac, id_cells_rst, field = "frac_1")
# could I just resample since the values are either 0 or 1?
id_cells_resamp <- resample(for_own_rc_id, id_cells_rst) # I think this is what I want
# but I need the white areas to be filled with 0s
#id_cells_resamp_ave <- resample(for_own_rc_id, id_cells_rst, method = "average") 

#for_own_rc_id <- project(for_own_rc_id, "epsg:5070")
#id_cells_rast <- project(id_cells_rast, "epsg:5070")

id_cells_rst_resamp <- resample(id_cells_rst, for_own_proj_rc_id) # still not quite right
id_cells_rst_ext <- extend(id_cells_rst, ext(for_own_proj_rc_id))
id_cells_rst_crop <- crop(id_cells_rst, ext(for_own_rc_id))

id_zonal <- zonal(for_own_proj_rc_id, id_cells_rst_resamp, fun = "mean", as.raster = TRUE, na.rm = TRUE)
id_zonal_sf <- zonal(for_own_proj_rc_id, vect(id_cells_sf), fun = "mean", as.raster = TRUE, na.rm = TRUE)
id_zonal_sf
saveRDS(id_zonal, here::here("data/processed/id_zonal.rds"))
saveRDS(id_zonal_sf, here::here("data/processed/id_zonal_sf.rds"))
plot(id_zonal)

# set all NAs to 0 ahead of time
# before running 

# the reclassified original data 0 = public foresr and 1 = private forest
plot(for_own_rc_id)
# using exact_extract and then re-rasterizing 
plot(id_cells_frac_rast)
# using resample with a template raster
plot(id_cells_resamp)

