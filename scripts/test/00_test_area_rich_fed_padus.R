library(tidyverse)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(tigris)
library(tmap)
library(raster)
library(units)
library(purrr)
library(progress)

projection = "epsg:5070"

nc <- st_read(system.file("shape/nc.shp", package="sf"))

fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"), layer = "PADUS4_0Fee") 

nc_fed <- fed %>%
  filter(State_Nm == "NC") %>%
  filter(Mang_Type == "FED")

rm(fed)

# reproject
nc_proj <- st_transform(nc, crs = projection)
fed_proj <- st_transform(nc_fed, crs = projection)
fed_proj_name <- fed_proj %>%
  dplyr::select(Mang_Name)
fed_proj_type <- fed_proj %>%
  dplyr::select(Mang_Type)

nc_cells <- st_make_grid(nc_proj, cellsize = 3000)
nc_cells_sf <- st_sf(nc_cells) 
# add unique cell id
nc_cells_sf <- nc_cells_sf %>% 
  mutate(GRIDCELL_REFID = as.character(row_number()))

#plot(st_geometry(nc_proj))
#plot(st_geometry(nc_cells_sf), add = TRUE)
#plot(st_geometry(fed_proj_type), add = TRUE)

#---% area fed----
fed_type_union <- fed_proj_type %>%
  st_union(.)

## Create a template raster for the shapefiles
XMIN <- ext(nc_cells_sf)$xmin
XMAX <- ext(nc_cells_sf)$xmax
YMIN <- ext(nc_cells_sf)$ymin
YMAX <- ext(nc_cells_sf)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
nc_cells_rst <- rast(ncol=NCOLS, nrow=NROWS, 
                        xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                        vals=1, crs=projection)

nc_fed_type_int <- st_intersection(nc_cells_sf, fed_type_union)
nc_fed_type_int_ar <- nc_fed_type_int %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (3000*3000))

nc_fedtype_int_ar <- rasterize(nc_fed_type_int_ar, nc_cells_rst, field = "percent_area")
plot(nc_fedtype_int_ar)
plot(st_geometry(nc_proj), add = TRUE)
#plot(st_geometry(fed_type_union), add = TRUE)

intersections <- st_intersects(nc_cells_sf, fed_type_union)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(nc_cells_sf)[1])

intersectFeatures <- map_dfr(1:dim(nc_cells_sf)[1], function(ix){
  pb$tick()
  st_intersection(x = nc_cells_sf[ix,], y = fed_type_union[intersections[[ix]],])
})

test_intersectFeatures <- head(intersectFeatures, n = 2047L)

intersectFeatures_ar <- test_intersectFeatures %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (3000*3000))

identical(intersectFeatures_ar$percent_area, nc_fed_type_int_ar$percent_area)

intersectFeatures_ar_rst <- rasterize(intersectFeatures_ar, nc_cells_rst, field = "percent_area")
plot(intersectFeatures_ar_rst)
plot(st_geometry(nc_proj), add = TRUE)
#plot(st_geometry(fed_proj_type), add = TRUE)

identical(intersectFeatures_ar_rst, nc_fedtype_int_ar)

#----richness----

nc_fed_name_int <- st_intersection(nc_cells_sf, fed_proj_name)
nc_fed_name_rich <- nc_fed_name_int %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

nc_fed_name_rich_rst <- rasterize(nc_fed_name_rich, nc_cells_rst, field = "numfed")

plot(nc_fed_name_rich_rst)
plot(st_geometry(nc_proj), add = TRUE)

intersections_rich <- st_intersects(nc_cells_sf, fed_proj_name)

pb2 <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(nc_cells_sf)[1])

intersectFeatures_rich <- map_dfr(1:dim(nc_cells_sf)[1], function(ix){
  pb2$tick()
  st_intersection(x = nc_cells_sf[ix,], y = fed_proj_name[intersections_rich[[ix]],])
})

#hd_intersectFeatures_rich <- head(intersectFeatures_rich, n = 79L)

intersectFeatures_richness <- intersectFeatures_rich %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

identical(nc_fed_name_rich, intersectFeatures_richness)
identical(nc_fed_name_rich$numfed, intersectFeatures_richness$numfed)

intersectFeatures_rich_rst <- rasterize(intersectFeatures_richness, nc_cells_rst, field = "numfed")

plot(intersectFeatures_rich_rst)
plot(st_geometry(nc_proj), add = TRUE)


identical(nc_fed_name_rich_rst, intersectFeatures_rich_rst)



#----Now run for CONUS----

## Load county boundaries from tigris
counties <- tigris::counties()
# create the list of CONUS states 
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

counties <- tigris::counties()
counties <- counties %>%
  filter(STATEFP %in% continental.states$FIPS) %>%
  dplyr::select(GEOID, COUNTYFP, STATEFP, geometry)

fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"), layer = "PADUS4_0Fee") 

conus_fed <- fed %>%
  filter(State_Nm %in% continental.states$state) %>%
  filter(Mang_Type == "FED")

rm(fed)

# reproject
counties_proj <- st_transform(counties, crs = projection)
conus_fed_proj <- st_transform(conus_fed, crs = projection)
conus_fed_proj_name <- conus_fed_proj %>%
  dplyr::select(Mang_Name)
conus_fed_proj_type <- conus_fed_proj %>%
  dplyr::select(Mang_Type)

# create a 3km grid for conus
conus_cells <- st_make_grid(counties_proj, cellsize = 3000)
conus_cells_sf <- st_sf(conus_cells) 

# add unique cell id
conus_cells_sf <- conus_cells_sf %>% 
  mutate(GRIDCELL_REFID = as.character(row_number()))

## Create a template raster for the shapefiles
XMIN <- ext(conus_cells_sf)$xmin
XMAX <- ext(conus_cells_sf)$xmax
YMIN <- ext(conus_cells_sf)$ymin
YMAX <- ext(conus_cells_sf)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
conus_cells_rst <- rast(ncol=NCOLS, nrow=NROWS, 
                     xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                     vals=1, crs=projection)

# Calculate the % area of federal land
conus_fed_type_union <- conus_fed_proj_type %>%
  st_union(.)

intersections_fedtype <- st_intersects(conus_cells_sf, conus_fed_type_union)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(conus_cells_sf)[1])

intersectFeatures_fedtype <- map_dfr(1:dim(nc_cells_sf)[1], function(ix){
  pb$tick()
  st_intersection(x = conus_cells_sf[ix,], y = conus_fed_type_union[intersections_fedtype[[ix]],])
})

hd_intersectFeatures_fedtype <- head(intersections_fedtype, n = xL)

intersectFeatures_fedtype_ar <- hd_intersectFeatures_fedtype %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (3000*3000))

intersectFeatures_fedtype_ar <- rasterize(intersectFeatures_fedtype_ar, conus_cells_rst, field = "percent_area")
plot(intersectFeatures_fedtype_ar)
writeRaster(intersectFeatures_fedtype_ar, here::here(paste0("data/processed/conus_fed_pctarea_", Sys.Date(), ".tif")))
#plot(st_geometry(counties_proj), add = TRUE)

#----Fed Richness-----

intersections_rich <- st_intersects(conus_cells_sf, conus_fed_proj_name)

pb2 <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(conus_cells_sf)[1])

intersectFeatures_rich <- map_dfr(1:dim(conus_cells_sf)[1], function(ix){
  pb2$tick()
  st_intersection(x = conus_cells_sf[ix,], y = conus_fed_proj_name[intersections_rich[[ix]],])
})

intersectFeatures_richness <- intersectFeatures_rich %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

intersectFeatures_rich_rst <- rasterize(intersectFeatures_richness, conus_cells_rst, field = "numfed")
plot(intersectFeatures_rich_rst)
writeRaster(intersectFeatures_rich_rst, here::here(paste0("data/processed/conus_fed_rich_", Sys.Date(), ".tif")))

