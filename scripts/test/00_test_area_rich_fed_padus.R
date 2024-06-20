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

# create the list of CONUS states 
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

## Download the county boundaries and filter by states
counties <- tigris::counties(state = continental.states$state, cb = TRUE)

fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"), layer = "PADUS4_0Fee") 

conus_fed <- fed %>%
  filter(State_Nm %in% continental.states$state) %>%
  filter(Mang_Type == "FED")

rm(fed)

# reproject
counties_proj <- st_transform(counties, crs = projection)

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

# clean up padus data
sf_use_s2(FALSE)

conus_fed_proj <- conus_fed %>% st_transform(., crs = projection)

## Make the multisurface into multipolygons
conus_fed_proj <- conus_fed_proj %>% st_cast("MULTIPOLYGON")

## Check for invalid geometries and fix
all(st_is_valid(conus_fed_proj))
conus_fedp_val <- st_make_valid(conus_fed_proj)
all(st_is_valid(conus_fedp_val))

## Check for and remove empty geometries
all(st_is_empty(conus_fedp_val))

conus_fed_proj_type <- conus_fedp_val %>%
  dplyr::select(Mang_Type) # for % area Fed

conus_fed_proj_name <- conus_fedp_val %>%
  dplyr::select(Mang_Name) # for Fed richness

# Calculate the % area of federal land
conus_fed_type_union <- conus_fed_proj_type %>%
  st_union(.)

#saveRDS(conus_fed_type_union, here::here("data/processed/conus_fed_type_union.RDS")) # save in case you need to start over from here

intersections_fedtype <- st_intersects(conus_cells_sf, conus_fed_type_union)
saveRDS(intersections_fedtype, here::here("data/processed/intersections_fedtype.RDS")) # save in case you need to start over from here

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(conus_cells_sf)[1])

intersectFeatures_fedtype <- map_dfr(1:dim(conus_cells_sf)[1], function(ix){
  pb$tick()
  st_intersection(x = conus_cells_sf[ix,], y = conus_fed_type_union[intersections_fedtype[[ix]],])
})
saveRDS(intersectFeatures_fedtype, here::here("data/processed/intersectFeatures_fedtype.RDS")) # save in case you need to start over from here

hd_intersectFeatures_fedtype <- head(intersectFeatures_fedtype, n = 278203L)

intersectFeatures_fedtype_ar <- hd_intersectFeatures_fedtype %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (3000*3000))

intersectFeatures_fedtype_ar <- rasterize(intersectFeatures_fedtype_ar, conus_cells_rst, field = "percent_area")
plot(intersectFeatures_fedtype_ar)
writeRaster(intersectFeatures_fedtype_ar, here::here(paste0("data/processed/conus_fed_pctarea_", Sys.Date(), ".tif")))
#plot(st_geometry(counties_proj), add = TRUE)

#----Fed Richness-----

intersections_rich <- st_intersects(conus_cells_sf, conus_fed_proj_name)
saveRDS(intersections_rich, here::here("data/processed/intersections_rich.RDS")) # save in case you need to start over from here

pb2 <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(conus_cells_sf)[1])

intersectFeatures_rich <- map_dfr(1:dim(conus_cells_sf)[1], function(ix){
  pb2$tick()
  st_intersection(x = conus_cells_sf[ix,], y = conus_fed_proj_name[intersections_rich[[ix]],])
})

saveRDS(intersectFeatures_rich, here::here("data/processed/intersectFeatures_rich.RDS"))

intersectFeatures_richness <- intersectFeatures_rich %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

intersectFeatures_rich_rst <- rasterize(intersectFeatures_richness, conus_cells_rst, field = "numfed")
writeRaster(intersectFeatures_rich_rst, here::here(paste0("data/processed/conus_fed_rich_", Sys.Date(), ".tif")))
plot(intersectFeatures_rich_rst)

