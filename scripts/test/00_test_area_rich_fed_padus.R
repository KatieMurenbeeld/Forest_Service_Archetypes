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

nc_cells <- st_make_grid(nc_proj, cellsize = 30000)
nc_cells_sf <- st_sf(nc_cells) 
# add unique cell id
nc_cells_sf <- nc_cells_sf %>% 
  mutate(GRIDCELL_REFID = as.character(row_number()))

plot(st_geometry(nc_proj))
plot(st_geometry(nc_cells_sf), add = TRUE)
#plot(st_geometry(fed_proj_type), add = TRUE)

fed_type_union <- fed_proj_type %>%
  st_union(.)

## Test the cells intersecting the counties
nc_int <- st_intersection(nc_cells_sf, nc_proj)
nc_int_ar <- nc_int %>%
  mutate(area = st_area(.)) %>%
  mutate(pctarea = area / (30000 * 30000))

## Create a template raster for the shapefiles
XMIN <- ext(nc_cells_sf)$xmin
XMAX <- ext(nc_cells_sf)$xmax
YMIN <- ext(nc_cells_sf)$ymin
YMAX <- ext(nc_cells_sf)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 30000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
nc_cells_rst <- rast(ncol=NCOLS, nrow=NROWS, 
                        xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                        vals=1, crs=projection)

nc_int_ar <- rasterize(nc_int_ar, nc_cells_rst)
plot(nc_int_ar)

nc_fed_type_int <- st_intersection(nc_cells_sf, fed_type_union)
nc_fed_type_int_ar <- nc_fed_type_int %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (30000*30000))

nc_fedtype_int_ar <- rasterize(nc_fed_type_int_ar, nc_cells_rst)
plot(nc_fedtype_int_ar)
plot(st_geometry(nc_proj), add = TRUE)

intersections <- st_intersects(nc_cells_sf, fed_type_union)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(nc_cells_sf)[1])

intersectFeatures <- map_dfr(1:dim(nc_cells_sf)[1], function(ix){
  pb$tick()
  st_intersection(x = nc_cells_sf[ix,], y = fed_type_union[intersections[[ix]],])
})

test_intersectFeatures <- head(intersectFeatures, n = 79L)

intersectFeatures_ar <- test_intersectFeatures %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (30000*30000))

intersectFeatures_ar_rst <- rasterize(intersectFeatures_ar, nc_cells_rst)
plot(intersectFeatures_ar_rst)
plot(st_geometry(nc_proj), add = TRUE)

identical(intersectFeatures_ar_rst, nc_fedtype_int_ar)


class(intersectFeatures)
class(nc_fed_type_int)

head_util <- utils::head(intersectFeatures)
head_terra <- terra::head(intersectFeatures)
head_raster <- raster::head(intersectFeatures)


test <- intersectFeatures %>%
  rowwise() %>%
  mutate(geometry = nc_cells) %>%
  st_as_sf(sf_column_name = "geometry")

intersectFeatures$nc_cells <- unlist(intersectFeatures$nc_cells)
