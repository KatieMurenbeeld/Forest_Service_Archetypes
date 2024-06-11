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

# 1. Download the PADUS geodatabase
## From this website https://www.sciencebase.gov/catalog/item/652ef930d34edd15305a9b03
## download the PADUS4_0Geodatabase.zip 
## The download requires one to pass a CAPTCHA challenge 

# 2. Load the data and filter for the contiguous US (CONUS)
projection = "epsg:5070"


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

## Load the PADUS4_0Fee layer of the PADUS geodatabase and filter for by states

### Check the layers of the geodatabase, here choose the PADUS4_0Fee layer
#padus_layers <- st_layers(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"))

#fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"), layer = "PADUS4_0Fee")      

#conus_fed <- fed %>%
#  filter(State_Nm %in% continental.states$state) %>%
#  filter(Mang_Type == "FED")

### Once the data has been filtered, remove fed
#rm(fed)

# optional, write the shapefile to save a little time and memory 
#st_write(conus_fed, "~/Analysis/Archetype_Analysis/data/processed/conus_fed.shp")
# read in shapefile
conus_fed <- st_read(here::here("data/processed/conus_fed.shp"))

# 3. Set the projection and check for shape validity and empty geometries

conus_fedp <- conus_fed %>% st_transform(., crs = projection)

## Make the multisurface into multipolygons
conus_fedp <- conus_fedp %>% st_cast("MULTIPOLYGON")

## Turn off using spherical geometry
sf_use_s2(FALSE)

## Check for invalid geometries and fix
all(st_is_valid(conus_fedp))
conus_fedp_val <- st_make_valid(conus_fedp)
all(st_is_valid(conus_fedp_val))

## Check for and remove empty geometries
all(st_is_empty(conus_fedp_val))

# 4. Calculate the area of Federal fee lands
# Create grid cells as a shapefile for the bounding box and crs of the ref raster
# Then calculate the area of padus polygons overlapping the grid cells
# Then rasterize and replace NAs with 0 

ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
ref_rast_proj <- project(ref_rast, projection)
counties_proj <- counties %>% st_transform(., crs = projection)

#----test with idaho counties and 30kmx30km grid cell----
id_counties <- counties_proj %>%
  filter(STUSPS == "ID")

id_cells_sf <- st_make_grid(id_counties, cellsize = 30000, what = "polygons")
id_cells_sf <- st_sf(id_cells_sf) 

id_cells <- id_cells_sf %>% 
  dplyr::mutate(GRIDCELL_REFID = as.character(row_number())) %>%
  dplyr::mutate(test_area = 0)

## Create a template raster for the shapefiles
XMIN <- ext(id_cells)$xmin
XMAX <- ext(id_cells)$xmax
YMIN <- ext(id_cells)$ymin
YMAX <- ext(id_cells)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 30000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
id_cells_rst <- rast(ncol=NCOLS, nrow=NROWS, 
                     xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                     vals=1, crs=projection)
#----
conus_fed_name <- conus_fedp_val %>%
  dplyr::select(Mang_Nm)

conus_fed_type <- conus_fedp_val %>%
  dplyr::select(Mng_Typ)

conus_fed_name_proj <- conus_fed_name %>% st_transform(., crs = projection)
conus_fed_type_proj <- conus_fed_type %>% st_transform(., crs = projection)
#----
# for all Federal agencies need to union first otherwise I got >1.00 percent_area
id_fed_union <- conus_fed_type_proj %>%
  st_crop(., id_cells) %>%
  st_union(.)

plot(st_geometry(id_fed_union))

id_fed_uni_int <- st_intersection(id_cells, id_fed_union)
id_fed_uni_int <- id_fed_uni_int %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (30000*30000)) # add another field if percent area is NA return 0 else return percent area
id_fed_uni_rast <- rasterize(id_fed_uni_int, id_cells_rst, field = "percent_area")
plot(id_fed_uni_rast)
plot(st_geometry(id_cells), add = TRUE)
plot(st_geometry(id_counties), add = TRUE)

# make all NAs 0 in the raster first before the intersection
#id_fed_uni_rast

# for different Federal agencies
id_fed_name_int <- st_intersection(id_cells, conus_fed_name_proj)
#id_test_intersect <- st_intersects(id_cells, conus_fed_name_proj)
id_fed_name_int <- id_fed_name_int %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (30000*30000))

id_fed_all_areas <- id_fed_name_int %>% 
  #as_tibble() %>% 
  group_by(GRIDCELL_REFID, Mang_Name) %>% 
  summarize(area = sum(area)) %>%
  mutate(percent_area = drop_units(area) / (30000*30000))

id_test_blm <- id_fed_all_areas %>%
  filter(Mang_Name == "BLM")
id_test_usfs <- id_fed_all_areas %>%
  filter(Mang_Name == "USFS")
id_test_nps <- id_fed_all_areas %>%
  filter(Mang_Name == "NPS")

id_test_blm_rast <- rasterize(id_test_blm, id_cells_rst, field = "percent_area")
id_test_usfs_rast <- rasterize(id_test_usfs, id_cells_rst, field = "percent_area")
id_test_nps_rast <- rasterize(id_test_nps, id_cells_rst, field = "percent_area")
plot(id_test_blm_rast)
plot(st_geometry(id_counties), add=TRUE)
plot(id_test_usfs_rast)
plot(st_geometry(id_counties), add=TRUE)
plot(id_test_nps_rast)
plot(st_geometry(id_counties), add=TRUE)

# Calculate the Shannon Evenness
id_fed_all_areas_prop <- id_fed_all_areas %>% 
  mutate(step1 = -percent_area * log(percent_area))
#id_fed_all_areas_prop$step1[is.na(id_fed_all_areas_prop$step1)] <- 0

id_fed_rich <- id_fed_all_areas %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

id_shannon <- id_fed_all_areas_prop %>% 
  group_by(., GRIDCELL_REFID) %>% 
  summarise(., numfed = n(), 
            H = sum(step1),
            fedarea = sum(unique(drop_units(area))),
            E = H/log(fedarea))

id_shan_E_rast <- rasterize(id_shannon, id_cells_rst, field = "E")
id_shan_rich_rast <- rasterize(id_shannon, id_cells_rst, field = "numfed")

# don't use the evenness
plot(id_shan_E_rast)
plot(st_geometry(id_counties), add = TRUE)

# use the richness and here NAs = 0
plot(id_shan_rich_rast)
plot(st_geometry(id_counties), add = TRUE)

#----try for conus----
# create the sf of grid cells
conus_cells <- st_make_grid(counties_proj, cellsize = 3000)

# make into sf
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

#conus_cells_crop <- st_crop(conus_cells, conus_fed_name_proj)

# test doing intersects first
intersections <- st_intersects(x = conus_cells_sf, y = conus_fed_type_proj)
#intersections2 <- st_intersects(x = conus_fed_type_proj, y = conus_cells_sf)
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(conus_cells_sf)[1])

intersectFeatures <- map_dfr(1:dim(conus_cells_sf)[1], function(ix){
  pb$tick()
  st_intersection(x = conus_cells_sf[ix,], y = conus_fed_type_proj[intersections[[ix]],])
})

test_intersectFeatures <- head(intersectFeatures, n = 338773)
intersectFeatures_areas <- test_intersectFeatures %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (3000*3000))

test_intersectFeatures_rast <- rasterize(intersectFeatures_areas, conus_cells_rst, field = "percent_area")
plot(test_intersectFeatures_rast)
test_intersectFeatures_rast[is.na(test_intersectFeatures_rast)] <- 0
plot(test_intersectFeatures_rast)
writeRaster(test_intersectFeatures_rast, here::here("data/processed/test_pctarea_fed_conus_3km.tif"))

# test doing intersects first - the different Federal agencies (conus_fed_name_proj) for richness
intersections_fedname <- st_intersects(x = conus_cells_sf, y = conus_fed_name_proj)
#intersections2 <- st_intersects(x = conus_fed_type_proj, y = conus_cells_sf)
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(conus_cells_sf)[1])

intersectFeatures_fedname <- map_dfr(1:dim(conus_cells_sf)[1], function(ix){
  pb$tick()
  st_intersection(x = conus_cells_sf[ix,], y = conus_fed_name_proj[intersections_fedname[[ix]],])
})

conus_fed_rich <- intersectFeatures_fedname %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

# test with st_intersection(x, y) where y is the conus_cells as a larger sfc object
test_fed_type_int <- st_intersection(conus_fed_type_proj, conus_cells)
conus_fed_rich <- test_fed_type_int %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())
# Calculate the richness (number of Federal agencies on one gridcell)
rich.df <- data.frame(GRIDCELL_REFID = character(),
                      areafed = numeric())

#st_intersection(conus_fed_name_proj, conus_cells[1,])

for (i in 1:10){
  inx <- st_intersection(conus_cells[i,], conus_fed_type_proj)
  GRIDCELL_REFID <- conus_cells[i,]$GRIDCELL_REFID
  areafed <- st_area(inx)
  if(length(areafed) == 0){
    areafed <- 0
  } else{areafed <- areafed}
  rich.df[nrow(rich.df) + 1,] <- c(GRIDCELL_REFID,
                                   areafed)
  print(i)
}

conus_fed_name_int <- st_intersection(conus_cells, conus_fed_name_proj)
conus_fed_rich <- conus_fed_name_int %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

### do not run----------
#conus_fed_name_int <- conus_fed_name_int %>%
#  mutate(area = st_area(.)) %>%
#  mutate(percent_area = drop_units(area) / (3000*3000))

#conus_fed_all_areas <- conus_fed_name_int %>% 
#  group_by(GRIDCELL_REFID, Mang_Name) %>% 
#  summarize(area = sum(area)) %>%
#  mutate(percent_area = drop_units(area) / (3000*3000))
#-----
# for all Federal agencies need to union first otherwise I got >1.00 percent_area
conus_fed_union <- conus_fed_type_proj %>%
  st_crop(., conus_cells) %>%
  st_union(.)
saveRDS(conus_fed_union, here::here("data/processed/conus_fed_union.rds"))

# intersection for all Fed fee lands
###----for-loop----
ext.list <- list()
ext.list <- for (i in 1:nrow(conus_cells)){
  inx <- st_intersection(conus_fed_union, conus_cells[i])
  df <- data.frame(id = conus_cells[i, "GRIDCELL_REFID"],
                   areafed = st_area(inx),
                   pctfed = areafed/(cellSize^2))
  print(i)
}

conus_fed_uni_int <- st_intersection(conus_cells, conus_fed_union)
saveRDS(conus_fed_uni_int, here::here("data/processed/conus_fed_uni_int.rds"))
conus_fed_uni_int <- conus_fed_uni_int %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (3000*3000)) # add another field if percent area is NA return 0 else return percent area
conus_fed_uni_rast <- rasterize(conus_fed_uni_int, conus_cells_rst, field = "percent_area")
# then set nas to 0
plot(id_fed_uni_rast)
writeRaster(conus_fed_uni_rast, here::here("data/processed/fed_area_3km_conus_", Sys.Date(), ".tif"))
#plot(st_geometry(id_cells), add = TRUE)
#plot(st_geometry(id_counties), add = TRUE)

conus_rich <- rast("~/Analysis/NEPA_Efficiency/data/processed/conus_fed_rich.tif")
conus_rich
plot(conus_rich)
conus_rich[is.na(conus_rich)] <-0
plot(conus_rich)
