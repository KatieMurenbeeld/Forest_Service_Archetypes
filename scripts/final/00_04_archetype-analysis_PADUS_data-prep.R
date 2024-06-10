library(tidyverse)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(tigris)
library(tmap)
library(raster)
library(units)
#library(plyr)


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
padus_layers <- st_layers(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"))

fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"), layer = "PADUS4_0Fee")      

conus_fed <- fed %>%
  filter(State_Nm %in% continental.states$state) %>%
  filter(Mang_Type == "FED")

### Once the data has been filtered, remove fed
rm(fed)

# 3. Set the projection and check for shape validity and empty geometries

#conus_fedp <- st_transform(conus_fed, st_crs(counties))
conus_fedp <- conus_fed %>% st_transform(., crs = projection)
#identical(st_crs(conus_fedp), crs(ref_rast))
#identical(st_crs(conus_fedp), st_crs(counties))

## Make the multisurface into multipolygons
conus_fedp <- conus_fedp %>% st_cast("MULTIPOLYGON")
#conus_fed <- conus_fed %>% st_cast("MULTIPOLYGON")

## Turn off using spherical geometry
sf_use_s2(FALSE)

## Check for invalid geometries and fix
all(st_is_valid(conus_fedp))
conus_fedp_val <- st_make_valid(conus_fedp)
all(st_is_valid(conus_fedp_val))
#conus_fedp_val_buff <- st_buffer(conus_fedp_val, dist = 0)

## Check for and remove empty geometries
all(st_is_empty(conus_fedp_val))
#conus_fedp_val_noempty = conus_fedp_val[!st_is_empty(conus_fedp_val),]
#st_is_empty(conus_fedp_val_noempty)

# 4. Calculate the area of overlapping Fed and county polygons (maybe I didn't need to figure out the rasterization yet...)----
## from stack overflow https://gis.stackexchange.com/questions/362466/calculate-percentage-overlap-of-2-sets-of-polygons-in-r

## Calculate area and tidy up
#intersect_pct <- st_intersection(counties, conus_fedp_val) %>% 
#  mutate(intersect_area = st_area(.)) %>% # create new column with shape area
#  group_by(GEOID) %>% # group by GEOID
#  summarise(intersect_area_sum = sum(intersect_area)) %>% # add the intersect areas
#  dplyr::select(GEOID, intersect_area_sum) %>%   # only select columns needed to merge
#  st_drop_geometry()  # drop geometry as we don't need it

# Create a fresh area variable for counties
counties <- mutate(counties, county_area = st_area(counties))

# Merge by county FIPS (GEOID)
counties <- merge(counties, intersect_pct, by = "GEOID", all.x = TRUE)

# Calculate coverage and replace NA with 0
counties <- counties %>% 
  mutate(coverage = as.numeric(intersect_area_sum/county_area))
counties$coverage[is.na(counties$coverage)] <- 0

counties_sub <- counties %>%
  dplyr::select(GEOID, intersect_area_sum, coverage)

## save as a shapefile
write_sf(obj = counties, dsn = paste0(here::here("data/processed/"), "county_fed_gov_coverage_pct", Sys.Date(), ".shp"), overwrite = TRUE, append = FALSE)
print("new shapefile written")
#----
# 4.1 Repeat but I want 3km cells and the % of the cell covered by government agency
# Create grid cells as a shapefile for the bounding box and crs of the ref raster
# Then calculate the area of padus polygons overlapping the grid cells
# Could I create an empty raster and use extract? Then rasterize? 

#counties_p <- counties %>% st_transform(., crs = crs(ref_rast))
#----testing % area fed and % area fed type----
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

conus_fed_name <- conus_fedp_val %>%
  dplyr::select(Mang_Name)

conus_fed_type <- conus_fedp_val %>%
  dplyr::select(Mang_Type)

conus_fed_name_proj <- conus_fed_name %>% st_transform(., crs = "epsg:5070") #%>%
  #st_cast(., to = "POLYGON")
conus_fed_type_proj <- conus_fed_type %>% st_transform(., crs = "epsg:5070") #%>%
  #st_cast(., to = "POLYGON")

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

ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
ref_rast_proj <- project(ref_rast, projection)
counties_proj <- counties %>% st_transform(., crs = projection)

conus_cells <- st_make_grid(counties_proj, cellsize = 3000)

# check the maps
#plot(conus_cells)
#plot(st_geometry(counties_proj), add = TRUE)

# make into sf
conus_cells <- st_sf(conus_cells) 

# add unique cell id
conus_cells <- conus_cells %>% 
  mutate(GRIDCELL_REFID = as.character(row_number()))

## Create a template raster for the shapefiles
XMIN <- ext(conus_cells)$xmin
XMAX <- ext(conus_cells)$xmax
YMIN <- ext(conus_cells)$ymin
YMAX <- ext(conus_cells)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
conus_cells_rst <- rast(ncol=NCOLS, nrow=NROWS, 
                     xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                     vals=1, crs=projection)

conus_fed_name <- conus_fedp_val %>%
  dplyr::select(Mang_Name)

conus_fed_type <- conus_fedp_val %>%
  dplyr::select(Mang_Type)

conus_fed_name_proj <- conus_fed_name %>% st_transform(., crs = "epsg:5070") #%>%
#st_cast(., to = "POLYGON")
conus_fed_type_proj <- conus_fed_type %>% st_transform(., crs = "epsg:5070") #%>%
#st_cast(., to = "POLYGON")

# for all Federal agencies need to union first otherwise I got >1.00 percent_area
conus_fed_union <- conus_fed_type_proj %>%
  st_crop(., conus_cells) %>%
  st_union(.)

saveRDS(conus_fed_union, here::here("data/processed/conus_fed_union.rds"))

# intersection for all Fed fee lands
#d[is.na(d)] <- 0
conus_fed_union[is.na(conus_fed_union)] <- 0
saveRDS(conus_fed_union, here::here("data/processed/conus_fed_union_na_0.rds"))

conus_fed_uni_int <- st_intersection(conus_cells, conus_fed_union)
saveRDS(conus_fed_uni_int, here::here("data/processed/conus_fed_uni_int.rds"))
conus_fed_uni_int <- conus_fed_uni_int %>%
  mutate(area = st_area(.)) %>%
  mutate(percent_area = drop_units(area) / (3000*3000)) # add another field if percent area is NA return 0 else return percent area
conus_fed_uni_rast <- rasterize(conus_fed_uni_int, conus_cells_rst, field = "percent_area")
plot(id_fed_uni_rast)
writeRaster(conus_fed_uni_rast, here::here("data/processed/fed_area_3km_conus_", Sys.Date(), ".tif"))
#plot(st_geometry(id_cells), add = TRUE)
#plot(st_geometry(id_counties), add = TRUE)

