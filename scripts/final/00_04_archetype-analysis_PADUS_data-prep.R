library(tidyverse)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(tigris)
library(tmap)
library(raster)
library(units)


# 1. Download the PADUS geodatabase
## From this website https://www.sciencebase.gov/catalog/item/652ef930d34edd15305a9b03
## download the PADUS4_0Geodatabase.zip 
## The download requires one to pass a CAPTCHA challenge 

# 2. Load the data and filter for the contiguous US (CONUS)

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
conus_fedp <- st_transform(conus_fed, st_crs(counties))
identical(st_crs(conus_fedp), st_crs(counties))

## Make the multisurface into multipolygons
conus_fedp <- conus_fedp %>% st_cast("MULTIPOLYGON")

# Turn off using spherical geometry
sf_use_s2(FALSE)

## Check and fix validity
all(st_is_valid(conus_fedp))
conus_fedp_val <- st_make_valid(conus_fedp)
all(st_is_valid(conus_fedp_val))
#conus_fedp_val_buff <- st_buffer(conus_fedp_val, dist = 0)

## Check for and remove empty geometries
all(st_is_empty(conus_fedp_val))
#conus_fedp_val_noempty = conus_fedp_val[!st_is_empty(conus_fedp_val),]
#st_is_empty(conus_fedp_val_noempty)


#----
#4. Rasterize the data, may not need to do this until later? Save a shapefile instead?
## Load the reference raster from fire-data-prep
#ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))

## Set to the same projection
#conus_fed_val_noempty_p <- st_transform(conus_fedp_val_noempty, crs(ref_rast))

## Rasterize the data and write the raster
#fed_rast <- rasterize(vect(conus_fed_val_noempty_p), ref_rast, field = "Mang_Name")
#writeRaster(fed_rast, here::here("data/processed/fed_lands_3000m.tif"), overwrite = TRUE)
#----

# 5. Calculate the area of overlapping Fed and county polygons (maybe I didn't need to figure out the rasterization yet...)
## from stack overflow https://gis.stackexchange.com/questions/362466/calculate-percentage-overlap-of-2-sets-of-polygons-in-r

## Calculate area and tidy up
intersect_pct <- st_intersection(counties, conus_fedp_val) %>% 
  mutate(intersect_area = st_area(.)) %>% # create new column with shape area
  group_by(GEOID) %>% # group by GEOID
  summarise(intersect_area_sum = sum(intersect_area)) %>% # add the intersect areas
  dplyr::select(GEOID, intersect_area_sum) %>%   # only select columns needed to merge
  st_drop_geometry()  # drop geometry as we don't need it

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
#write_sf(obj = counties, dsn = paste0(here::here("data/processed/"), "county_fed_gov_coverage_pct", Sys.Date(), ".shp"), overwrite = TRUE, append = FALSE)
#print("new shapefile written")

# 6. Calculate the Shannon Diversity Index for Federal ownership by county

## Calculate area and tidy up
fed_intersect <- st_intersection(counties_sub, conus_fedp_val) %>% 
  mutate(fed_intersect = st_area(.)) %>% # create new column with shape area
  group_by(GEOID, Mang_Name) %>% # group by GEOID and the Fed agency name
  summarise(fed_inter_sum = sum(fed_intersect)) %>% # sum the intersected areas
  dplyr::select(GEOID, Mang_Name, fed_inter_sum, intersect_area_sum, coverage) %>%   # only select columns needed to merge
  st_drop_geometry()
  
# Create a fresh area variable for counties
counties_join <- mutate(counties, county_area = drop_units(st_area(counties)))

# Merge by county name
counties_join <- merge(counties_join, drop_units(fed_intersect), by = "GEOID", all.x = TRUE)
counties_join$fed_inter_sum[is.na(counties_join$fed_inter_sum)] <- 0

# Calculate Shannon Index and replace NA with 0
counties_prop <- counties_join %>% 
  mutate(., prop = fed_inter_sum/county_area,
         step1 = -prop * log(prop))
counties_prop$step1[is.na(counties_prop$step1)] <- 0

counties_fed_rich <- counties_prop %>%
  group_by(., GEOID) %>%
  summarise(., numfed = n())

counties_shannon <- counties_prop %>% 
  drop_na(Mang_Name) %>%
  group_by(., GEOID) %>% 
  summarise(., numfed = n(), 
            H = sum(step1),
            fedarea = unique(fed_inter_sum),
            footprint = unique(county_area),
            E = H/log((fedarea)))
counties_shannon$E <- ifelse(drop_units(counties_shannon$fedarea) == 0, 1, counties_shannon$E)

shannon_diversity <- counties_prop %>%
  group_by(., GEOID) %>%
  summarise(H = sum(step1))

shannon <- counties_prop %>%
  drop_na(Mang_Name) %>%
  group_by(., GEOID) %>%
  summarise(., numfed = n(), 
            H = sum(step1)) %>%
  dplyr::select(GEOID, numfed, H) %>%
  st_drop_geometry()

counties_shannon <- left_join(counties, shannon)

