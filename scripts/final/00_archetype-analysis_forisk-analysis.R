library(tidyverse)
library(terra)
library(sf)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)
library(tmap)
library(patchwork)
library(units)
library(spdep)
library(spatstat)

#----Distance to mills and Mill Capacity "hotspots"
# Pseudocode:
# 1. Load the data (FORISK and county boundaries)
# 2. Align the data
# 3. Rasterize data
# 4. Aggregate to 1.5km and 3km
# 5. Calculate distance from mill points
# 6. Mill capacity metric

# 1. Load the data
mill_sf <- read_sf(here::here("data/original/2024_Q1_Forisk_North_American_Ind_Cap_DB_Shape/Forisk_NA_FI_Capacity_DB_2024_Q1.shp"))

##Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

counties <- tigris::counties(state = continental.states$state, cb = TRUE)

conus_mills <- mill_sf %>%
  filter(Region != "Canada West") %>%
  filter(Region != "Canada East") %>%
  filter(State_Prov %in% continental.states$state) #%>% # need to keep all mills in order to calculate change in mill capacity for mills that have closed
  #filter(Status == "Open")

# What is the distribution of total wood capacity?

hist(conus_mills$Total_Wood, main = "Distribution of Total Wood Capacity", xlab = "Total Wood Capacity", ylab = "Frequency")

# 2. Align the data
st_crs(conus_mills)
st_crs(counties)

identical(st_crs(conus_mills), st_crs(counties))

# Create a mill count variable for each county
counties$mill_count <- lengths(st_intersects(counties, conus_mills))

# Calculate the change in mill capacity from 2019-2023 (5 years)
## For mills that came "online" set the CPT_2019 to 1.0

conus_mills$CPT_2019[conus_mills$CPT_2019 == 0.0] <- 1.0 

conus_mills <- conus_mills %>%
  mutate(millcap_5yr = (CPT_2023 - CPT_2019) / CPT_2019)

### Make categorical. Could also make this only 3 categories (decrease, no change, increase in order to protect identities)
conus_mills <- conus_mills %>%
  mutate(change_millcap = case_when(millcap_5yr == -1.0 ~ "mill closed", 
                                    millcap_5yr > -1.0 & millcap_5yr < 0.0 ~ "decrease",
                                    millcap_5yr == 0.0 ~ "no change",
                                    millcap_5yr > 0.0 & CPT_2019 != 1.0 ~ "increase",
                                    millcap_5yr > 0.0 & CPT_2019 == 1.0 ~"new mill"))

# Check for missing data
print ("Row and Col positions of NA values") 
which(is.na(conus_mills$millcap_5yr), arr.ind = TRUE)

## Interpolate del mill capacity
nodes <- st_make_grid(counties,
                      n = c(50,50),
                      what = "centers")
dist <- distance(vect(nodes), vect(conus_mills))
nearest_conus <- apply(dist, 1, function(x) which(x == min(x)))
nearest_conus1 <- as.data.frame(nearest_conus[1:2500])
nearest_conus2 <- nearest_conus1 %>% filter(row_number() == 2)
#nearest_conus3 <- as.array(nearest_conus2)
nearest <- as.numeric(as.vector(nearest_conus2[1,]))
millcap5.nn <- conus_mills$millcap_5yr[nearest]
currcap.nn <- conus_mills$Current_Ca[nearest]
totwood.nn <- conus_mills$Total_Wood[nearest]
preds <- st_as_sf(nodes)
preds$millcap5 <- millcap5.nn
preds$currcap <- currcap.nn
preds$totwood <- totwood.nn
preds <- as(preds, "Spatial")
sp::gridded(preds) <- TRUE
preds.rast <- rast(preds)

mc5sf05 <- gstat(id = "millcap_5yr", formula = millcap_5yr~1, data=conus_mills,  nmax=7, set=list(idp = 0.5))
mc5sf1 <- gstat(id = "millcap_5yr", formula = millcap_5yr~1, data=conus_mills,  nmax=7, set=list(idp = 1))
mc5sf2 <- gstat(id = "millcap_5yr", formula = millcap_5yr~1, data=conus_mills,  nmax=7, set=list(idp = 2))

interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

zmc5sf05 <- interpolate(preds.rast, mc5sf05, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)
zmc5sf1 <- interpolate(preds.rast, mc5sf1, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)
zmc5sf2 <- interpolate(preds.rast, mc5sf2, debug.level=0, fun=interpolate_gstat, crs=crs(preds.rast), index=1)

# Crop the mill capacity change predictions to the counties data. These are rasters.
zmc5sf05_crop <- crop(zmc5sf05, counties, mask = TRUE) 
zmc5sf1_crop <- crop(zmc5sf1, counties, mask = TRUE)
zmc5sf2_crop <- crop(zmc5sf2, counties, mask = TRUE)

# 3. Rasterize the data using the raster created in the 00_archetype-analysis_download-prep-fire.R

ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif")) 
mill_proj <- conus_mills %>% st_transform(., crs = crs(ref_rast))
millchange_rast <- rasterize(vect(mill_proj), ref_rast, field = "change_millcap")

# 3.5 Write rasters

writeRaster(millchange_rast, here::here("data/processed/millchange_cap.tif"), overwrite = TRUE)
writeRaster(zmc5sf05_crop, here::here("data/processed/millchangecap_interp-05.tif"), overwrite = TRUE)
writeRaster(zmc5sf1_crop, here::here("data/processed/millchangecap_interp-1.tif"), overwrite = TRUE)
writeRaster(zmc5sf2_crop, here::here("data/processed/millchangecap_interp-2.tif"), overwrite = TRUE)


