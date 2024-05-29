library(elevatr)
library(tigris)
library(terra)
library(sf)

# Some files are large so set the timeout to 10 minutes (6000 seconds)
options(timeout=6000)

# 1. Load the counties data from tigris
## This will act as the bounding box

##Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

counties <- tigris::counties(state = continental.states$state, cb = TRUE)

# 2. Use get_elev_raster from the elevatr package
## This returns a RasterLayer, but this functionality will be dropped in future versions

elevation <- get_elev_raster(counties, z = 5)

ele2 <- rast(here::here("data/original/_ags_b3860be0_3b01_41fb_ba0d_06d68b293983.tif"))

## Convert to a SpatRaster

ele_rast <- as(elevation, "SpatRaster")

# 3. Aggregate to 1.5km and 3km

# 4. Calculate a topographic index using terra terrain 

tri <- terrain(ele_rast, "tri", unit = "meters")
