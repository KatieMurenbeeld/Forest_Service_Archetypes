###@article{stanke2020rfia,
#title={rFIA: An R package for estimation of forest attributes with the US Forest Inventory and Analysis database},
#author={Stanke, Hunter and Finley, Andrew O and Weed, Aaron S and Walters, Brian F and Domke, Grant M},
#journal={Environmental Modelling \& Software},
#volume={127},
#pages={104664},
#year={2020},
#publisher={Elsevier}
#}

library(rFIA)
library(tidyverse) 
library(tigris)
library(sf)
library(sp)
library(terra)
library(raster)
library(stringr)
library(ggplot2)

options(timeout=6000)

## See code from https://rfia.netlify.app/tutorial/bigdata/

## Download data for Idaho
id_fia <- getFIA(states = "ID", dir = here::here("data/original/"))
countiesID <- tigris::counties(state = "ID")

## Now set up a Remote.FIA.Database with readFIA by setting inMemory = FALSE
#fia <- readFIA( here::here("data/original/"), inMemory = FALSE)

# Get most recent data using clipFIA
idMR <- clipFIA(id_fia, mostRecent = TRUE)

# Can group tpa (tree per acre) estimates by areal units (see https://rfia.netlify.app/tutorial/spatial/)

tpa_sf <- tpa(idMR, polys = countiesID, returnSpatial = TRUE)
plot(tpa_sf$BAA)

## Plot distribution of Basal area/ acre across space
plotFIA(tpa_sf, y = BAA, legend.title = 'BAA (sq.ft/acre)')

# Stand age and productivity
prod_codes <- c()

stdage <- idMR$COND$STDAGE
productivity <- idMR$COND$SITECLCD

id_age_prod <- dplyr::select(idMR$COND, COUNTYCD, STDAGE, SITECLCD)

id_age_prod <- id_age_prod %>%
  mutate(siteprod = case_when(
    SITECLCD == 1 ~ 225,
    SITECLCD == 2 ~ 194.5,
    SITECLCD == 3 ~ 142,
    SITECLCD == 4 ~ 102, 
    SITECLCD == 5 ~ 67,
    SITECLCD == 6 ~ 34.5, 
    SITECLCD == 7 ~ 9.5
  ))

id_age_prod_grp <- id_age_prod %>%
  group_by(COUNTYCD) %>%
  summarise(mean_stdage = mean(as.numeric(STDAGE), na.rm= TRUE),
            max_prodcd = max(as.numeric(SITECLCD), na.rm= TRUE), 
            min_prodcd = min(as.numeric(SITECLCD), na.rm= TRUE),
            mean_prod = mean(as.numeric(siteprod), na.rm = TRUE))

id_age_prod_grp$COUNTYFP <- str_pad(id_age_prod_grp$COUNTYCD, 3, pad = "0")

# join to county data
id_age_prod_sf <- st_as_sf(left_join(id_age_prod_grp, countiesID, by = "COUNTYFP"))


ggplot() +
  geom_sf(data = id_age_prod_sf, mapping = aes(color = mean_stdage, fill = mean_stdage))
ggplot() +
  geom_sf(data = id_age_prod_sf, mapping = aes(color = max_prodcd, fill = max_prodcd))
ggplot() +
  geom_sf(data = id_age_prod_sf, mapping = aes(color = min_prodcd, fill = min_prodcd))
ggplot() +
  geom_sf(data = id_age_prod_sf, mapping = aes(color = mean_prod, fill = mean_prod))

## scale up for CONUS

## Load county boundaries from tigris
counties <- tigris::counties()
##Get Continental US list
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
  filter(STATEFP %in% us.states$FIPS) %>%
  dplyr::select(GEOID, COUNTYFP, STATEFP, geometry)

states_list <- continental.states$state

## See code from https://rfia.netlify.app/tutorial/bigdata/
## Download data for all states, only need the COND table
for (s in states_list){
  getFIA(s, dir = here::here("data/original/fia/"), tables = "COND", load = TRUE)
}

## Now set up a Remote.FIA.Database with readFIA by setting inMemory = FALSE
## Instead of reading in the data now, readFIA will simply save a pointer
## and allow the estimator functions to read/process the data state-by-state
fia <- readFIA(here::here("data/original/fia/"), tables = "COND", inMemory = TRUE)
#fiaMR <- clipFIA(fia, mostRecent = TRUE)

# from the COND table select the STATECD, COUNTYCD, STDAGE, and SITECLCD
conus_age_prod <- dplyr::select(fia$COND, STATECD, COUNTYCD, STDAGE, SITECLCD)

# create a GEOID of the of the county and state codes (make the FIPS code) to 
# easily join to counties and to better group and summarise
conus_age_prod <- conus_age_prod %>%
  mutate(GEOID = paste0(str_pad(as.character(STATECD), 2, pad = "0"), str_pad(COUNTYCD, 3, pad = "0")))

# put the SITECD (productivity code) into a real number code 1 = 225 cuf/ac/yr, 
# but every other code is the ((max - min)/2) + min 
# see https://www.fs.usda.gov/rm/pubs/rmrs_gtr245.pdf 
# USDA Forest Service Gen. Tech. Rep. RMRS-GTR-245. 2010 pg 53 

conus_age_prod <- conus_age_prod %>%
  mutate(siteprod = case_when(
    SITECLCD == 1 ~ 225,
    SITECLCD == 2 ~ 194.5,
    SITECLCD == 3 ~ 142,
    SITECLCD == 4 ~ 102, 
    SITECLCD == 5 ~ 67,
    SITECLCD == 6 ~ 34.5, 
    SITECLCD == 7 ~ 9.5
  ))

# group the data by GEOID (FIPS code) and summarise
# there will be warning for rows with no data
conus_age_prod_grp <- conus_age_prod %>%
  group_by(GEOID) %>%
  summarise(mean_stdage = mean(as.numeric(STDAGE), na.rm= TRUE),
            max_prodcd = max(as.numeric(SITECLCD), na.rm= TRUE), 
            min_prodcd = min(as.numeric(SITECLCD), na.rm= TRUE),
            mean_prod = mean(as.numeric(siteprod), na.rm = TRUE))

# join to the county geometries and make it an sf
conus_age_prod_sf <- st_as_sf(left_join(conus_age_prod_grp, counties, by = "GEOID"))

## Check and fix validity
all(st_is_valid(conus_age_prod_sf))

## Check for empty geometries and invalid or corrupt geometries 
any(st_is_empty(conus_age_prod_sf))
conus_age_prod_sf_noempty <- conus_age_prod_sf[!st_is_empty(conus_age_prod_sf),]
any(st_is_empty(conus_age_prod_sf_noempty))
any(is.na(st_is_valid(conus_age_prod_sf_noempty)))
#any(na.omit(st_is_valid(var_bdry_noempty)) == FALSE)
st_is_longlat(conus_age_prod_sf_noempty)

## Save the validated shapefile
write_sf(obj = conus_age_prod_sf_noempty, dsn = here::here("data/processed/conus_age_prod_fia.shp"), overwrite = TRUE, append = FALSE)
print("new shapefile written")




