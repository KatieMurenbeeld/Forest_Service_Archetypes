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
stdage <- idMR$COND$STDAGE
productivity <- idMR$COND$SITECLCD

id_age_prod <- dplyr::select(idMR$COND, COUNTYCD, STDAGE, SITECLCD)
id_age_prod_grp <- id_age_prod %>%
  group_by(COUNTYCD) %>%
  summarise(mean_stdage = mean(as.numeric(STDAGE), na.rm=TRUE),
            max_prod = max(as.numeric(SITECLCD), na.rm=TRUE), 
            min_prod = min(as.numeric(SITECLCD), na.rm=TRUE))

id_age_prod_grp$COUNTYFP <- str_pad(id_age_prod_grp$COUNTYCD, 3, pad = "0")

# join to county data
id_age_prod_sf <- st_as_sf(left_join(id_age_prod_grp, countiesID, by = "COUNTYFP"))


ggplot() +
  geom_sf(data = id_age_prod_sf, mapping = aes(color = mean_stdage, fill = mean_stdage))
ggplot() +
  geom_sf(data = id_age_prod_sf, mapping = aes(color = max_prod, fill = max_prod))
ggplot() +
  geom_sf(data = id_age_prod_sf, mapping = aes(color = min_prod, fill = min_prod))





