library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(tigris)
library(readxl)

# Steps:
# 0. Load the libraries
# 1. Load the csv files 
# 2. Load the counties data and select ID, MT, WA, and OR
# 3. Create a large data frame of the variables/attributes and FIPS codes
# 4. Join the attribute dataframe and counties
# 5. Check and fix validity, empty geometries, etc.
# 6. Save the shapefile

# 1. Load the csv files
forest_depend <- read_csv(paste0(here::here("data/toy/original/Data/County-Forest_Dep_Comm_Capital.csv")))
del_pop <- read_csv(paste0(here::here("data/toy/original/population_estimates_2022.csv")))
bric <- read_excel(paste0(here::here("data/toy/original/bric2020_us.xlsx")))

# 2. Load county boundaries from tigris and select ID, MT, WA, and OR
counties <- tigris::counties()
##Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
select.states <- us.states[us.states$state == "ID" | us.states$state == "MT" | us.states$state == "WA" | us.states$state == "OR",] #only CONUS

counties <- tigris::counties()
counties <- counties %>%
  filter(STATEFP %in% select.states$FIPS) %>%
  dplyr::select(GEOID, geometry)

# 3. Create one large table with FIPS codes and desired variables
## For each data set, select the variables and rename the column with the FIPS codes if needed
## BRIC data
bric_2020 <- bric %>%
  dplyr::select(-"State Code (FIPS)", -"County Code (FIPS)", -"County") %>%
  rename("FIPS" = "GEOID")

## Change in Population
delpop <- del_pop %>%
  filter(Attribute == "R_NET_MIG_2021") %>%
  dplyr::select(FIPStxt, Value) %>%
  rename("FIPS" = "FIPStxt", "R_NET_MIG_2021" = "Value")

## Current Population
pop_2020 <- del_pop %>%
  filter(Attribute == "CENSUS_2020_POP") %>%
  dplyr::select(FIPStxt, Value) %>%
  rename("FIPS" = "FIPStxt", "CENSUS_2020_POP" = "Value") 

## Forest Dependence Variables
fordep <- forest_depend %>%
  dplyr::select(fips, forest.area, pct.pay, pct.forest, forest.dependent) %>%
  rename("FIPS" = "fips")

## Make sure all FIPS codes are padded with 0s for a total of 5 characters
## Function to pad FIPS codes with 0s
update_fips <- function(data_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}

fordep_fips <- update_fips(fordep)
delpop_fips <- update_fips(delpop)
bric_2020_fips <- update_fips(bric_2020)

## Join all of the variables
all_vars <- plyr::join_all(list(fordep_fips, delpop_fips, bric_2020_fips),
                           by='FIPS', 
                           type='left')

# 4. Join the variables to the counties

var_bdry <- left_join(all_vars, counties,
                      by = c("FIPS" = "GEOID"))

## Make into an sf object
var_bdry <- st_as_sf(var_bdry)

# 5. Check and fix validity, empty geometries, etc.
## Check and fix validity
all(st_is_valid(var_bdry))

## Check for empty geometries and invalid or corrupt geometries 
any(st_is_empty(var_bdry))
var_bdry_noempty <- var_bdry[!st_is_empty(var_bdry),]
any(st_is_empty(var_bdry_noempty))
any(is.na(st_is_valid(var_bdry_noempty)))
#any(na.omit(st_is_valid(var_bdry_noempty)) == FALSE)
st_is_longlat(var_bdry_noempty)

# 6. Save the validated shapefile
write_sf(obj = var_bdry, dsn = paste0(here::here("data/toy/processed/"), "all_vars_to_rst_", Sys.Date(), ".shp"), overwrite = TRUE, append = FALSE)
print("new shapefile written from csv files ")
