library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(tigris)
library(readxl)

## Load the csv files
forest_depend <- read_csv(paste0(here::here("data/original/Data/County-Forest_Dep_Comm_Capital.csv")))
del_pop <- read_csv(paste0(here::here("data/original/population_estimates_2022.csv")))
econ_bea <- read.csv(paste0(here::here("data/original/CAINC6N__ALL_AREAS_2001_2022.csv")))
bric <- read_excel(paste0(here::here("data/original/bric2020_us.xlsx")))

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
  dplyr::select(GEOID, geometry)

# Create one large table with FIPS codes and desired variables
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

# Economic Data
econ_bea$Description <- trimws(econ_bea$Description)
econ_bea$X2022 <- as.numeric(econ_bea$X2022)
linecodes <- as.character(c(1, 81, 100, 200, 300, 400, 500, 600, 700, 800, 807, 900, 1000,
               1100, 1200, 1300, 1500, 1600, 1700, 1800, 1900, 2000, 2010))

col_names <- unique(econ_bea$Description)

econ_comp <- econ_bea %>% 
  dplyr::select(GeoFIPS, LineCode, Description, X2022) %>%
  filter(LineCode %in% linecodes) %>%
  spread(Description, X2022, fill = 0) %>%
  dplyr::select(-LineCode) %>%
  rename("FIPS" = "GeoFIPS") %>%
  group_by(FIPS) %>%
  summarise_all(sum)

names <- c("acc_food", "art_rec", "emp_comp", "constr", "edu", "farm", "fin_ins",
     "forest", "gov", "health", "info", "mngmnt", "manuf", "mining", "other",
     "pro_scit", "real_est", "retail", "sightsee", "st_loc", "tran_war",
     "utils", "ws_trade")
colnames(econ_comp)[2:24] <- names

econ_compercent <- cbind(econ_comp, econ_comp[names]/econ_comp$emp_comp)

names2 <- c("acc_food_p", "art_rec_p", "emp_comp_p", "constr_p", "edu_p", "farm_p", "fin_ins_p",
           "forest_p", "gov_p", "health_p", "info_p", "mngmnt_p", "manuf_p", "mining_p", "other_p",
           "pro_scit_p", "real_est_p", "retail_p", "sightsee_p", "st_loc_p", "tran_war_p",
           "utils_p", "ws_trade_p")

colnames(econ_compercent)[25:47] <- names2

## Make sure all FIPS codes are padded with 0s for a total of 5 characters
update_fips <- function(data_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}

fordep_fips <- update_fips(fordep)
delpop_fips <- update_fips(delpop)
econ_compercent_fips <- update_fips(econ_compercent) # need to adjust column names
econ_compercent_fips <- econ_compercent_fips %>% 
  mutate(FIPS= trimws(as.character(FIPS)))
bric_2020_fips <- update_fips(bric_2020)

all_vars <- plyr::join_all(list(fordep_fips, delpop_fips, econ_compercent_fips, bric_2020_fips),
                     by='FIPS', 
                     type='left')

# Join to counties

var_bdry <- left_join(all_vars, counties,
                    by = c("FIPS" = "GEOID"))

var_bdry <- st_as_sf(var_bdry)

## Check and fix validity
all(st_is_valid(var_bdry))

## Check for empty geometries and invalid or corrupt geometries 
any(st_is_empty(var_bdry))
var_bdry_noempty <- var_bdry[!st_is_empty(var_bdry),]
any(st_is_empty(var_bdry_noempty))
any(is.na(st_is_valid(var_bdry_noempty)))
#any(na.omit(st_is_valid(var_bdry_noempty)) == FALSE)
st_is_longlat(var_bdry_noempty)

## Save the validated shapefile
write_sf(obj = var_bdry, dsn = paste0(here::here("data/processed/"), "all_vars_to_rst_", Sys.Date(), ".shp"), overwrite = TRUE, append = FALSE)
print("new shapefile written")
