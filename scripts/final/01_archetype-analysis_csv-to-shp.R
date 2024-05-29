library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(tigris)
library(readxl)

## Load the csv files
elect_cntx <- read_csv(paste0(here::here("data/original/election_context_2018.csv")))
forest_depend <- read_csv(paste0(here::here("data/original/Data/County-Forest_Dep_Comm_Capital.csv")))
nat_amen <- read.csv(paste0(here::here("data/original/natural_amenity.csv")), skip = 104)
econ_typ <- read_csv(paste0(here::here("data/original/county_typology_codes_2015.csv")))
parti_sort <- read_csv(paste0(here::here("data/original/Codebook-County-relative-exposure-aggregate-data-updated.csv")))
rrlrbn_cc <- read_csv(paste0(here::here("data/original/rural_urban_cc_2023.csv")))
del_pop <- read_csv(paste0(here::here("data/original/population_estimates_2022.csv")))
lcv_score <- read_csv(paste0(here::here("data/original/2019-house.csv")))
econ_bea <- read.csv(paste0(here::here("data/original/CAINC6N__ALL_AREAS_2001_2022.csv")))
vote_local <- read.csv(paste0(here::here("data/original/LOCAL_precinct_general.csv")))
bric <- read_excel(paste0(here::here("data/original/bric2020_us_forweb.xlsx")))
vote_gen <- data.frame()

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
  select(GEOID, geometry)

#cong_dist <- tigris::congressional_districts()

# Create one large table with FIPS codes and desired variables

rucc <- rrlrbn_cc %>%
  filter(Attribute == "RUCC_2023") %>%
  select(FIPS, Value) %>%
  rename("RUCC_2023" = "Value")

delpop <- del_pop %>%
  filter(Attribute == "R_NET_MIG_2021") %>%
  select(FIPStxt, Value) %>%
  rename("FIPS" = "FIPStxt", "R_NET_MIG_2021" = "Value")

pop_2020 <- del_pop %>%
  filter(Attribute == "CENSUS_2020_POP") %>%
  select(FIPStxt, Value) %>%
  rename("FIPS" = "FIPStxt", "CENSUS_2020_POP" = "Value") 

nam <- nat_amen %>%
  select(FIPS.Code, X1.Low..7.High) %>%
  rename("FIPS" = "FIPS.Code", "NAT_AMEN_SCORE_1999" = "X1.Low..7.High")

fordep <- forest_depend %>%
  select(fips, forest.area, pct.pay, pct.forest, forest.dependent) %>%
  rename("FIPS" = "fips")

econ <- econ_typ %>%
  select(FIPStxt, `Economic Types Type_2015_Update non-overlapping`, Economic_Type_Label) %>%
  rename("FIPS" = "FIPStxt", "econ_type_2015" = `Economic Types Type_2015_Update non-overlapping`, 
         "econ_label" = "Economic_Type_Label")

`%+%` <- function(x, y)  mapply(sum, x, y, MoreArgs = list(na.rm = TRUE))

elect <- elect_cntx %>% #need to ignore NAs when calculating things
  mutate("vt_pres16" = (trump16 %+% clinton16 %+% otherpres16) / total_population) %>%
  mutate("vt_pres12" = (romney12 %+% obama12 %+% otherpres12) / total_population) %>%
  mutate("ave_vt_pres" = (vt_pres16 %+% vt_pres12) / 2 ) %>%
  mutate("ave_vt_nopres" = (((demsen16 %+% repsen16 %+% othersen16) / total_population) %+%
           ((demhouse16 %+% rephouse16 %+% otherhouse16) / total_population) %+%
           ((demgov14 %+% repgov14 %+% othergov14) / total_population) %+%
             ((demgov16 %+% repgov16 %+% othergov16) / total_population)) / 3) %>% 
  mutate("ave_rep" = ((trump16 / (trump16 %+% clinton16 + otherpres16)) %+% 
           (romney12 / (romney12 %+% obama12 %+% otherpres12)) %+% 
           (repsen16 / (repsen16 %+% demsen16 %+% othersen16)) %+% 
           (rephouse16 / (rephouse16 + demhouse16 %+% otherhouse16)) %+%
           (repgov16/ (repgov16 %+% demgov16 %+% othergov16)) %+%
           (repgov14 / (repgov14 %+% demgov14 %+% othergov14))) / 5) %>%
  mutate("ave_dem" = ((clinton16 / (trump16 %+% clinton16 %+% otherpres16)) %+% 
                              (obama12 / (romney12 %+% obama12 %+% otherpres12)) %+% 
                              (demsen16 / (repsen16 %+% demsen16 %+% othersen16)) %+% 
                              (demhouse16 / (rephouse16 %+% demhouse16 %+% otherhouse16)) %+%
                              (demgov14 / (repgov14 %+% demgov14 %+% othergov14))) / 5) %>%
  select(fips, ave_vt_pres, ave_vt_nopres, ave_rep, ave_dem, lesscollege_pct) %>%
  rename("FIPS" = "fips")

econ_bea$Description <- trimws(econ_bea$Description)
econ_bea$X2022 <- as.numeric(econ_bea$X2022)
linecodes <- as.character(c(1, 81, 100, 200, 300, 400, 500, 600, 700, 800, 807, 900, 1000,
               1100, 1200, 1300, 1500, 1600, 1700, 1800, 1900, 2000, 2010))

col_names <- unique(econ_bea$Description)

econ_comp <- econ_bea %>% 
  select(GeoFIPS, LineCode, Description, X2022) %>%
  filter(LineCode %in% linecodes) %>%
  spread(Description, X2022, fill = 0) %>%
  select(-LineCode) %>%
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

voteloc <- vote_local %>%
  select(office, votes, county_fips) %>%
  group_by(office, county_fips) %>%
  summarise(total_votes = sum(votes)) %>%
  spread(office, total_votes, fill = 0)

# Left-join to pop_2020 by FIPS
# Calculate local voter turnout (could I get 2018 estimates instead?)

files <- list.files(path = here::here("data/original/precinct_2020/"), pattern = ".csv")

for (file in files) {
  tmp_data <- read.csv(paste0(here::here("data/original/precinct_2020/", file)))
  tmp_sum <- tmp_data %>%
    select(office, votes, county_fips) %>%
    group_by(office, county_fips) %>%
    summarise(total_votes = sum(votes))
  vote_gen <- rbind(vote_gen, tmp_sum)
}

## need to figure out which "offices" to filter out
## sum again to get total votes by county for local elections (and general elec)
## then combine with Census population data by FIPS code
## then calculate the voter turnout (for each county total_votes/total_population)
## but I may only want total population of eligible voters

# need to open each state's csv
# Then group by office and FIPS
# Sum the votes 
# append to dataframe

update_fips <- function(date_set) {
  data_set$FIPS <- as.character(data_set$FIPS)
  data_set$FIPS <- str_pad(data_set$FIPS, 5, side="left", pad="0")
  return(data_set)
}

data_set <- elect
elect_fips <- update_fips(elect)
data_set <- fordep
fordep_fips <- update_fips(fordep)
data_set <- nam
nam_fips <- update_fips(nam)
data_set <- econ
econ_fips <- update_fips(econ)
data_set <- delpop
delpop_fips <- update_fips(delpop)
data_set <- rucc
rucc_fips <- update_fips(rucc)
data_set <- econ_compercent
econ_compercent_fips <- update_fips(econ_compercent) # need to adjust column names
econ_compercent_fips <- econ_compercent_fips %>% 
  mutate(FIPS= trimws(as.character(FIPS)))

all_vars <- plyr::join_all(list(elect_fips, fordep_fips, nam_fips, 
                                delpop_fips, rucc_fips, econ_compercent_fips),
                     by='FIPS', 
                     type='left')

# Join to counties

var_bdry <- left_join(all_vars, counties,
                    by = c("FIPS" = "GEOID"))

var_bdry <- st_as_sf(var_bdry)

# Join to congressional districts...only needed for LCV score

## Check and fix validity
all(st_is_valid(var_bdry))

## Check for empty geometries and invalid or corrupt geometries 
any(st_is_empty(var_bdry))
any(is.na(st_is_valid(var_bdry)))
any(na.omit(st_is_valid(var_bdry)) == FALSE)
st_is_longlat(var_bdry)

# Set the projection and a base shape
#prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # projection for NLCD2021
#base_shp <- var_bdry %>% st_transform(., crs = prj)


## Write the validated and factorized shp to a new shp
write_sf(obj = var_bdry, dsn = paste0(here::here("data/processed/"), "all_vars_to_rst_", Sys.Date(), ".shp"), overwrite = TRUE, append = FALSE)
print("new shapefile written")
