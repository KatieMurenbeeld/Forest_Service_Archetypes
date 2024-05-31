# Download the original data using this script
# Some data will need to be downloaded manually, potentially with authentication
# Some data requires more in-depth processing
# Some data is proprietary

# 0. Load the libraries and set the timeout
library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Some files are large so set the timeout to 100 minutes (6000 seconds)
options(timeout=6000)

# Steps:
# 0. Load libraries, set timeout
# 1. Function to download and unzip files
# 2. Download csv files
# 3. Download shapefiles
# 4. Download tif/img
# 5. Comments for more difficult data

#---1. Function to download and unzip files----
download_data <- function(url, file_name) {
  if (str_detect(url, ".zip")) {
    tmp <- tempfile()
    download.file(url, tmp)
    unzip(zipfile = tmp, exdir = here::here("data/toy/original/"))
  } else if (str_detect(file_name, ".zip")) {
    download.file(url, here::here(paste0("data/toy/original/", file_name)))
    unzip(zipfile = here::here(paste0("data/toy/original/", file_name)))
  }
  else {
    download.file(url, here::here(paste0("data/toy/original/"), file_name))
  }
}

#---2. Download csv data----

# Population data
url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=3995.5"
file_name <- "population_estimates_2022.csv"

download_data(url, file_name)

# Forest dependency 
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2021-0077/RDS-2021-0077.zip"
file_name <- "forest_depend.zip"

download_data(url, file_name)

# Election Context 2018
x <- getURL("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")
y <- read.csv(text = x)
write.csv(y, here::here("data/toy/original/election_context_2018.csv"))

# Baseline Resilience Indicators for Communities (BRIC) data
url <- "https://sc.edu/study/colleges_schools/artsandsciences/centers_and_institutes/hvri/documents/bric/bric2020_us_forweb.xlsx"

download.file(url, destfile = here::here("data/toy/original/bric2020_us.xlsx"))

#---3. Download shp data----

# USFS National Forest Boundaries
url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
file_name <- ""

download_data(url, file_name)

# USFS Regional Boundaries
url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeRegion.zip"
file_name <- ""

download_data(url, file_name)


# National Wilderness Areas
url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Wilderness.zip"
file_name = ""

download_data(url, file_name)

# FAA National Tree Growth Database
url <- "https://www.airporttech.tc.faa.gov/DesktopModules/EasyDNNNews/DocumentDownload.ashx?portalid=0&moduleid=3682&articleid=2870&documentid=3042"
file_name = "faa_tree-growth-ecoregions.zip"

download_data(url, file_name)
unzip(paste0(here::here("data/toy/original/"), file_name),  exdir = here::here("data/toy/original/"))

# Climate and Economic Justice Screening Tool
url <- "https://static-data-screeningtool.geoplatform.gov/data-versions/1.0/data/score/downloadable/1.0-shapefile-codebook.zip"
file_name <- "usa.zip"

download_data(url, file_name)
unzip(paste0(here::here("data/toy/original/"), file_name),  exdir = here::here("data/toy/original/"))

# Forest Ownership
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2020-0044/RDS-2020-0044.zip"
file_name <- ""

download_data(url, file_name)

#---4. Download tif data----
## These are too large for toy example?

# National Land Cover Data 2021
#url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2021_land_cover_l48_20230630.zip"
#file_name <- ""

#download_data(url, file_name)

# Land Cover Change Index
#url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2001_2021_land_cover_change_index_l48_20230630.zip"
#file_name <- ""

#download_data(url, file_name)

# Forest Type
# url <- "https://data.fs.usda.gov/geodata/rastergateway/forest_type/conus_forest-type.zip"
# file_name <- ""

# download_data(url, file_name)

#---5. Data that needs more in-depth downloading, processing----
#---is by subscription only or requires an account with AppEARS (EarthData)-----

# NACP 1-km forest age maps
# see https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1096
# see data/original/NA_TreeAge_1096/data 

# Regional Economic Accounts
# From the "Personal income (state and local)" dropdown select 
# CAINC6NC: compensation of employees by NAICS industry
# see https://apps.bea.gov/regional/downloadzip.cfm?_gl=1*7muxs7*_ga*MTYzOTk2OTAyNC4xNzExOTg3Njgz*_ga_J4698JNNFT*MTcxMTk4NzY4Mi4xLjEuMTcxMTk4Nzk3OC42MC4wLjA

# Wildfire Risk
# See scripts/final/00_01_archetype-analysis_download-prep-fire.R

# Temp, Precip, Elevation, and Travel time 
# See scripts/final/00_02_archetype-analysis_geodata_data-prep.R

# Mill capacity data 
# See scripts/final/00_03_archetype-analysis_forisk-analysis.R
# Note that raw data is only available to subscribers
# Change in mill capacity will be in "data/processed" folder
# millchange_cap.tif. millchangecap_interp*.tif

# Protected Lands and Federal Ownership
# See scripts/final/00_04_archetype-analysis_PADUS_data-prep.R

# Forest Gain from Hansen et al 2013 (Global Forest Change) - testing
# See https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.1.html
# See script scripts/test/00_archetype-analysis_forest_gain_data-prep.R


print("all files downloaded to /data/toy/original")
