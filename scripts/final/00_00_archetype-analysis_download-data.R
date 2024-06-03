# 
library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Some files are large so set the timeout to 100 minutes (6000 seconds)
options(timeout=6000)

# There are three file types we will download: csv, shp, and tif/img.

# update this to detect .zip in the file name as well
download_data <- function(url, file_name) {
  if (str_detect(url, ".zip")) {
    tmp <- tempfile()
    download.file(url, tmp)
    unzip(zipfile = tmp, exdir = here::here("data/original/"))
  } else if (str_detect(file_name, ".zip")) {
    download.file(url, here::here(paste0("data/original/", file_name)))
    unzip(zipfile = here::here(paste0("data/original/", file_name)))
  }
  else {
    download.file(url, here::here(paste0("data/original/", file_name)))
  }
}

#---Download csv data-----------------------------------------------------------

# Partisan sorting, need to download from website
## Still need way to ref spatially!!

#url <- "https://dataverse.harvard.edu/file.xhtml?fileId=8165593&version=3.0#"
#file_name <- "partisan_sorting.csv"

#download_data(url, file_name)

# League of Conservation Voters
rt <- "http://scorecard.lcv.org/exports/"
yr <- seq(from=2018,to=2019,by=1)
hs <- "-house"
fl <- "-scorecard-grid-export.csv"

for(i in 1:length(yr)){
  y <- yr[i]
  link <- paste0(rt,y,hs,fl)
  fname <- paste0(here::here("data/original/"),y,hs,".csv")
  download.file(url=link, destfile=fname)
}

# Change in population (from USDA ERS)
url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=3995.5"
file_name <- "population_estimates_2022.csv"

download_data(url, file_name)

# Change in population (from Census)
url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2023/counties/totals/co-est2023-comp.xlsx"
file_name <- "census_2023_comp_est.xlsx"

download_data(url_file_name)
# Forest dependency 
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2021-0077/RDS-2021-0077.zip"
file_name <- "forest_depend.zip"

download_data(url, file_name)

# Election Context 2018
x <- getURL("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")
y <- read.csv(text = x)
write.csv(y, here::here("data/original/election_context_2018.csv"))

# Baseline Resilience Indicators for Communities (BRIC) data
url <- "https://sc.edu/study/colleges_schools/artsandsciences/centers_and_institutes/hvri/documents/bric/bric2020_us_forweb.xlsx"

download.file(url, destfile = here::here("data/original/bric2020_us.xlsx"))

#---Download shp data-----------------------------------------------------------

# USFS National Forest Boundaries
url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip"
file_name <- ""

download_data(url, file_name)

# USFS Regional Boundaries
url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeRegion.zip"
file_name <- ""

download_data(url, file_name)

# FWS Critical Habitat
url <- "https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip"
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
unzip(paste0(here::here("data/original/"), file_name),  exdir = here::here("data/original/"))

# Climate and Economic Justice Screening Tool
url <- "https://static-data-screeningtool.geoplatform.gov/data-versions/1.0/data/score/downloadable/1.0-shapefile-codebook.zip"
file_name <- ""

download_data(url, file_name)

#---Download tif data-----------------------------------------------------------
## These may be too large to do through RStudio?

# Forest Ownership
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2020-0044/RDS-2020-0044.zip"
file_name <- ""

download_data(url, file_name)

# National Land Cover Data 2021
#url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2021_land_cover_l48_20230630.zip"
#file_name <- ""

#download_data(url, file_name)

# Land Cover Change Index
#url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2001_2021_land_cover_change_index_l48_20230630.zip"
#file_name <- ""

#download_data(url, file_name)

# Forest Type
url <- "https://data.fs.usda.gov/geodata/rastergateway/forest_type/conus_forest-type.zip"
file_name <- ""

download_data(url, file_name)

# Forest stand age 1km
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2015-0043/RDS-2015-0043_Data_2011.zip"
file_name <- ""

download_data(url, file_name)

#---Data that needs more in-depth downloading, processing-----------------------
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
