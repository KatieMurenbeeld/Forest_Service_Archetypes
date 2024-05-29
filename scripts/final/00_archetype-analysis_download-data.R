# 
library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Some files are large so set the timeout to 10 minutes (6000 seconds)
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

url <- "https://dataverse.harvard.edu/file.xhtml?fileId=8165593&version=3.0#"
file_name <- "partisan_sorting.csv"

download_data(url, file_name)

# Natural amenity rank
url <- "https://www.ers.usda.gov/webdocs/DataFiles/52201/natamenf_1_.xls?v=723.9"
file_name <- "natural_amenity.csv"

download_data(url, file_name)

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

# Economy typology
url <- "https://www.ers.usda.gov/webdocs/DataFiles/48652/2015CountyTypologyCodes.csv?v=7440.8"
file_name <- "county_typology_codes_2015.csv"

download_data(url, file_name)

# Change in population
url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=3995.5"
file_name <- "population_estimates_2022.csv"

download_data(url, file_name)

# Rural-Urban continuum
url <- "https://www.ers.usda.gov/webdocs/DataFiles/53251/Ruralurbancontinuumcodes2023.csv?v=4628.7"
file_name <- "rural_urban_cc_2023.csv"
  
download_data(url, file_name)

# Forest dependency 
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2021-0077/RDS-2021-0077.zip"
file_name <- "forest_depend.zip"

download_data(url, file_name)

# Election Context 2018
x <- getURL("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")
y <- read.csv(text = x)
write.csv(y, here::here("data/original/election_context_2018.csv"))

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

# FAA Natioanl Tree Growth Database
url <- "https://www.airporttech.tc.faa.gov/DesktopModules/EasyDNNNews/DocumentDownload.ashx?portalid=0&moduleid=3682&articleid=2870&documentid=3042"
file_name = "faa_tree-growth-ecoregions.zip"

download_data(url, file_name)
#unzip(paste0(here::here("data/original/"), file_name),  exdir = here::here("data/original/"))

# Climate and Economic Justice Screening Tool
url <- "https://static-data-screeningtool.geoplatform.gov/data-versions/1.0/data/score/downloadable/1.0-shapefile-codebook.zip"
file_name <- ""

download_data(url, file_name)

# Forest Dependence 
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2021-0077/RDS-2021-0077.zip"
file_name <- ""

download_data(url, file_name)

#---Download tif data-----------------------------------------------------------
## These may be too large to do through RStudio?

# Elevation
url <- "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Topo_2020-LF2020_Elev_220_CONUS.zip&TYPE=landfire"
file_name <- ""

download_data(url, file_name)

# Elevation 
## from the USGS national map https://apps.nationalmap.gov/3depdem/ export the 

# National Land Cover Data 2021
url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2021_land_cover_l48_20230630.zip"
file_name <- ""

download_data(url, file_name)

# Land Cover Change Index
url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2001_2021_land_cover_change_index_l48_20230630.zip"
file_name <- ""

download_data(url, file_name)

# Forest Type
url <- "https://www.notion.so/CONUS-Forest-Type-1d0c781dd0474cdf82c95015ca7dd5a9?pvs=4"
file_name <- ""

download_data(url, file_name)

# GPP, NEP, RE
url <- ""
file_name <- ""

download_data(url, filename)

#---Data that needs more indepth processing or is by subscription only----------

# Wildfire Risk
# See script 00_archetype-analysis_download-prep-fire.R

# Mill capacity data 
# See script 00_archetype_analysis_forisk-analysis.R
