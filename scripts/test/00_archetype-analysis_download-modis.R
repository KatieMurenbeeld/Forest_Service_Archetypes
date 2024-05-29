# ------------------------------------------------------------------------------------------------ #
# How to Access the LP DAAC Data Pool with R
# The following R code example demonstrates how to configure a connection to download data from an
# Earthdata Login enabled server, specifically the LP DAAC Data Pool.
# source: https://git.earthdata.nasa.gov/projects/LPDUR/repos/daac_data_download_r/browse/DAACDataDownload.R
# ------------------------------------------------------------------------------------------------ #
# Author: LP DAAC
# Last Updated: 06/07/2022
# ------------------------------------------------------------------------------------------------ #
# Check for required packages, install if not previously installed
if ("sys" %in% rownames(installed.packages()) == FALSE) {install.packages("sys")}
if ("getPass" %in% rownames(installed.packages()) == FALSE) { install.packages("getPass")}
if ("httr" %in% rownames(installed.packages()) == FALSE) { install.packages("httr")}

# Load necessary packages into R
library(sys)
library(getPass)
library(httr)
# ---------------------------------SET UP ENVIRONMENT--------------------------------------------- #
# IMPORTANT: Update the line below if you want to download to a different directory (ex: "c:/data/")
dl_dir <- "~/Analysis/NEPA_Efficiency/data/original/"                                 # Set dir to download files to
setwd(dl_dir)                                                # Set the working dir to the dl_dir
usr <- file.path(Sys.getenv("USERPROFILE"))                  # Retrieve home dir (for netrc file)
if (usr == "") {usr = Sys.getenv("HOME")}                    # If no user profile exists, use home
netrc <- file.path(usr,'.netrc', fsep = .Platform$file.sep)  # Path to netrc file

# ------------------------------------CREATE .NETRC FILE------------------------------------------ #
# If you already have a .netrc file with your Earthdata Login credentials stored in your home
# directory, this portion will be skipped. Otherwise you will be prompted for your NASA Earthdata
# Login Username/Password and a netrc file will be created to store your credentials (in home dir)
if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
  
  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
}

# ---------------------------CONNECT TO DATA POOL AND DOWNLOAD FILES------------------------------ #
# Below, define either a single link to a file for download, a list of links, or a text file
# containing links to the desired files to download. For a text file, there should be 1 file link
# listed per line. Here we show examples of each of the three ways to download files.
# **IMPORTANT: be sure to update the links for the specific files you are interested in downloading.

# 1. Single file:
#files <- "https://e4ftl01.cr.usgs.gov/MOLT/MOLT/MOD17A2HGF.061/ MYD09GA.A2002187.h10v04.061.2020071193416.hdf"

# 2. List of files:
files <- c("https://data.ornldaac.earthdata.nasa.gov/protected/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_flux_ED_ICESat2.tif",
           "https://data.ornldaac.earthdata.nasa.gov/public/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_flux_ED_ICESat2.tif.sha256",
           "https://data.ornldaac.earthdata.nasa.gov/protected/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_stock_ED_ICESat2.tif",
           "https://data.ornldaac.earthdata.nasa.gov/public/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_stock_ED_ICESat2.tif.sha256",
           "https://data.ornldaac.earthdata.nasa.gov/protected/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_flux_ED_GEDI_ICESat2.tif",
           "https://data.ornldaac.earthdata.nasa.gov/public/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_flux_ED_GEDI_ICESat2.tif.sha256",
           "https://data.ornldaac.earthdata.nasa.gov/protected/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_stock_ED_GEDI_ICESat2.tif",
           "https://data.ornldaac.earthdata.nasa.gov/public/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_stock_ED_GEDI_ICESat2.tif.sha256",
           "https://data.ornldaac.earthdata.nasa.gov/protected/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_stock_ED_GEDI.tif",
           "https://data.ornldaac.earthdata.nasa.gov/public/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_stock_ED_GEDI.tif.sha256",
           "https://data.ornldaac.earthdata.nasa.gov/protected/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_flux_ED_GEDI.tif",
           "https://data.ornldaac.earthdata.nasa.gov/public/cms/CMS_Global_Forest_AGC/data/forest_aboveground_carbon_flux_ED_GEDI.tif.sha256")

# 3. Textfile containing links:
#files <- readLines("C:/datapool_downloads/URL_file_list.txt", warn = FALSE)

# Loop through all files
for (i in 1:length(files)) {
  filename <-  tail(strsplit(files[i], '/')[[1]], n = 1) # Keep original filename
  
  # Write file to disk (authenticating with netrc) using the current directory/filename
  response <- GET(files[i], write_disk(filename, overwrite = TRUE), progress(),
                  config(netrc = TRUE, netrc_file = netrc), set_cookies("LC" = "cookies"))
  
  # Check to see if file downloaded correctly
  if (response$status_code == 200) {
    print(sprintf("%s downloaded at %s", filename, dl_dir))
  } else {
    print(sprintf("%s not downloaded. Verify that your username and password are correct in %s", filename, netrc)) }
}
    
    
    
    
    