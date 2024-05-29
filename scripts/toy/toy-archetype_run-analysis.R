# Use this script to run through the entire analysis
# 00_01*.R, 00_02*.R, and 00_03*.R have been commented out for the meeting
# The resulting files from each script are already in data/processed/ 
# If you want to test the entire analysis un-comment those lines

source(here::here("scripts/toy/00_00_toy-archetype_download-data.R"))
#source(here::here("scripts/toy/00_01_toy-archetype_fire-data-prep.R"))
#source(here::here("scripts/toy/00_02_toy-archetype_PADUS-data-prep.R"))
#source(here::here("scripts/toy/00_03_toy-archetype_geodata-data-prep.R"))
source(here::here("scripts/toy/01_toy-archetype_csv-to-shp.R"))
source(here::here("scripts/toy/02_toy-archetype_shp-to-rst.R"))
source(here::here("scripts/toy/03_toy-archetype_rst-to-fcm-rst.R"))
source(here::here("scripts/toy/04_toy-archetype_fcm_parameters.R"))
source(here::here("scripts/toy/05_toy-archetype_fcm-analysis.R"))
source(here::here("scripts/toy/06_toy-archetype_fcm-maps.R"))

# print session information
sI <- sessionInfo()
sI

print(sI, RNG = TRUE, locale = FALSE)
toLatex(sI, locale = FALSE) 
