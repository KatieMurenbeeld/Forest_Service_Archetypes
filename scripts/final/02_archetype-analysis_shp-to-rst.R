library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(tigris)

#---Load the shapefiles-----

# variables from csv county data
all_vars <- st_read(here::here("data/processed/all_vars_to_rst_2024-05-31.shp"))

# Wilderness areas
wild <- st_read(here::here("data/original/S_USA.Wilderness.shp"))

# Critical habitat
crithab <- st_read(here::here("data/original/crithab_poly.shp"))

# CEJST (Climate and Economic Justice Screening Tool)
cejst <- st_read(here::here("data/original/usa.shp"))

# county level federal coverage
fed_cov <- st_read(here::here("data/processed/county_fed_gov_coverage_pct2024-05-28.shp"))

# county level federal shannan diversity and evenness
fed_shann <- st_read(here::here("data/processed/county_fed_shannon_div_even_2024-05-29.shp"))

#---Load reference raster----
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
crs(ref_rast)

#---Transform the projection of shapefiles----
st_crs(all_vars)

all_vars_proj <- all_vars %>% st_transform(., crs = crs(ref_rast))
wild_proj <- wild %>% st_transform(., crs = crs(ref_rast))
crithab_proj <- crithab %>% st_transform(., crs = crs(ref_rast))
cejst_proj <- cejst %>% st_transform(., crs = crs(ref_rast))
counties_proj <- counties %>% st_transform(., crs = crs(ref_rast))
fed_cov_proj <- fed_cov %>% st_transform(., crs = crs(ref_rast))
fed_shann_proj <- fed_shann %>% st_transform(., crs = crs(ref_rast))

#---Calculate distances from wilderness areas and critical habitat
## Create a template raster for the shapefiles
XMIN <- ext(ref_rast)$xmin
XMAX <- ext(ref_rast)$xmax
YMIN <- ext(ref_rast)$ymin
YMAX <- ext(ref_rast)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(ref_rast))

wild <- rasterize(vect(wild_proj),templateRas)
wild_dist <- terra::distance(wild)
plot(wild_dist)
plot(wild, add = TRUE)

wild_dist_crop <- crop(wild_dist, ref_rast, mask = TRUE)
plot(wild_dist_crop)
names(wild_dist_crop) <- "distance_to_wilderness_m"

criti <- rasterize(vect(crithab_proj), templateRas)
criti_dist <- terra::distance(criti)
plot(criti_dist)
plot(criti, add = TRUE)

criti_dist_crop <- crop(criti_dist, ref_rast, mask = TRUE)
plot(criti_dist_crop)
names(criti_dist_crop) <- "distance_to_crithab_m"

#---Rasterize shapefiles with variables of interest----
percent_forpay_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "pct_pay")
percent_for_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "pct_frs")
percent_ext_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "ext_pct")
commcap_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "COMMCAP")
#fordep_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "frst_dp")
delpop_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "R_NET_M")
lesshighsch_rast <- rasterize(vect(cejst_proj), ref_rast, field = "HSEF")
propburd_rast <- rasterize(vect(cejst_proj), ref_rast, field = "HBF_PFS")
enerburd_rast <- rasterize(vect(cejst_proj), ref_rast, field = "EBF_PFS") 
pm25_rast <- rasterize(vect(cejst_proj), ref_rast, field = "PM25F_PFS")
#percent_sitesee_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "sghts_p")
#percent_govpay_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "gov_p")
percent_fed_area_rast <- rasterize(vect(fed_cov_proj), ref_rast, field = "coverag")
fed_even_rast <- rasterize(vect(fed_shann_proj), ref_rast, field = "E")

#---Check alignment and extents-----
rast_stack <- c(percent_forpay_rast, percent_for_rast, percent_ext_rast, 
                commcap_rast, delpop_rast, lesshighsch_rast, propburd_rast,
                enerburd_rast, pm25_rast, percent_fed_area_rast, fed_even_rast, 
                wild_dist_crop, criti_dist_crop)
# Visually check the rasters
plot(rast_stack)

writeRaster(x = rast_stack, filename = paste0(here::here("data/processed/"), "arch_attri_", Sys.Date(), ".tif"), overwrite = TRUE)

