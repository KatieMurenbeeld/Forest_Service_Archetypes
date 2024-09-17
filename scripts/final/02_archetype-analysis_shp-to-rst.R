library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(haven)
library(tigris)

projection = "epsg:5070"

#---Load the shapefiles-----

# variables from csv county data
vars <- st_read(here::here("data/processed/vars_to_rst_2024-09-17.shp"))
delpop <- st_read(here::here("data/processed/delpop_to_rst_2024-09-17.shp"))

# Wilderness areas
wild <- st_read(here::here("data/original/S_USA.Wilderness.shp"))

# Critical habitat
crithab <- st_read(here::here("data/original/crithab_poly.shp"))

# CEJST (Climate and Economic Justice Screening Tool)
cejst <- st_read(here::here("data/original/usa.shp"))

# county level federal coverage
#fed_cov <- st_read(here::here("data/processed/county_fed_gov_coverage_pct2024-05-28.shp"))

# county level federal shannan diversity and evenness
#fed_shann <- st_read(here::here("data/processed/county_fed_shannon_div_even_2024-05-29.shp"))

# fia productivity 
fia_prod <- st_read(here::here("data/processed/conus_prod_fill_fia_2024-09-12.shp"))

#---Load reference raster----
ref_rast <- rast(here::here("data/processed/merged/conus_whp_3km_agg_2024-08-09.tif"))
ref_rast_proj <- project(ref_rast, projection)

#---Transform the projection of shapefiles----
st_crs(all_vars)

vars_proj <- vars %>% st_transform(., crs = projection)
delpop_proj <- delpop %>% st_transform(., crs = projection)
wild_proj <- wild %>% st_transform(., crs = projection)
crithab_proj <- crithab %>% st_transform(., crs = projection)
cejst_proj <- cejst %>% st_transform(., crs = projection)
fia_proj <- fia_prod %>% st_transform(., crs = projection)

# filter the CEJST for CONUS states
cejst_proj <- cejst_proj %>%
  filter(SF != c("Hawaii", "Alaska", "Puerto Rico",
                  "Northern Mariana Islands", "Guam", "American Samoa"))

#---Calculate distances from wilderness areas and critical habitat
## Create a template raster for the shapefiles
XMIN <- ext(ref_rast_proj)$xmin
XMAX <- ext(ref_rast_proj)$xmax
YMIN <- ext(ref_rast_proj)$ymin
YMAX <- ext(ref_rast_proj)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(ref_rast_proj))

wild <- rasterize(vect(wild_proj),templateRas)
wild_dist <- terra::distance(wild)
#plot(wild_dist)
#plot(wild, add = TRUE)

wild_dist_crop <- crop(wild_dist, ref_rast, mask = TRUE)
#plot(wild_dist_crop)
names(wild_dist_crop) <- "distance_to_wilderness_m"

criti <- rasterize(vect(crithab_proj), templateRas)
criti_dist <- terra::distance(criti)
#plot(criti_dist)
#plot(criti, add = TRUE)

criti_dist_crop <- crop(criti_dist, ref_rast, mask = TRUE)
#plot(criti_dist_crop)
names(criti_dist_crop) <- "distance_to_crithab_m"

#---Fill in data for the CEJST dataset----
cejst_index <- st_touches(cejst_proj, cejst_proj)

cejst_proj_fill <- cejst_proj %>%
  mutate(HSEF_fill = ifelse(is.na(HSEF),
                            apply(cejst_index, 1, function(i){mean(.$HSEF[i], na.rm = TRUE)}),
                            HSEF),
         HBF_PFS_fill = ifelse(is.na(HBF_PFS),
                            apply(cejst_index, 1, function(i){mean(.$HBF_PFS[i], na.rm = TRUE)}),
                            HBF_PFS),
         EBF_PFS_fill = ifelse(is.na(EBF_PFS),
                            apply(cejst_index, 1, function(i){mean(.$EBF_PFS[i], na.rm = TRUE)}),
                            EBF_PFS),
         PM25F_PFS_fill = ifelse(is.na(PM25F_PFS),
                            apply(cejst_index, 1, function(i){mean(.$PM25F_PFS[i], na.rm = TRUE)}),
                            PM25F_PFS)
         )



#---Rasterize shapefiles with variables of interest----
ref_rast_proj_fill <- focal(ref_rast_proj, w = 3, na.rm = TRUE) #fill in the NAs in the ref raster

percent_forpay_rast <- rasterize(vect(vars_proj), templateRas, field = "pct_py_")
#percent_forpay_rast_focal <- focal(percent_forpay_rast, w = 9, na.rm = TRUE)
#percent_forpay_rast_crop <- crop(percent_forpay_rast_focal, ref_rast_proj, mask = TRUE)
#plot(percent_forpay_rast_crop)
commcap_rast <- rasterize(vect(vars_proj), templateRas, field = "COMMCAP")
delpop_rast <- rasterize(vect(delpop_proj), templateRas, field = "")
lesshighsch_rast <- rasterize(vect(cejst_proj_fill), templateRas, field = "HSEF_fill")
propburd_rast <- rasterize(vect(cejst_proj_fill), templateRas, field = "HBF_PFS_fill")
enerburd_rast <- rasterize(vect(cejst_proj_fill), templateRas, field = "EBF_PFS_fill") 
pm25_rast <- rasterize(vect(cejst_proj_fill), templateRas, field = "PM25F_PFS_fill")
#percent_sitesee_rast <- rasterize(vect(all_vars_proj), ref_rast_proj, field = "sghts_p")
#percent_govpay_rast <- rasterize(vect(all_vars_proj), ref_rast_proj, field = "gov_p")
#percent_fed_area_rast <- rasterize(vect(fed_cov_proj), ref_rast_proj, field = "coverag")
#fed_even_rast <- rasterize(vect(fed_shann_proj), ref_rast_proj, field = "E")
fia_prod_rast <- rasterize(vect(fia_proj), templateRas, field = "mn_prd_")
aip_rast <- rasterize(vect(vars_proj), templateRas, field = "mrp_dlgy_f")
demshare_rast <- rasterize(vect(all_vars_proj_fill), templateRas, field = "dmshr_p")

#---Check alignment and extents-----
rast_stack <- c(percent_forpay_rast, commcap_rast, delpop_rast, 
                lesshighsch_rast, propburd_rast,
                enerburd_rast, pm25_rast, fia_prod_rast,
                aip_rast, demshare_rast,
                wild_dist_crop, criti_dist_crop)
# Visually check the rasters and replace NAs with 0s if needed
plot(rast_stack)

writeRaster(x = rast_stack, filename = paste0(here::here("data/processed/"), "arch_attri_", Sys.Date(), ".tif"), overwrite = TRUE)





