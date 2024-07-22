library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(haven)
library(tigris)

projection = "epsg:5070"

#---Load the shapefiles-----

# variables from csv county data
all_vars <- st_read(here::here("data/processed/all_vars_to_rst_2024-07-19.shp"))

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
fia_prod <- st_read(here::here("data/processed/conus_prod_fill_fia.shp"))

#---Load reference raster----
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
ref_rast_proj <- project(ref_rast, projection)

#---Transform the projection of shapefiles----
st_crs(all_vars)

all_vars_proj <- all_vars %>% st_transform(., crs = projection)
wild_proj <- wild %>% st_transform(., crs = projection)
crithab_proj <- crithab %>% st_transform(., crs = projection)
cejst_proj <- cejst %>% st_transform(., crs = projection)
#counties_proj <- counties %>% st_transform(., crs = projection)
fia_proj <- fia_prod %>% st_transform(., crs = projection)
#fed_cov_proj <- fed_cov %>% st_transform(., crs = projection)
#fed_shann_proj <- fed_shann %>% st_transform(., crs = projection)

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

#---Fill in missing county level data for the CEJST dataset----
index <- st_touches(all_vars_proj, all_vars_proj)

all_vars_proj_fill <- all_vars_proj %>% 
  mutate(pct_pay_fill = ifelse(is.na(pct_frp),
                               apply(index, 1, function(i){mean(.$pct_frp[i], na.rm = TRUE)}),
                               pct_frp),
         COMMCAP_fill = ifelse(is.na(COMMCAP),
                               apply(index, 1, function(i){mean(.$COMMCAP[i], na.rm = TRUE)}),
                               COMMCAP),
         R_NET_M_fill = ifelse(is.na(R_NET_M),
                               apply(index, 1, function(i){mean(.$R_NET_M[i], na.rm = TRUE)}),
                               R_NET_M)
         )

cejst_proj_fill <- cejst_proj %>%
  mutate(HSEF_fill = ifelse(is.na(HSEF),
                            apply(index, 1, function(i){mean(.$HSEF[i], na.rm = TRUE)}),
                            HSEF),
         HBF_PFS_fill = ifelse(is.na(HBF_PFS),
                            apply(index, 1, function(i){mean(.$HBF_PFS[i], na.rm = TRUE)}),
                            HBF_PFS),
         EBF_PFS_fill = ifelse(is.na(EBF_PFS),
                            apply(index, 1, function(i){mean(.$EBF_PFS[i], na.rm = TRUE)}),
                            EBF_PFS),
         PM25F_PFS_fill = ifelse(is.na(PM25F_PFS),
                            apply(index, 1, function(i){mean(.$PM25F_PFS[i], na.rm = TRUE)}),
                            PM25F_PFS)
         )

#---Rasterize shapefiles with variables of interest----
percent_forpay_rast <- rasterize(vect(all_vars_proj_fill), ref_rast_proj, field = "pct_pay_fill")
#percent_for_rast <- rasterize(vect(all_vars_proj_fill), ref_rast_proj, field = "pct_frs")
#percent_ext_rast <- rasterize(vect(all_vars_proj_fill), ref_rast_proj, field = "ext_pct")
commcap_rast <- rasterize(vect(all_vars_proj_fill), ref_rast_proj, field = "COMMCAP_fill")
#fordep_rast <- rasterize(vect(all_vars_proj_fill), ref_rast_proj, field = "frst_dp")
delpop_rast <- rasterize(vect(all_vars_proj_fill), ref_rast_proj, field = "R_NET_M_fill")
lesshighsch_rast <- rasterize(vect(cejst_proj_fill), ref_rast_proj, field = "HSEF_fill")
propburd_rast <- rasterize(vect(cejst_proj_fill), ref_rast_proj, field = "HBF_PFS_fill")
enerburd_rast <- rasterize(vect(cejst_proj_fill), ref_rast_proj, field = "EBF_PFS_fill") 
pm25_rast <- rasterize(vect(cejst_proj_fill), ref_rast_proj, field = "PM25F_PFS_fill")
#percent_sitesee_rast <- rasterize(vect(all_vars_proj), ref_rast_proj, field = "sghts_p")
#percent_govpay_rast <- rasterize(vect(all_vars_proj), ref_rast_proj, field = "gov_p")
#percent_fed_area_rast <- rasterize(vect(fed_cov_proj), ref_rast_proj, field = "coverag")
#fed_even_rast <- rasterize(vect(fed_shann_proj), ref_rast_proj, field = "E")
fia_prod_rast <- rasterize(vect(fia_proj), ref_rast_proj, field = "mn_prd_")
aip_rast <- rasterize(vect(all_vars_proj_fill), ref_rast_proj, field = "mrp_dlg")
demshare_rast <- rasterize(vect(all_vars_proj_fill), ref_rast_proj, field = "dmshr_p")

#---Check alignment and extents-----
rast_stack <- c(percent_forpay_rast, commcap_rast, delpop_rast, 
                lesshighsch_rast, propburd_rast,
                enerburd_rast, pm25_rast, fia_prod_rast,
                aip_rast, demshare_rast,
                wild_dist_crop, criti_dist_crop)
# Visually check the rasters and replace NAs with 0s if needed
plot(rast_stack)

writeRaster(x = rast_stack, filename = paste0(here::here("data/processed/"), "arch_attri_", Sys.Date(), ".tif"), overwrite = TRUE)

