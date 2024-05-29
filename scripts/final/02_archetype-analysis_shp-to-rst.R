library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(tigris)

#---Load the shapefiles-----

# variables from csv county data
all_vars <- st_read(here::here("data/processed/all_vars_to_rst_2024-04-03.shp"))

# Wilderness areas
wild <- st_read(here::here("data/original/S_USA.Wilderness.shp"))

# Critical habitat
crithab <- st_read(here::here("data/original/crithab_poly.shp"))

# CEJST (Climate and Economic Justice Screening Tool)
cejst <- st_read(here::here("data/original/usa/usa.shp"))

#---Load reference raster----
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
crs(ref_rast)

#---Transform the projection of shapefiles----
st_crs(all_vars)

all_vars_proj <- all_vars %>% st_transform(., crs = crs(ref_rast))
wild_proj <- wild %>% st_transform(., crs = crs(ref_rast))
crithab_proj <- crithab %>% st_transform(., crs = crs(ref_rast))
cejst_proj <- cejst %>% st_transform(., crs = crs(ref_rast))

#---Calculate distances from wilderness areas and critical habitat
#mill_proj <- conus_mills %>% st_transform(., crs = crs(ref_rast))
counties_proj <- counties %>% st_transform(., crs = crs(ref_rast))

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
rruc_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "RUCC_20")
vtpres_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "av_vt_p")
vtnopres_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "av_vt_n")
#econ15_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "e__2015")
percent_demvt_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "ave_dem")
percent_repvt_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "ave_rep")
percent_forpay_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "pct_pay")
percent_for_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "pct_frs")
fordep_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "frst_dp")
lesscoll_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "lsscll_")
nam_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "NAT_AME")
delpop_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "R_NET_M")
lif_rast <- rasterize(vect(cejst_proj), ref_rast, field = "LIF_PFS")
lmi_rast <- rasterize(vect(cejst_proj), ref_rast, field = "LMI_PFS") 
tf_rast <- rasterize(vect(cejst_proj), ref_rast, field = "TF_PFS") 
td_rast <- rasterize(vect(cejst_proj), ref_rast, field = "TD_PFS")
ealr_rast <- rasterize(vect(cejst_proj), ref_rast, field = "EALR_PFS") 
pm25_rast <- rasterize(vect(cejst_proj), ref_rast, field = "PM25F_PFS")
percent_sitesee_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "sghts_p")
percent_govpay_rast <- rasterize(vect(all_vars_proj), ref_rast, field = "gov_p")

#crithab_rast <- rasterize(vect(crithab_proj), ref_rast, field = "listing_st")

#wild_rast <- rasterize(vect(wild_proj), ref_rast, field = "BOUNDARYST")

#---Layerize (segregate) the categorical rasters----
rruc_layers_rast <- terra::segregate(rruc_rast)
#econ15_layers_rast <- terra::segregate(econ15_rast)
nam_layer_rast <- terra::segregate(nam_rast)

# Update names
names(rruc_layers_rast)
names(rruc_layers_rast) <- c("metro_greater1mil", "metro_250k_1mil", 
                             "metro_less250k", "nonmetro_greater20k_adj",
                             "nonmetro_greater20k_nonadj", "nonmetro_5k_20k_adj", 
                             "nonmetro_5k_20k_nonadj", "nonmetro_less5k_adj",
                             "nonmetro_less5k_nonadj")
names(econ15_layers_rast)
names(econ15_layers_rast) <- c("nonspecialized", "farming", "mining", 
                               "manufacturing", "fed_state_gov", "recreation")
names(nam_layer_rast)
names(nam_layer_rast) <- c("remove", "low_nam", "low_neu_nam", "neu_low_nam", "neutral_nam", "neu_high_nam", "high_neu_nam", "high_nam")
nam_layer_rast <- nam_layer_rast[[2:8]]


#---Check alignment and extents-----
rast_stack <- c(rruc_rast, vtpres_rast, vtnopres_rast, percent_demvt_rast, 
                percent_repvt_rast, percent_forpay_rast, percent_sitesee_rast, 
                percent_govpay_rast, fordep_rast, lesscoll_rast, nam_rast, 
                delpop_rast, ref_rast, criti_dist_crop, wild_dist_crop,  
                lif_rast, lmi_rast, tf_rast, td_rast, ealr_rast, pm25_rast)

writeRaster(x = rast_stack, filename = paste0(here::here("data/processed/"), "arch_attri_05_", Sys.Date(), ".tif"), overwrite = TRUE)

#---Check on the plots----
plot(rast_stack[[1:4]])
