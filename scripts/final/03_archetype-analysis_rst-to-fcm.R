## some code came from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html

library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)

#---Load the data-----
ref_rast <- rast(here::here("data/processed/merged/WHP_merge3000m.tif"))
arch_attri <- rast(here::here("data/processed/arch_attri_05_2024-04-03.tif"))
mill_dist <- rast(here::here("data/processed/mill_dist_open.tif"))
mill_change_cap <- rast(here::here("data/processed/millchangecap_interp-2.tif"))
agc_flux <- rast("~/Analysis/NEPA_Efficiency/data/original/forest_aboveground_carbon_flux_ED_GEDI_ICESat2.tif")
agc_stock <- rast("~/Analysis/NEPA_Efficiency/data/original/forest_aboveground_carbon_stock_ED_GEDI.tif")
biodiver <- rast(here::here("data/original/distribute_RecognizedBiodiversityValue_2020_7_2020/RecognizedBiodiversityValue_USERC_2020_7_2.tif"))
geophys <- rast(here::here("data/original/Settings/Settings_2020_3_31_USAwwat.tif"))
spei <- rast(here::here("data/original/GRIDMET-speih-12mo.tif"))
elevation <- rast(here::here("data/original/_ags_b3860be0_3b01_41fb_ba0d_06d68b293983.tif"))
land_cover <- rast(here::here("data/original/nlcd_2021_land_cover_l48_20230630.img"))
tree_age <- rast(here::here("data/original/NA_TreeAge_1096/data/conus_age06_1km.tif"))
for_type <- rast(here::here("data/original/conus_forest-type (1)/conus_foresttype.img"))

# ignore for now, file corrupted?
#gpp_rast <- rast(here::here("data/original/CarbonFlux_2000_2013/Average_GPP_2000_2013.img"))

#---Process the original rasters----
# Aggregate, resample or reproject, and crop
#agc_stock_proj <- project(agc_stock, 
#                          y = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
#agc_stock_crop <- crop(agc_stock_proj, ref_rast, mask = TRUE)

biodiver_agg <- aggregate(biodiver, 100, fun = mean)
biodiver_resamp <- resample(biodiver_agg, ref_rast)
biodiver_crop <- crop(biodiver_resamp, ref_rast, mask = TRUE)

geophys_agg <- aggregate(geophys, 100, fun = mean)
geophys_resamp <- project(geophys_agg, ref_rast)
geophys_crop <- crop(geophys_resamp, ref_rast, mask = TRUE)

ele_proj <- project(elevation, ref_rast)
ele_resamp <- resample(ele_proj, ref_rast)
ele_crop <- crop(ele_resamp, ref_rast, mask = TRUE)

## For forest type group into either hardwood or softwood
hardwood <- c("Oak - Pine Group", "Oak - Hickory Group", "Oak - Gum - Cypress Group", 
              "Elm - Ash - Cottonwood Group", "Maple - Beech - Birch Group", 
              "Tropical Hardwoods Group", "Aspen - Birch Group", "Exotic Hardwoos Group", 
              "Alder - Maple Group", "Wester Oak Group", "Tanoak - Laurel Group", 
              "Other Western Hardwoods Group")
softwood <- c("White - Red - Jack Pine Group", "Spruce - Fir Group", "Longleaf - Slash Pine Group",
              "Loblolly - Shortleaf Pine Group", "Exotic Softwoods Group", "Douglas-fir Group",
              "Ponderosa Pine Group", "Pinyon - Juniper Group", "Western White Pine Group", 
              "Fir - Spruce - Mountain Hemlock Group", "Lodgepole Pine Group", 
              "Hemlock - Sitka Spruce Group", "Western Larch Group", "Redwood Group", 
              "Other Western Softwoods Group", "California Mixed Conifer Group")


# Check alignment 
rast_stack <- c(arch_attri, mill_dist, biodiver_crop, geophys_crop)

#---Process raster for use with geocmeans----

# Update the names if you want 
names(rast_stack)

# Select Variables for FCmeans
rst_fcm <- rast_stack[[c("R_NET_M", "pct_pay", "sghts_p", "gov_p",
                         "ave_dem", "lsscll_", "WHP", "CL_ELEV", 
                         "distance_to_wilderness_m", "last")]]
names(rst_fcm) <- c("net_mig", "pct_for_pay", "pct_sight_pay", "pct_gov_p", 
                    "ave_dem", "less_coll", "WHP", "geophysical", 
                    "distance_to_wilderness_m", "distance_to_mill_m")
writeRaster(rst_fcm, paste0("data/processed/rast_fcm_08_", Sys.Date(), ".tif"), overwrite = TRUE)

# Scale the data
rst_fcm_sc <- scale(rst_fcm)

# Quickly investigate the correlation between the attributes
correlation <- layerCor(rst_fcm, "pearson", na.rm = TRUE)
fcm_08_cor <- as.data.frame(correlation$correlation)
#write_csv(fcm_08_cor, here::here("data/processed/fcm_07_cor.csv"))

# Convert to a simple list of SpatRaster
dataset <- lapply(names(rst_fcm_sc), function(n){
  aband <- rst_fcm_sc[[n]]
  return(aband)
})

names(dataset) <- names(rst_fcm_sc)

#---Find appropriate parameter values----
### Code below from https://jeremygelb.github.io/geocmeans/articles/web_vignettes/rasters.html
# finding an appropriate k and m values
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 2:10, m = seq(1.1,2,0.1), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Silhouette.index"),
                                  verbose = TRUE)

# plotting the silhouette index values
sil.idx <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2) +
  coord_fixed(ratio=0.125) +
  scale_fill_viridis()
sil.idx
ggsave(here::here("figures/FCM_08_sil_idx_02.png"), sil.idx, 
       width = 12, height = 12, dpi = 300)

# plotting the explained inertia
ex.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert
ggsave(here::here("figures/FCM_08_ex_inert_02.png"), ex.inert, 
       width = 12, height = 12, dpi = 300)

#---Run the FCM---- 
FCM_result <- CMeans(dataset, k = 5, m = 1.1, standardize = FALSE)
map.res <- rast(FCM_result$rasters)
writeRaster(map.res[["Groups"]], filename = paste0("data/processed/FCM_08_", Sys.Date(), ".tif"))


##---save the iteration, k, m as a dataframe----
aa_iteration <- data.frame(iteration_name = character(),
                           attris = character(),
                           k = numeric(),
                           m = numeric())

iteration_name <- "FCM_08"
attris <- paste(names(rst_fcm), collapse= ", ")
k <- 5
m <- 1.1

aa_iteration[nrow(aa_iteration) + 1,] <- list(iteration_name, attris, k, m)

write_csv(aa_iteration, here::here("data/processed/aa_iteration.csv"), append = TRUE)






