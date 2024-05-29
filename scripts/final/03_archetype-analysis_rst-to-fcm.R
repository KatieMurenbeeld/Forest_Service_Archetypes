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
arch_attri <- rast(here::here("data/processed/arch_attri_2024-05-29.tif"))
mill_change_cap <- rast(here::here("data/processed/millchangecap_interp-2.tif"))
prec_seas <- rast(here::here("data/processed/prec_seas_3000m.tif"))
temp_seas <- rast(here::here("data/processed/temp_seas_3000m.tif"))
roughness <- rast(here::here("data/processed/roughness_3000m.tif"))
trav_time <- rast(here::here("data/processed/trav_time_3000m.tif"))

## original rasters
tree_age <- rast(here::here("data/original/NA_TreeAge_1096/data/conus_age06_1km.tif"))
for_type <- rast(here::here("data/original/conus_foresttype.img"))

#---Process the original rasters----

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

identical(crs(ref_rast), crs(tree_age))
identical(crs(ref_rast), crs(for_type))

resamp <- function(raster, ref_raster, method){
  rast_proj <- project(raster, crs(ref_raster))
  rast_resamp <- resample(rast_proj, ref_raster, method)
}

tree_age_resamp <- resamp(tree_age, ref_rast, "bilinear")
for_type_resamp <- resamp(for_type, ref_rast, "bilinear")
mill_change_resamp <- resamp(mill_change_cap, ref_rast, "bilinear")

# Check alignment and stack the rasters
rast_stack <- c(arch_attri, ref_rast, mill_change_resamp, prec_seas, temp_seas, 
                roughness, trav_time, tree_age_resamp)

## Save the raster
writeRaster(x = rast_stack, filename = paste0(here::here("data/processed/"), "full_rast_stack_attributes_", Sys.Date(), ".tif"), overwrite = TRUE)

#---Process raster for use with geocmeans----

# Update the names (optional) 
names(rast_stack)

# Select Variables for FCmeans
#rst_fcm <- rast_stack[[c("R_NET_M", "pct_pay", "sghts_p", "gov_p",
#                         "ave_dem", "lsscll_", "WHP", "CL_ELEV", 
#                         "distance_to_wilderness_m", "last")]]
rst_fcm <- rast_stack
names(rst_fcm) <- c("pct_for_pay", "pct_forest", "forest_depend", "net_mig", 
                    "less_hs", "house_burd", "ener_burd", "pm25", 
                    "pct_sightsee_pay", "pct_gov_pay", "fed_area", "fed_evenness",
                    "wildfire_haz", "mill_cap_5yr_chng", "precip_seas", "temp_seas",
                    "roughness", "travel_time_to_cities", "tree_age")
writeRaster(rst_fcm, paste0("data/processed/rast_fcm_", Sys.Date(), ".tif"), overwrite = TRUE)

# Scale the data
rst_fcm_sc <- scale(rst_fcm)

# Quickly investigate the correlation between the attributes
correlation <- layerCor(rst_fcm, "pearson", na.rm = TRUE)
fcm_cor <- as.data.frame(correlation$correlation)
#write_csv(fcm_cor, here::here("outputs/fcm_correlation.csv"), append = FALSE)

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
ggsave(here::here("figures/FCM_sil_idx.png"), sil.idx, 
       width = 12, height = 12, dpi = 300)

# plotting the explained inertia
ex.inert <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2) +
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
ex.inert
ggsave(here::here("figures/FCM_ex_inert.png"), ex.inert, 
       width = 12, height = 12, dpi = 300)

#---Run the FCM---- 
FCM_result <- CMeans(dataset, k = 5, m = 1.7, standardize = FALSE)
map.res <- rast(FCM_result$rasters)
writeRaster(map.res[["Groups"]], filename = paste0("data/processed/FCM_", Sys.Date(), ".tif"))


##---save the iteration, k, m as a dataframe----
aa_iteration <- data.frame(iteration_name = character(),
                           attris = character(),
                           k = numeric(),
                           m = numeric())

iteration_name <- "FCM_"
attris <- paste(names(rst_fcm), collapse= ", ")
k <- 5
m <- 1.7

aa_iteration[nrow(aa_iteration) + 1,] <- list(iteration_name, attris, k, m)

write_csv(aa_iteration, here::here("outputs/aa_iteration.csv"), append = TRUE)






