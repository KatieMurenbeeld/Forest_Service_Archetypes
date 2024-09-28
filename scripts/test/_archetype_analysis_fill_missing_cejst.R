## Note: This script, the original/usa.shp and the merged/conus_whp_* 
## were uploaded and run on Borah with minor changes to the code
## in the form of updates to the file path.

library(sf)
library(terra)
library(tidyverse)

projection = "epsg:5070"

#----Load the shapefiles and reference raster
# CEJST (Climate and Economic Justice Screening Tool)
cejst <- st_read(here::here("data/original/usa.shp"))

ref_rast <- rast(here::here("data/processed/merged/conus_whp_3km_agg_2024-08-09.tif"))
ref_rast_proj <- project(ref_rast, projection)

#----Transform the CEJST shapefile
cejst_proj <- cejst %>% st_transform(., crs = projection)

#----Filter non-CONUS entities and select variables----
cejst_proj <- cejst_proj %>%
  filter(SF != c("Hawaii", "Alaska", "Puerto Rico",
                 "Northern Mariana Islands", "Guam", "American Samoa"))

## test with Washington first
#cejst_proj <- cejst_proj %>%
#  filter(SF == "Washington")

cejst_proj_sel <- cejst_proj %>%
  select(HSEF, HBF_PFS, EBF_PFS, PM25F_PFS)

all(st_is_valid(cejst_proj_sel))
any(st_is_empty(cejst_proj_sel))

ggplot(data = cejst_proj_sel) + 
  geom_sf(aes(fill = HSEF, color = HSEF))

which(is.na(cejst_proj_sel$HSEF))

#---Fill in data for the CEJST dataset----

# create an index of the polygons that touch
index <- st_touches(cejst_proj_sel, cejst_proj_sel)

cejst_proj_fill <- cejst_proj_sel %>%
  mutate(HSEF_fill = ifelse(is.na(HSEF),
                            apply(wa_index, 1, function(i){mean(.$HSEF[i], na.rm = TRUE)}),
                            HSEF),
         HBF_PFS_fill = ifelse(is.na(HBF_PFS),
                               apply(wa_index, 1, function(i){mean(.$HBF_PFS[i], na.rm = TRUE)}),
                               HBF_PFS),
         EBF_PFS_fill = ifelse(is.na(EBF_PFS),
                               apply(wa_index, 1, function(i){mean(.$EBF_PFS[i], na.rm = TRUE)}),
                               EBF_PFS),
         PM25F_PFS_fill = ifelse(is.na(PM25F_PFS),
                                 apply(wa_index, 1, function(i){mean(.$PM25F_PFS[i], na.rm = TRUE)}),
                                 PM25F_PFS)
  )

cejst_proj_fill_noemp <- cejst_proj_fill %>% filter(!st_is_empty(.))

print(paste("Geometries are valid? ", all(st_is_valid(cejst_proj_fill))))
print(paste("There are empty geometries? ", any(st_is_empty(cejst_proj_fill_noemp))))

#----Save the filled in shapefile----
write_sf(obj = cejst_proj_fill_noemp, dsn = paste0(here::here("data/processed/"), "cejst_to_rst_", Sys.Date(), ".shp"), overwrite = TRUE, append = FALSE)
print("new shapefile written")
