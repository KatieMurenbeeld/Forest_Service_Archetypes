library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(distributional)
library(ggdist)
library(ggsci)
library(tigris)

# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")
aip <- st_read("data/processed/aip_vars_2024-08-02.shp")
#conus_attri_all <- rast("data/processed/rast_fcm_2024-05-29.tif") # raster stack given to FCM model
#map.conus <- rast("data/processed/FCM_2024-05-29.tif") # FCM results raster
fcm_all_attri <- rast("data/processed/rast_fcm_all_2024-07-22.tif")
fcm_all_result <- rast("data/processed/FCM_all_2024-07-22.tif") 
fcm_all <- readRDS("data/processed/FCM_all_2024-07-22.rds")
map.res.all <- rast(fcm_all$rasters)

fcm_pmrc_attri <- rast("data/processed/rast_fcm_pmrc2024_2024-07-22.tif")
fcm_pmrc_result <- rast("data/processed/FCM_pmrc_2024-07-22.tif")
fmc_pmrc <- readRDS("data/processed/FCM_pmrc_2024-07-22.rds")

fcm_pmrc_poli_attri <- rast("data/processed/rast_fcm_pmrc2024_ploi_2024-07-22.tif")
fcm_pmrc_poli_result <- rast("data/processed/FCM_pmrc_poli_2024-07-22.tif")
fcm_pmrc_poli <- readRDS("data/processed/FCM_pmrc_poli_2024-07-22.rds")

fcm_poli_nogs_attri <- rast("data/processed/rast_fcm_poli_nogs_2024-07-22.tif")
fcm_poli_nogs_result_01 <- rast("data/processed/FCM_poli_nogs_01_2024-07-22.tif")
fcm_poli_nogs_result_02 <- rast("data/processed/FCM_poli_nogs_02_2024-07-22.tif")
fcm_poli_nogs_1 <- readRDS("data/processed/FCM_poli_nogs_01_2024-07-22.rds")
fcm_poli_nogs_2 <- readRDS("data/processed/FCM_poli_nogs_02_2024-07-22.rds")

### Built in spider and violin plots
df_all <- as.data.frame(fcm_all_attri, na.rm = TRUE)
df_all_sc <- as.data.frame((fcm_all_attri - global(fcm_all_attri, "min", na.rm=TRUE)[,1])/(global(fcm_all_attri, "max", na.rm=TRUE)[,1] - global(fcm_all_attri, "min", na.rm=TRUE)[,1]),
                           na.rm = TRUE)

df_pmrc <- as.data.frame(fcm_pmrc_attri, na.rm = TRUE)
df_pmrc_sc <- as.data.frame((fcm_pmrc_attri - global(fcm_pmrc_attri, "min", na.rm=TRUE)[,1])/(global(fcm_pmrc_attri, "max", na.rm=TRUE)[,1] - global(fcm_pmrc_attri, "min", na.rm=TRUE)[,1]),
                           na.rm = TRUE)

df_pmrc_poli <- as.data.frame(fcm_pmrc_poli_attri, na.rm = TRUE)
df_pmrc_poli_sc <- as.data.frame((fcm_pmrc_poli_attri - global(fcm_pmrc_poli_attri, "min", na.rm=TRUE)[,1])/(global(fcm_pmrc_poli_attri, "max", na.rm=TRUE)[,1] - global(fcm_pmrc_poli_attri, "min", na.rm=TRUE)[,1]),
                            na.rm = TRUE)

df_poli_nogs <- as.data.frame(fcm_poli_nogs_attri, na.rm = TRUE)
df_poli_nogs_sc <- as.data.frame((fcm_poli_nogs_attri - global(fcm_poli_nogs_attri, "min", na.rm=TRUE)[,1])/(global(fcm_poli_nogs_attri, "max", na.rm=TRUE)[,1] - global(fcm_poli_nogs_attri, "min", na.rm=TRUE)[,1]),
                                 na.rm = TRUE)

spiderPlots(df_all_sc, fcm_all$Belongings, 
            chartcolors = c("darkorange3", "grey", "royalblue"))

spiderPlots(df_poli_nogs, fcm_poli_nogs_1$Belongings, 
            chartcolors = c("darkorange3", "grey", "royalblue"))

spiderPlots(df_poli_nogs, fcm_poli_nogs_2$Belongings, 
            chartcolors = c("darkorange3", "grey", "royalblue",
                            "goldenrod", "seagreen"))

spiderPlots(df_pmrc, fcm_pmrc$Belongings)

spiderPlots(df_pmrc_poli, fcm_pmrc_poli$Belongings)

violinPlots(df_all_sc, fcm_all$Groups)

violinPlots(df_pmrc_sc, fcm_pmrc$Groups)
pmrc_vplots_zsc <- violinPlots(scale(df_pmrc), fcm_pmrc$Groups)
pmrc_vplots_rs <- (pmrc_vplots_zsc[[17]] | pmrc_vplots_zsc[[8]] | pmrc_vplots_zsc[[11]]) /
  (pmrc_vplots_zsc[[13]] | pmrc_vplots_zsc[[14]] | pmrc_vplots_zsc[[15]])
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/pmrc_vplots_rs_", Sys.Date(), ".png"), 
       plot = pmrc_vplots_rs, 
       width = 18, height = 12, dpi = 300)  

pmrc_vplots_gs <- (pmrc_vplots_zsc[[9]] | pmrc_vplots_zsc[[10]] | pmrc_vplots_zsc[[19]])
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/pmrc_vplots_gs_", Sys.Date(), ".png"), 
       plot = pmrc_vplots_gs, 
       width = 18, height = 9, dpi = 300)

pmrc_vplots_ru_u <- (pmrc_vplots_zsc[[18]] | pmrc_vplots_zsc[[1]]) /
  (pmrc_vplots_zsc[[12]] | pmrc_vplots_zsc[[20]])
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/pmrc_vplots_ru_u_", Sys.Date(), ".png"), 
       plot = pmrc_vplots_ru_u, 
       width = 12, height = 12, dpi = 300)

pmrc_vplots_u <- (pmrc_vplots_zsc[[4]] | pmrc_vplots_zsc[[16]] | pmrc_vplots_zsc[[2]]) /
  (pmrc_vplots_zsc[[5]] | pmrc_vplots_zsc[[6]] | pmrc_vplots_zsc[[7]] | pmrc_vplots_zsc[[3]])
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/pmrc_vplots_u_", Sys.Date(), ".png"), 
       plot = pmrc_vplots_u, 
       width = 18, height = 12, dpi = 300)

violinPlots(df_pmrc_poli_sc, fcm_pmrc_poli$Groups)
violinPlots(scale(df_pmrc_poli), fcm_pmrc_poli$Groups)
pmrc_poli_vplots_zsc <- violinPlots(scale(df_pmrc_poli), fcm_pmrc_poli$Groups)

pmrc_poli_vplots_rs <- (pmrc_poli_vplots_zsc[[18]] | pmrc_poli_vplots_zsc[[8]] | pmrc_poli_vplots_zsc[[12]]) /
  (pmrc_poli_vplots_zsc[[14]] | pmrc_poli_vplots_zsc[[15]] | pmrc_poli_vplots_zsc[[16]])
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/pmrc_poli_vplots_rs_", Sys.Date(), ".png"), 
       plot = pmrc_poli_vplots_rs, 
       width = 18, height = 12, dpi = 300)  

pmrc_poli_vplots_gs <- (pmrc_poli_vplots_zsc[[10]] | pmrc_poli_vplots_zsc[[11]] | pmrc_poli_vplots_zsc[[20]])
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/pmrc_poli_vplots_gs_", Sys.Date(), ".png"), 
       plot = pmrc_poli_vplots_gs, 
       width = 18, height = 9, dpi = 300)  

pmrc_poli_vplots_ru_u <- (pmrc_poli_vplots_zsc[[19]] | pmrc_poli_vplots_zsc[[1]]) /
  (pmrc_poli_vplots_zsc[[13]] | pmrc_poli_vplots_zsc[[21]])
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/pmrc_poli_vplots_ru_u_", Sys.Date(), ".png"), 
       plot = pmrc_poli_vplots_ru_u, 
       width = 12, height = 12, dpi = 300)  

pmrc_poli_vplots_u <- (pmrc_poli_vplots_zsc[[4]] | pmrc_poli_vplots_zsc[[17]] | pmrc_poli_vplots_zsc[[2]] | pmrc_poli_vplots_zsc[[9]]) /
  (pmrc_poli_vplots_zsc[[5]] | pmrc_poli_vplots_zsc[[6]] | pmrc_poli_vplots_zsc[[7]] | pmrc_poli_vplots_zsc[[3]])
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/pmrc_poli_vplots_u_", Sys.Date(), ".png"), 
       plot = pmrc_poli_vplots_u, 
       width = 18, height = 12, dpi = 300)  

violinPlots(df_poli_nogs_sc, fcm_poli_nogs_1$Groups)

violinPlots(df_poli_nogs_sc, fcm_poli_nogs_2$Groups)

## Reproject the forest service shapes to NAD83
projection <- "epsg: 5070"

aip.proj <- aip %>% st_transform(., crs = projection)

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(fcm_pmrc_poli_attri))

# crop no_gs attributes to Idaho, California, Minnesota, and Alabama
states <- states(cb = TRUE)
states_proj <- states %>% st_transform(., crs = projection)
id_proj <- states_proj %>%
  filter(STUSPS == "ID")
ca_proj <- states_proj %>%
  filter(STUSPS == "CA")
al_proj <- states_proj %>%
  filter(STUSPS == "AL")
mn_proj <- states_proj %>%
  filter(STUSPS == "MN")

# get shape fil

## Check and rename the attributes in each raster stack
names(fcm_all_attri)

aip_county <- ggplot() + 
  geom_sf(data = aip.proj, aes(fill = mrp_dlg)) +
  scale_fill_gradient2(midpoint = 0.0)
ggsave(here::here("figures/aip_county.png"), aip_county)

aip_se_county <- ggplot() + 
  geom_sf(data = aip.proj, aes(fill = mrp_dl_)) + 
  scale_fill_gradient(low = "white", high = "red4")
ggsave(here::here("figures/aip_se_county.png"), aip_se_county)

## Create a map of the clusters with the National Forest boundaries
fcm.all.df <- fcm_all_result$Groups %>% as.data.frame(xy = TRUE)

fcm_nf_map <- ggplot() +
  geom_raster(aes(x = fcm.all.df$x, y = fcm.all.df$y, fill = as.factor(fcm.all.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black", linewidth = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "All Attributes: k=3, m=1.5", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

fcm_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/fcm_all_nf_map_", Sys.Date(), ".png"), plot = fcm_nf_map, width = 12, height = 12, dpi = 300)  

## Create a map of the clusters with the Region and National Forest boundaries

all_rg_nf_map <- ggplot() +
  geom_raster(aes(x = fcm.all.df$x, y = fcm.all.df$y, fill = as.factor(fcm.all.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "All Attributes: k=3, m=1.5", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

all_rg_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/fcm_all_reg_nf_map_", Sys.Date(), ".png"), plot = all_rg_nf_map, width = 12, height = 12, dpi = 300)  

# PMRC attributes
fcm.pmrc.df <- fcm_pmrc_result$Groups %>% as.data.frame(xy = TRUE)

pmrc_rg_nf_map <- ggplot() +
  geom_raster(aes(x = fcm.pmrc.df$x, y = fcm.pmrc.df$y, fill = as.factor(fcm.pmrc.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "PMRC Attributes: k=8, m=1.625", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

pmrc_rg_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/fcm_pmrc_reg_nf_map_", Sys.Date(), ".png"), plot = pmrc_rg_nf_map, width = 12, height = 12, dpi = 300)  

# PMRC Poli
fcm.pmrc.poli.df <- fcm_pmrc_poli_result$Groups %>% as.data.frame(xy = TRUE)

pmrc_poli_rg_nf_map <- ggplot() +
  geom_raster(aes(x = fcm.pmrc.poli.df$x, y = fcm.pmrc.poli.df$y, fill = as.factor(fcm.pmrc.poli.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "PMRC with AIP: k=8, m=1.625", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

pmrc_poli_rg_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/fcm_pmrc_poli_reg_nf_map_", Sys.Date(), ".png"), plot = pmrc_poli_rg_nf_map, width = 12, height = 12, dpi = 300)  

## Check the probability of belonging for each cluster
### using different undecided thresholds
maps1 <- mapClusters(object = fcm_pmrc_poli, undecided = 0.45)


# No GS Poli 01
fcm.poli.nogs.01.df <- fcm_poli_nogs_result_01$Groups %>% as.data.frame(xy = TRUE)

poli_nogs_01_rg_nf_map <- ggplot() +
  geom_raster(aes(x = fcm.poli.nogs.01.df$x, y = fcm.poli.nogs.01.df$y, fill = as.factor(fcm.poli.nogs.01.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "No GS with AIP v1: k=3, m=1.625", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

poli_nogs_01_rg_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/fcm_poli_nogs_v01_reg_nf_map_", Sys.Date(), ".png"), plot = poli_nogs_01_rg_nf_map, width = 12, height = 12, dpi = 300)  

# No GS Poli 02
fcm.poli.nogs.02.df <- fcm_poli_nogs_result_02$Groups %>% as.data.frame(xy = TRUE)

poli_nogs_02_rg_nf_map <- ggplot() +
  geom_raster(aes(x = fcm.poli.nogs.02.df$x, y = fcm.poli.nogs.02.df$y, fill = as.factor(fcm.poli.nogs.02.df$Groups))) +
  geom_sf(data = fs_nf.proj, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "No GS with AIP v2: k=5, m=1.5", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))

poli_nogs_02_rg_nf_map
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/fcm_poli_nogs_v02_reg_nf_map_", Sys.Date(), ".png"), plot = poli_nogs_02_rg_nf_map, width = 12, height = 12, dpi = 300)  


# Maps for Idaho, California, Minnesota, Alabama
id_fcm_result <- crop(fcm_no_gs_result, id_proj, mask = TRUE)
id_nf_int <- st_intersection(fs_nf.proj, id_proj, mask = TRUE)
ca_fcm_result <- crop(fcm_no_gs_result, ca_proj, mask = TRUE)
ca_nf_int <- st_intersection(fs_nf.proj, ca_proj, mask = TRUE)
mn_fcm_result <- crop(fcm_no_gs_result, mn_proj, mask = TRUE)
mn_nf_int <- st_intersection(fs_nf.proj, mn_proj, mask = TRUE)
al_fcm_result <- crop(fcm_no_gs_result, al_proj, mask = TRUE)
al_nf_int <- st_intersection(fs_nf.proj, al_proj, mask = TRUE)

fcm.id <- id_fcm_result$Groups %>% as.data.frame(xy = TRUE)

fcm_id_map <- ggplot() +
  geom_raster(aes(x = fcm.id$x, y = fcm.id$y, fill = as.factor(fcm.id$Groups))) +
  geom_sf(data = id_proj, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = id_nf_int, fill = NA, color = "black", linewidth = 1.5) +
  geom_sf(data = id_nf_int %>% filter(FORESTORGC == "0402"), fill = NA, color = "red", linewidth = 1.75) + 
  scale_fill_brewer(palette = "Set2") +
  labs(title = "FCM SES Attributes: k=5, m=1.875",
       subtitle = "Idaho: Boise National Forest",
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

fcm_id_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_id_nf_map_", Sys.Date(), ".png"), plot = fcm_id_map, width = 12, height = 12, dpi = 300)  

fcm.ca <- ca_fcm_result$Groups %>% as.data.frame(xy = TRUE)

fcm_ca_map <- ggplot() +
  geom_raster(aes(x = fcm.ca$x, y = fcm.ca$y, fill = as.factor(fcm.ca$Groups))) +
  geom_sf(data = ca_proj, fill = NA, color = "black",  linewidth = 1) +
  geom_sf(data = ca_nf_int, fill = NA, color = "black",  linewidth = 1.5) +
  geom_sf(data = ca_nf_int %>% filter(FORESTORGC == "0511"), fill = NA, color = "red",  linewidth = 1.75) + 
  scale_fill_brewer(palette = "Set2") +
  labs(title = "FCM SES Attributes: k=5, m=1.875",
       subtitle = "California: Plumas National Forest",
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

fcm_ca_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_ca_nf_map_", Sys.Date(), ".png"), plot = fcm_ca_map, width = 12, height = 12, dpi = 300)  

fcm.mn <- mn_fcm_result$Groups %>% as.data.frame(xy = TRUE)

fcm_mn_map <- ggplot() +
  geom_raster(aes(x = fcm.mn$x, y = fcm.mn$y, fill = as.factor(fcm.mn$Groups))) +
  geom_sf(data = mn_proj, fill = NA, color = "black",  linewidth = 1) +
  geom_sf(data = mn_nf_int, fill = NA, color = "black", linewidth = 1.5) +
  geom_sf(data = mn_nf_int %>% filter(FORESTORGC == "0909"), fill = NA, color = "red", linewidth = 1.75) + 
  scale_fill_brewer(palette = "Set2") +
  labs(title = "FCM SES Attributes: k=5, m=1.875",
       subtitle = "Minnesota: Superior National Forest", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

fcm_mn_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_mn_nf_map_", Sys.Date(), ".png"), plot = fcm_mn_map, width = 12, height = 12, dpi = 300)  


fcm.al <- al_fcm_result$Groups %>% as.data.frame(xy = TRUE)

fcm_al_map <- ggplot() +
  geom_raster(aes(x = fcm.al$x, y = fcm.al$y, fill = as.factor(fcm.al$Groups))) +
  geom_sf(data = al_proj, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = al_nf_int, fill = NA, color = "black", linewidth = 1.5) +
  geom_sf(data = al_nf_int %>% filter(FORESTORGC == "0801"), fill = NA, color = "red", linewidth = 1.75) + 
  scale_fill_manual(values = c("#8DA0CB", "#E78AC3")) + 
  labs(title = "FCM SES Attributes: k=5, m=1.875", 
       subtitle = "Alabama: Alabama National Forests",
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

fcm_al_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_al_nf_map_", Sys.Date(), ".png"), plot = fcm_al_map, width = 12, height = 12, dpi = 300)  

### Create a patchwork of the 4 state maps

patch1 <- fcm_id_map + fcm_ca_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_states_patch1_", Sys.Date(), ".png"), plot = patch1, width = 12, height = 12, dpi = 300)  

### create simplified state maps and combine into patch
simp_al_map <- ggplot() +
  geom_raster(aes(x = fcm.al$x, y = fcm.al$y, fill = as.factor(fcm.al$Groups))) +
  geom_sf(data = al_proj, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = al_nf_int, fill = NA, color = "black", linewidth = 1.5) +
  geom_sf(data = al_nf_int %>% filter(FORESTORGC == "0801"), fill = NA, color = "red", linewidth = 1.75) + 
  scale_fill_manual(values = c("#8DA0CB", "#E78AC3")) + 
  labs(title = "D) Alabama", 
       subtitle = "Alabama National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 18),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"))
simp_al_map

simp_id_map <- ggplot() +
  geom_raster(aes(x = fcm.id$x, y = fcm.id$y, fill = as.factor(fcm.id$Groups))) +
  geom_sf(data = id_proj, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = id_nf_int, fill = NA, color = "black", linewidth = 1.5) +
  geom_sf(data = id_nf_int %>% filter(FORESTORGC == "0402"), fill = NA, color = "red", linewidth = 1.75) + 
  scale_fill_brewer(palette = "Set2") +
  labs(title = "A) Idaho", 
       subtitle = "Boise National Forest") +
  theme_bw() + 
  theme(text = element_text(size = 18),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"))

simp_id_map

simp_ca_map <- ggplot() +
  geom_raster(aes(x = fcm.ca$x, y = fcm.ca$y, fill = as.factor(fcm.ca$Groups))) +
  geom_sf(data = ca_proj, fill = NA, color = "black",  linewidth = 1) +
  geom_sf(data = ca_nf_int, fill = NA, color = "black",  linewidth = 1.5) +
  geom_sf(data = ca_nf_int %>% filter(FORESTORGC == "0511"), fill = NA, color = "red",  linewidth = 1.75) + 
  scale_fill_brewer(palette = "Set2") +
  labs(title = "B) California",
       subtitle = "Plumas National Forest",
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 18),
        legend.position = "right",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"))

simp_ca_map

simp_mn_map <- ggplot() +
  geom_raster(aes(x = fcm.mn$x, y = fcm.mn$y, fill = as.factor(fcm.mn$Groups))) +
  geom_sf(data = mn_proj, fill = NA, color = "black",  linewidth = 1) +
  geom_sf(data = mn_nf_int, fill = NA, color = "black", linewidth = 1.5) +
  geom_sf(data = mn_nf_int %>% filter(FORESTORGC == "0909"), fill = NA, color = "red", linewidth = 1.75) + 
  scale_fill_brewer(palette = "Set2") +
  labs(title = "C) Minnesota",
       subtitle = "Superior National Forest") +
  theme_bw() + 
  theme(text = element_text(size = 18),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"))

simp_mn_map

patch2 <- simp_id_map + simp_ca_map + simp_mn_map + simp_al_map +
  plot_annotation(theme = theme(plot.title = element_text(size = 22)))
patch3 <- fcm_reg_nf_map / (simp_id_map + simp_mn_map + simp_al_map) +
  plot_layout(heights = c(0.5)) 

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_states_patch2_", Sys.Date(), ".png"), plot = patch2, width = 12, height = 12, dpi = 300)
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_states_patch3_", Sys.Date(), ".png"), plot = patch3, width = 20, height = 15, dpi = 300)

### Create map of archetypes with the Forest Region boundaries
fcm.all.df <- fcm_all_result$Groups %>% as.data.frame(xy = TRUE)
fcm_all_reg_map <- ggplot() +
  geom_raster(aes(x = fcm.all.df$x, y = fcm.all.df$y, fill = as.factor(fcm.all.df$Groups))) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", size = 150) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "FCM All Attributes: k=5, m=1.875", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
fcm_all_reg_map
#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_all_reg_map_", Sys.Date(), ".png"), 
#       plot = fcm_all_reg_map, width = 12, height = 12, dpi = 300)

fcm.no.gs.df <- fcm_no_gs_result$Groups %>% as.data.frame(xy = TRUE)
fcm_no_gs_reg_map <- ggplot() +
  geom_raster(aes(x = fcm.no.gs.df$x, y = fcm.no.gs.df$y, fill = as.factor(fcm.no.gs.df$Groups))) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "FCM SES Attributes: k=5, m=1.875", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
fcm_no_gs_reg_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_no_gs_reg_map_nn", Sys.Date(), ".png"), 
       plot = fcm_no_gs_reg_map, width = 12, height = 12, dpi = 300)

fcm.rursu01.df <- fcm_rursu01_result$Groups %>% as.data.frame(xy = TRUE)
fcm_rursu01_reg_map <- ggplot() +
  geom_raster(aes(x = fcm.rursu01.df$x, y = fcm.rursu01.df$y, fill = as.factor(fcm.rursu01.df$Groups))) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", size = 150) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "FCM RU-RS-U(A) Attributes: k=7, m=1.625", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
fcm_rursu01_reg_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_rursu01_reg_map_", Sys.Date(), ".png"), 
       plot = fcm_rursu01_reg_map, width = 12, height = 12, dpi = 300)

fcm.eco.df <- fcm_eco_result$Groups %>% as.data.frame(xy = TRUE)
fcm_eco_reg_map <- ggplot() +
  geom_raster(aes(x = fcm.eco.df$x, y = fcm.eco.df$y, fill = as.factor(fcm.eco.df$Groups))) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", size = 150) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "FCM ECO Attributes: k=2, m=1.125", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
fcm_eco_reg_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_eco_reg_map_", Sys.Date(), ".png"), 
       plot = fcm_eco_reg_map, width = 12, height = 12, dpi = 300)

## Create Spider Plots or Violin Plots of the Attribute Distribution
### Read in the rds of the fcm objects
fcm_all_model <- readRDS("data/processed/FCM_all_2024-06-20.rds")
fcm_no_gs_model <- readRDS("data/processed/FCM_no_gs_2024-06-20.rds")
fcm_rursu01_model <- readRDS("data/processed/FCM_ru_rs_u_01_2024-06-20.rds")
fcm_eco_model <- readRDS("data/processed/FCM_eco_2024-06-21.rds")

df_all <- as.data.frame(fcm_all_attri, na.rm = TRUE)
vplots_all <- violinPlots(df_all, fcm_all_model$Groups)
spiderPlots(df_all, fcm_all_model$Belongings)
vplots_all[[4]]
splots_all[[4]]

fcm_no_gs_attri_sc <- scale(fcm_no_gs_attri)
df_no_gs <- as.data.frame(fcm_no_gs_attri, na.rm = TRUE)
vplots_no_gs <- violinPlots(df_no_gs, fcm_no_gs_model$Groups)


vplots_no_gs[[4]] +
  scale_x_discrete(labels=c("Arch 1", "Arch 2", 
                            "Arch 3", "Arch 4",
                            "Arch 5")) + 
  scale_fill_brewer(labels=c("Arch 1", "Arch 2", 
                             "Arch 3", "Arch 4",
                             "Arch 5"),
                    palette = "Set2") +
  geom_violin() + 
  labs(title = fcm_no_gs_attri_labs[4], 
       fill = "Archetype") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16))

for ( i in 1:20){
  print(i)
  vplots_no_gs_tmp <- vplots_no_gs[[i]] +
    scale_x_discrete(labels=c("Arch 1", "Arch 2", 
                              "Arch 3", "Arch 4",
                              "Arch 5")) + 
    scale_fill_brewer(labels=c("Arch 1", "Arch 2", 
                               "Arch 3", "Arch 4",
                               "Arch 5"),
                      palette = "Set2") +
    geom_violin() + 
    labs(title = fcm_no_gs_attri_labs[i], 
         fill = "Archetype") +
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 16))
  ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_no_gs_vplots_", Sys.Date(), i, "_labels.png"), 
         plot = vplots_no_gs_tmp, width = 12, height = 12, dpi = 300)
}

df_eco <- as.data.frame(fcm_eco_attri, na.rm = TRUE)
vplots_eco <- violinPlots(df_eco, fcm_eco_model$Groups)

for ( i in 1:5){
  print(i)
  vplots_eco_tmp <- vplots_eco[[i]] +
    scale_x_discrete(labels=c("Arch 1", "Arch 2")) + 
    scale_fill_brewer(labels=c("Arch 1", "Arch 2"),
                      palette = "Set2") +
    geom_violin() + 
    labs(title = fcm_eco_attri_labs[i], 
         fill = "Archetype") +
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 16))
  ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_eco_vplots_", Sys.Date(), i, "_labels.png"), 
         plot = vplots_eco_tmp, width = 12, height = 12, dpi = 300)
}

df_rursu1 <- as.data.frame(fcm_rursu01_attri, na.rm = TRUE)
vplots_rursu1 <- violinPlots(df_rursu1, fcm_rursu01_model$Groups)
vplots_rursu1[[5]]

for ( i in 1:10){
  print(i)
  vplots_no_gs[[i]] <- vplots_no_gs[[i]] +
    scale_x_discrete(labels=c("Arch 1", "Arch 2",
                              "Arch 3", "Arch 4",
                              "Arch 5", "Arch 6",
                              "Arch 7")) + 
    scale_fill_brewer(labels=c("Arch 1", "Arch 2",
                               "Arch 3", "Arch 4",
                               "Arch 5", "Arch 6",
                               "Arch 7"),
                      palette = "Set2") +
    geom_violin() + 
    labs(title = fcm_rursu01_attri_labs[i], 
         fill = "Archetype") +
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 16))
  ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_rursu1_vplots_", Sys.Date(), i, "_labels.png"), 
         plot = vplots_rursu1[[i]], width = 12, height = 12, dpi = 300)
}

## Create maps of the attributes
rrl.df <- conus_attri$rrlurb %>% as.data.frame(xy = TRUE)

rrl_map <- ggplot() +
  geom_raster(aes(x = rrl.df$x, y = rrl.df$y, fill = rrl.df$RUCC_20)) +
  geom_sf(data = fs_reg.proj, fill = NA, color = "black", size = 2) +
  scale_fill_viridis("Rural-Urban Continuum", option = "plasma", alpha = 0.5) +
  labs(title = "Rural-Urban Continuum", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size=22), 
        legend.title = element_text(size = 12),
        legend.position = "bottom")
rrl_map

#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/rrl__conus_map_iter005", Sys.Date(), ".png"), plot = rrl_map, width = 12, height = 12, dpi = 300)


## Use patch to create a figure with all of the attribute maps
#patch_test <- rrl_map + 
#patch_test

#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/patch_test_", Sys.Date(), ".png"), plot = patch_test, width = 25, height = 10)


## Create a stat_pointinterval plot
names(conus_attri) 
#<- c("RUCC_20", "pct_pay", "NAT_AME", "R_NET_M", "WHP", "last")

all.vals <- c(map.conus[["Groups"]], conus_attri)

vals <- as.data.frame(values(all.vals, na.rm = TRUE, data.frame = TRUE)) 

# scale the values before pivot_longer
vals$net_mig <- scale(vals$net_mig)
vals$pct_for_pay <- scale(vals$pct_for_pay)
vals$pct_sight_pay <- scale(vals$pct_sight_pay)
vals$pct_gov_p <- scale(vals$pct_gov_p)
vals$ave_dem <- scale(vals$ave_dem)
vals$less_coll <- scale(vals$less_coll)
vals$WHP <- scale(vals$WHP)
vals$geophysical <- scale(vals$geophysical)
vals$distance_to_wilderness_m <- scale(vals$distance_to_wilderness_m)
vals$distance_to_mill_m <- scale(vals$distance_to_mill_m)
#vals$LMI_PFS <- scale(vals$LMI_PFS)
#vals$av_vt_n <- scale(vals$av_vt_n)
#vals$pct_pay <- scale(vals$pct_pay)

colnames(vals) <- c("group", "net_mig", "pct_for_pay", "pct_sight_pay",
                    "pct_gov_pay", "ave_dem", "less_coll", "WHP", "geophysical",
                    "dist_to_wild_m", "dist_to_mill_m")

vals.df <- as.data.frame(vals) %>%
  pivot_longer(., net_mig:dist_to_mill_m, names_to = "variable", values_to = "val")

# not sure if this part is needed?
vals.df.2 <- vals.df %>% 
  mutate(., cluster = str_remove(variable, " "))

vals.df.sum <- vals.df %>%
  group_by(variable) %>%
  filter(., val > quantile(val, probs = 0.05) & val < quantile(val, probs = 0.95))

## Make the multiple-interval plot
theme_set(theme_ggdist())

multi.int.plot <- ggplot(data = vals.df.sum, aes(x = val, y = variable, color = as.factor(group))) +
  stat_pointinterval(position = "dodge") +
  scale_color_brewer(palette = "Set2") 
multi.int.plot

ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_08_multi_point_plot_", Sys.Date(), ".png"), plot = multi.int.plot, width = 12, height = 15, dpi = 300)  

## Make a different plot to look at distribution of attribute values in clusters
vals.df.sum2 <- vals.df %>% 
  group_by(variable, group) %>% 
  median_qi(.width = c(.5, .8, .95))

fcm.hist <- ggplot(data= vals.df.sum2, mapping=aes(
  x = fct_rev(variable), y = val, 
  ymin = .lower, ymax = .upper,
  # size = -.width means smaller probability interval => thicker line
  # this can be omitted, geom_pointinterval includes it automatically
  # if a .width column is in the input data.
  linewidth = 1, color=as.factor(group), alpha=0.5
)) +  
  geom_interval(show.legend = FALSE) +
  scale_color_brewer(palette = "Set2") + 
  scale_alpha_manual(values=c(1,0.7, 0.4), aesthetic="interval_alpha") +
  scale_y_continuous()+
  scale_x_discrete()+
  ylab("Standardized Value") +
  xlab("Variable") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), aspect.ratio = 0.5)+
  facet_wrap(vars(group), scales= "free_y", ncol=1, strip.position = "left")
fcm.hist

ggsave(here::here("figures/fcm_08_attri_hist_free_y.png"), fcm.hist,
       width = 15, height = 20, dpi = 300)




