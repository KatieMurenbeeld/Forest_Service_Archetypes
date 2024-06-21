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

# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")
#conus_attri_all <- rast("data/processed/rast_fcm_2024-05-29.tif") # raster stack given to FCM model
#map.conus <- rast("data/processed/FCM_2024-05-29.tif") # FCM results raster
fcm_all_attri <- rast("data/processed/rast_fcm_all_2024-06-20.tif")
fcm_all_result <- rast("data/processed/FCM_all_2024-06-20.tif") 

fcm_no_gs_attri <- rast("data/processed/rast_fcm_no_gs_2024-06-20.tif")
fcm_no_gs_result <- rast("data/processed/FCM_no_gs_2024-06-20.tif")

fcm_rursu01_attri <- rast("data/processed/rast_fcm_ru_rs_u2024-06-20.tif")
fcm_rursu01_result <- rast("data/processed/FCM_ru_rs_u_01_2024-06-20.tif")

fcm_eco_attri <- rast("data/processed/rast_fcm_eco2024-06-21.tif")
fcm_eco_result <- rast("data/processed/FCM_eco_2024-06-21.tif")

## Reproject the forest service shapes to NAD83
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(fcm_all_attri))

## Create a map of the clusters with the National Forest boundaries
group.df <- map.conus$Groups %>% as.data.frame(xy = TRUE)

#fcm_nf_map <- ggplot() +
#  geom_raster(aes(x = group.df$x, y = group.df$y, fill = as.factor(group.df$Groups))) +
#  geom_sf(data = fs_nf.proj, fill = NA, color = "black", size = 2) +
#  scale_fill_brewer(palette = "Set2") +
#  labs(title = "Fuzzy Cluster Map: k=5, m=1.7") +
#  theme(legend.position = "bottom")

#fcm_nf_map
#ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_nf_map_", Sys.Date(), ".png"), plot = fcm_nf_map, width = 12, height = 12, dpi = 300)  

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
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_all_reg_map_", Sys.Date(), ".png"), 
       plot = fcm_all_reg_map, width = 12, height = 12, dpi = 300)

fcm.no.gs.df <- fcm_no_gs_result$Groups %>% as.data.frame(xy = TRUE)
fcm_no_gs_reg_map <- ggplot() +
  geom_raster(aes(x = fcm.no.gs.df$x, y = fcm.no.gs.df$y, fill = as.factor(fcm.no.gs.df$Groups))) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", size = 150) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "FCM No GS Attributes: k=5, m=1.875", 
       fill = "Archetypes") +
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
fcm_no_gs_reg_map
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_no_gs_reg_map_", Sys.Date(), ".png"), 
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

df_no_gs <- as.data.frame(fcm_no_gs_attri, na.rm = TRUE)
vplots_no_gs <- violinPlots(df_no_gs, fcm_no_gs_model$Groups)
#vplots_no_gs

df_eco <- as.data.frame(fcm_eco_attri, na.rm = TRUE)
vplots_eco <- violinPlots(df_eco, fcm_eco_model$Groups)
vplots_eco

df_rursu1 <- as.data.frame(fcm_rursu01_attri, na.rm = TRUE)
vplots_rursu1 <- violinPlots(df_rursu1, fcm_rursu01_model$Groups)
vplots_rursu1

for (i in 1:5){
  print(i)
  ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_eco_vplots_", Sys.Date(), i, ".png"), 
         plot = vplots_eco[[i]], width = 12, height = 12, dpi = 300)
}

for (i in 1:10){
  print(i)
  ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_rursu1_vplots_", Sys.Date(), i, ".png"), 
         plot = vplots_rursu1[[i]], width = 12, height = 12, dpi = 300)
}

for (i in 1:20){
  print(i)
  ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_no_gs_vplots_", Sys.Date(), i, ".png"), 
         plot = vplots_no_gs[[i]], width = 12, height = 12, dpi = 300)
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




