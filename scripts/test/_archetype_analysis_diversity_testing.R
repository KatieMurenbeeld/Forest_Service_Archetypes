library(tidyverse)
library(terra)
library(sf)
library(raster)
library(ggplot2)
library(exactextractr)
library(tigris)
library(gganimate)
library(gifski)
library(exactextractr)

# 3 things to do in this script
## 1 Need to calculate Shannon Diversity by Region
## 2 Sensitivity Analysis of buffer distances
## 3 Test different ways to calculate diversity 
## 4 Test out what different levels of mixing does for Shannon H 
### (create a sample raster and evenly assign the value of 8 clusters)


# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")

sgfcm_pmrc_poli_attri_sc <- rast(here::here("data/processed/rst_fcm_pmrc_poli_sc_2024-08-12.tif"))
sgfcm_pmrc_poli_attri <- rast(here::here("data/processed/rast_fcm_pmrc2024_ploi_2024-08-12.tif"))
sgfmc_pmrc_poli_result <- rast(here::here("data/processed/SGFCM_result_pmrc_poli_2024-08-26.tif"))
sgfcm_pmrc_poli <- readRDS(here::here("data/processed/SGFCM_result_pmrc_poli_2024-08-26.rds"))

## Reproject the forest service shapes and crop
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)

fs_nf.crop <- st_crop(fs_nf.proj, ext(sgfcm_pmrc_poli_attri))

fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)

fs_reg.crop <- st_crop(fs_reg.proj, ext(sgfcm_pmrc_poli_attri))

## 1. Need to calculate Shannon Diversity by Region

## Test for Region 4

reg4 <- fs_reg.crop %>%
  filter(REGION == "04")



test_v <- reg4
test_z <- crop(sgfmc_pmrc_poli_result, test_v, mask = TRUE)

test_x <- exact_extract(test_z, test_v, coverage_area = TRUE)
names(test_x) <- test_v$REGIONNAME

test <- bind_rows(test_x, .id = "REGIONNAME") %>%
  group_by(REGIONNAME, value) %>%
  #group_by(value) %>%
  summarize(total_area = sum(coverage_area)) %>%
  group_by(REGIONNAME) %>%
  mutate(proportion = total_area/sum(total_area))

test <- test %>% 
  replace_na(list(value = 0))

test_shan <- test %>%
  select(REGIONNAME, proportion) %>%
  #select(proportion) %>%
  group_by(REGIONNAME) %>%
  summarise(shan_div = -sum(proportion * log(proportion)))

# join to sf!
test_shan_sf <- test_shan %>%
  left_join(reg4, by = "REGIONNAME")

test_shan_sf <- st_as_sf(test_shan_sf)

# and plot
sgfcm_reg4 <- crop(sgfmc_pmrc_poli_result, reg4)

sgfcm.id <- sgfcm_reg4$Groups %>% as.data.frame(xy = TRUE)

test_shan_reg4 <- ggplot() +
  #geom_raster(aes(x = fcm.id$x, y = fcm.id$y, fill = as.factor(fcm.id$Groups))) +
  geom_sf(data = reg4, fill = NA, color = "black") +
  geom_sf(data = test_shan_sf, aes(fill = shan_div)) +
  #geom_sf(data = reg4_union_dif, fill = NA, color = "black") +
  #scale_fill_brewer(palette = "Set2") +
  labs(title = "Region 4: Shannon Diversity of Archetypes") +
       #subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
test_shan_reg4  

# 2 sensitivity analysis of buffer distances

# Union all of the national forests
nf_union <- st_union(fs_nf.crop)

# From the unioned forests create an overall buffer and get the "donut"
nf_union_buf_10 <- st_buffer(nf_union, dist = 10000)
nf_union_dif_10 <- st_difference(nf_union_buf_10, nf_union)

nf_union_buf_20 <- st_buffer(nf_union, dist = 20000)
nf_union_dif_20 <- st_difference(nf_union_buf_20, nf_union)

nf_union_buf_30 <- st_buffer(nf_union, dist = 30000)
nf_union_dif_30 <- st_difference(nf_union_buf_30, nf_union)

nf_union_buf_40 <- st_buffer(nf_union, dist = 40000)
nf_union_dif_40 <- st_difference(nf_union_buf_40, nf_union)

nf_union_buf_50 <- st_buffer(nf_union, dist = 50000)
nf_union_dif_50 <- st_difference(nf_union_buf_50, nf_union)

nf_union_buf_60 <- st_buffer(nf_union, dist = 60000)
nf_union_dif_60 <- st_difference(nf_union_buf_60, nf_union)

nf_union_buf_70 <- st_buffer(nf_union, dist = 70000)
nf_union_dif_70 <- st_difference(nf_union_buf_70, nf_union)

nf_union_buf_80 <- st_buffer(nf_union, dist = 80000)
nf_union_dif_80 <- st_difference(nf_union_buf_80, nf_union)

nf_union_buf_90 <- st_buffer(nf_union, dist = 90000)
nf_union_dif_90 <- st_difference(nf_union_buf_90, nf_union)

nf_union_buf_100 <- st_buffer(nf_union, dist = 100000)
nf_union_dif_100 <- st_difference(nf_union_buf_100, nf_union)


# define a function
nf_buffers <- function(area_with_nf, area_with_nf_buf_dif, dist_m){
  nf_buffs <- data.frame()
  for (nf in 1:109) {
    #print(reg4_nf$FORESTORGC[nf])
    tmp_nf <- area_with_nf %>%
      filter(FORESTORGC == area_with_nf$FORESTORGC[nf])
    tmp_nf_buf <- st_buffer(tmp_nf, dist = dist_m)
    tmp_nf_int <- st_intersection(tmp_nf_buf, area_with_nf_buf_dif)
    nf_buffs <- rbind(nf_buffs, tmp_nf_int)
  }
  return(nf_buffs)
}

buffers_10 <- nf_buffers(fs_nf.crop, nf_union_dif_10, 10000)  
buffers_20 <- nf_buffers(fs_nf.crop, nf_union_dif_20, 20000)
buffers_30 <- nf_buffers(fs_nf.crop, nf_union_dif_30, 30000)
buffers_40 <- nf_buffers(fs_nf.crop, nf_union_dif_40, 40000)
buffers_50 <- nf_buffers(fs_nf.crop, nf_union_dif_50, 50000)
buffers_60 <- nf_buffers(fs_nf.crop, nf_union_dif_60, 60000)
buffers_70 <- nf_buffers(fs_nf.crop, nf_union_dif_70, 70000)
buffers_80 <- nf_buffers(fs_nf.crop, nf_union_dif_80, 80000)
buffers_90 <- nf_buffers(fs_nf.crop, nf_union_dif_90, 90000)
buffers_100 <- nf_buffers(fs_nf.crop, nf_union_dif_100, 100000)

# Calculate a diversity metric for the archetypes within the buffer zone
## crop archetype attributes and fcm results to the buffer shape
## calculate the proportion of the area covered by each archetype (area archetype / total area)
## using the proportions, calculate Shannon Diversity Index (-sum(species_proportions * log(species_proportions)))
## want to do this for the Regions as well 

buff <- buffers_100

v <- buff %>% st_cast("MULTIPOLYGON")
z <- crop(sgfmc_pmrc_poli_result, v, mask = TRUE)

x <- exact_extract(z, v, coverage_area = TRUE)
names(x) <- v$FORESTORGC

areas <- bind_rows(x, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area))) %>%
  mutate(distance = 100)

areas <- areas %>% 
  replace_na(list(value = 0))

areas_100 <- areas
areas_90 <- areas
areas_80 <- areas
areas_70 <- areas
areas_60 <- areas
areas_50 <- areas
areas_40 <- areas
areas_30 <- areas
areas_20 <- areas
areas_10 <- areas


shan_h <- function(areas, dist_m) {
  shan_h <- areas %>%
    dplyr::select(FORESTORGC, proportion) %>%
    group_by(FORESTORGC) %>%
    summarise(shan_div = -sum(proportion * log(proportion))) %>%
    mutate(distance = dist_m)
  return (shan_h)
}

shan_h_10 <- shan_h(areas_10, 10)
shan_h_20 <- shan_h(areas_20, 20)
shan_h_30 <- shan_h(areas_30, 30)
shan_h_40 <- shan_h(areas_40, 40)
shan_h_50 <- shan_h(areas_50, 50)
shan_h_60 <- shan_h(areas_60, 60)
shan_h_70 <- shan_h(areas_70, 70)
shan_h_80 <- shan_h(areas_80, 80)
shan_h_90 <- shan_h(areas_90, 90)
shan_h_100 <- shan_h(areas_100, 100)

all_shan_h <- rbind(shan_h_10, shan_h_20, shan_h_30, shan_h_40, shan_h_50,
                     shan_h_60, shan_h_70, shan_h_80 ,shan_h_90 ,shan_h_100)
write_csv(all_shan_h, paste0(here::here("data/processed/"), "all_shan_div_dist10-100_", Sys.Date(), ".csv"))
#all_shan_h <- read_csv(here::here("data/processed/all_shan_div_dist10-100_2024-08-26.csv"))

# join to sf!
shan_h_sf <- function(shan_h){
  shan_h_sf <- shan_h %>%
  left_join(fs_nf.crop, by = "FORESTORGC")
  
  shan_h_sf <- st_as_sf(shan_h_sf)
  return(shan_h_sf)
}

shan_h_sf_10 <- shan_h_sf(shan_h_10)
shan_h_sf_20 <- shan_h_sf(shan_h_20)
shan_h_sf_30 <- shan_h_sf(shan_h_30)
shan_h_sf_40 <- shan_h_sf(shan_h_40)
shan_h_sf_50 <- shan_h_sf(shan_h_50)
shan_h_sf_60 <- shan_h_sf(shan_h_60)
shan_h_sf_70 <- shan_h_sf(shan_h_70)
shan_h_sf_80 <- shan_h_sf(shan_h_80)
shan_h_sf_90 <- shan_h_sf(shan_h_90)
shan_h_sf_100 <- shan_h_sf(shan_h_100)

all_shan_h_sf <- rbind(shan_h_sf_10, shan_h_sf_20, shan_h_sf_30, shan_h_sf_40, 
                       shan_h_sf_50, shan_h_sf_60, shan_h_sf_70, shan_h_sf_80, 
                       shan_h_sf_90, shan_h_sf_100)
write_sf(obj = all_shan_h_sf, dsn = paste0(here::here("data/processed/"), "all_shan_div_dist10-100_", Sys.Date(), ".shp"), overwrite = TRUE, append = FALSE)
#all_shan_h_sf <- read_sf(here::here("data/processed/all_shan_div_dist10-100_2024-08-26.shp"))

# Create an animated map?
# create the list of CONUS states 
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS
states <- tigris::states(cb = TRUE)

states <- states %>%
  filter(STUSPS %in% continental.states$state)

#----animate the map with the different buffer distances----
base_map <- ggplot() + 
  geom_sf(data = states, fill = "white", color = "black") +
  theme_void()
base_map


map_with_data <- base_map +
  geom_sf(data = all_shan_h_sf, aes(color = shan_div, 
                                    fill = shan_div, 
                                    group = distance))
map_with_data

map_with_animation <- map_with_data +
  transition_time(distance) +
  ggtitle('Distance: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
num_distances <- max(all_shan_h_sf$distance)/10 - min(all_shan_h_sf$distance)/10 + 1
animate(map_with_animation, nframes = num_distances)

#anim_save(here::here("outputs/example1.gif"), map_with_animation, nframes = num_distances, fps = 2)
animate(map_with_animation, duration = 10, fps = 1, renderer = gifski_renderer())
anim_save(here::here("figures/output.gif"))
#----------
# scatter/line plot with shan_div by distance for each forest?
#all_shan_h <- read_csv(here::here("data/processed/all_shan_div_dist10-100_2024-08-26.csv"))
dist_shan_sens <- all_shan_h %>%
  filter(shan_div > 1.5) %>%
  ggplot(aes(x=distance, y=shan_div, group=FORESTORGC, color=FORESTORGC)) +
  geom_line() + 
  scale_x_continuous(breaks = seq(10, 110, by = 10)) +
  theme_bw() +
  theme(legend.position="bottom")
dist_shan_sens
ggsave(here::here("figures/dist_shan_sens_2024-08-26.png"), dist_shan_sens, 
       width = 6, height = 4, dpi = 300)

## 3. Calculate diversity - do not mask forests

# define a function

test_buffers <- function(area_with_nf, dist_m){
  nf_buffs <- data.frame()
  for (nf in 1:109) {
    #print(reg4_nf$FORESTORGC[nf])
    tmp_nf <- area_with_nf %>%
      filter(FORESTORGC == area_with_nf$FORESTORGC[nf])
    tmp_nf_buf <- st_buffer(tmp_nf, dist = dist_m)
    nf_buffs <- rbind(nf_buffs, tmp_nf_buf)
  }
  return(nf_buffs)
}

# test for forests in Region 4
nf_r4 <- fs_nf.crop %>%
  filter(REGION == "04")

reg4_buffs <- test_buffers(nf_r4, 50000)
reg4_buffs_crop <- st_crop(reg4_buffs, st_bbox(reg4))
reg4_buffs_int <- st_intersection(reg4_buffs, reg4)

# Calculate the shannon diversity for each forest
test_v <- reg4_buffs_int
test_z <- crop(sgfmc_pmrc_poli_result, test_v, mask = TRUE)

test_x <- exact_extract(test_z, test_v, coverage_area = TRUE)
names(test_x) <- test_v$FORESTORGC

test <- bind_rows(test_x, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  #group_by(value) %>%
  summarize(total_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion = total_area/sum(total_area))

test <- test %>% 
  replace_na(list(value = 0))

test_shan <- test %>%
  dplyr::select(FORESTORGC, proportion) %>%
  #select(proportion) %>%
  group_by(FORESTORGC) %>%
  summarise(shan_div = -sum(proportion * log(proportion)))

# join to sf!
test_shan_sf <- test_shan %>%
  left_join(nf_r4, by = "FORESTORGC")

test_shan_sf <- st_as_sf(test_shan_sf)

# generate map
test_shan_nf_reg4 <- ggplot() +
  geom_sf(data = reg4, fill = NA, color = "black") +
  geom_sf(data = test_shan_sf, aes(fill = shan_div)) +
  geom_sf(data = reg4_buffs_int, fill = NA, color = "black") + 
  labs(title = "Region 4 NFs: Shannon Diversity of Archetypes") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
test_shan_nf_reg4  

# That works and would be the new way to do Shannon Diversity if we choose this method moving forward. 

## 4. Playing with a sample raster and shape for Shannon 
#r <- raster(ncol=10, nrow=8)
r <- raster(ncol = 1, nrow = 8)
ncell(r)

#values(r) <- sample(1:8, 80, replace = T, prob = c(0.125, 0.125, 0.125, 0.125, 
#                                                   0.125, 0.125, 0.125, 0.125))
# even distribution = 2.04

#values(r) <- sample(1:8, 80, replace = T, prob = c(0.005, 0.15, 0.005, 0.005, 
#                                                   0.685, 0.1, 0.245, 0.125))
# uneven distribution = 1.07

#values(r) <- 5
# only 1 cluster = 0

# raster with only 8 cells
values(r) <- sample(1:8, 8, replace = F)
# evenly distributed = 1.95

plot(r)

e <- extent(r)
# coerce to a SpatialPolygons object
p_ext <- as(e, 'SpatialPolygons') 
p <- st_as_sf(p_ext)

test_v <- p %>% st_cast("MULTIPOLYGON")
test_z <- crop(r, test_v, mask = TRUE)

test_x <- exact_extract(test_z, test_v, coverage_area = TRUE)
names(test_x) <- "cluster"

test_areas <- bind_rows(test_x, .id = "cluster") %>%
  group_by(cluster, value) %>%
  summarize(total_arch_area = sum(coverage_area)) %>%
  group_by(cluster) %>%
  mutate(proportion_pct = round((total_arch_area/sum(total_arch_area))*100, 2)) %>%
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

test_shan_h <- test_areas %>%
  dplyr::select(cluster, proportion) %>%
  group_by(cluster) %>%
  summarise(shan_div = -sum(proportion * log(proportion)))
test_shan_h


# so for something with 8 clusters or objects the Shannon diversity can range from
# 0 (only 1 cluster) to approximately 2 for evenly distributed clusters


## moving forward do not mask out the NFs
## plot the buffers with a scale bar in one area, make sure it makes sense
## what happens at the border of CONUS?

# Test for region 4
# but for a specific forest first
reg4_nf <- fs_nf.crop %>%
  filter(REGIO)
  
  
  
test_nf <- fs_nf.crop %>%
  filter(FORESTORGC == "0413")
test_nf_buf <- st_buffer(test_nf, dist = 50000)
plot(test_nf_buf$geometry)
plot(test_nf$geometry, add = TRUE, color = "blue")

test_nf_buffers <- function(area_with_nf, dist_m){
  nf_buffs <- data.frame()
  for (nf in 1:109) {
    #print(reg4_nf$FORESTORGC[nf])
    tmp_nf <- area_with_nf %>%
      filter(FORESTORGC == area_with_nf$FORESTORGC[nf])
    tmp_nf_buf <- st_buffer(tmp_nf, dist = dist_m)
    nf_buffs <- rbind(nf_buffs, tmp_nf_int)
  }
  return(nf_buffs)
}
