library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(exactextractr)
library(tigris)

# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")

fcm_no_gs_attri <- rast("data/processed/rast_fcm_no_gs_2024-06-20.tif")
fcm_no_gs_result <- rast("data/processed/FCM_no_gs_2024-06-20.tif")

## Reproject the forest service shapes and crop
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)

fs_nf.crop <- st_crop(fs_nf.proj, ext(fcm_no_gs_attri))

fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)

fs_reg.crop <- st_crop(fs_reg.proj, ext(fcm_no_gs_attri))

# Union all of the national forests
nf_union <- st_union(fs_nf.crop)

# Create the buffer around the unioned shape
nf_union_buf <- st_buffer(nf_union, dist = 50000)
nf_union_dif <- st_difference(nf_union_buf, nf_union)

## Define a function to create 50km buffer zones around all National Forests

nf_buffers <- function(area_with_nf, area_with_nf_buf_dif){
  nf_buffs <- data.frame()
  for (nf in 1:109) {
    #print(reg4_nf$FORESTORGC[nf])
    tmp_nf <- area_with_nf %>%
      filter(FORESTORGC == area_with_nf$FORESTORGC[nf])
    tmp_nf_buf <- st_buffer(tmp_nf, dist = 50000)
    tmp_nf_int <- st_intersection(tmp_nf_buf, area_with_nf_buf_dif)
    nf_buffs <- rbind(nf_buffs, tmp_nf_int)
  }
  return(nf_buffs)
}

buffers <- nf_buffers(fs_nf.crop, nf_union_dif)  

# Calculate a diversity metric for the archetypes within the buffer zone
## crop archetype attributes and fcm results to the buffer shape
## calculate the proportion of the area covered by each archetype (area archetype / total area)
## using the proportions, calculate Shannon Diversity Index (-sum(species_proportions * log(species_proportions)))

v <- buffers %>% st_cast("MULTIPOLYGON")
z <- crop(fcm_no_gs_result, v, mask = TRUE)

x <- exact_extract(z, v, coverage_area = TRUE)
names(x) <- v$FORESTORGC

areas <- bind_rows(x, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  summarize(total_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion = total_area/sum(total_area))

areas <- areas %>% 
  replace_na(list(value = 0))

shan_h <- areas %>%
  select(FORESTORGC, proportion) %>%
  group_by(FORESTORGC) %>%
  summarise(shan_div = -sum(proportion * log(proportion)))

# join to sf!
shan_h_sf <- shan_h %>%
  left_join(fs_nf.crop, by = "FORESTORGC")

shan_h_sf <- st_as_sf(shan_h_sf)

# and plot
#fcm_reg4 <- crop(fcm_no_gs_result, reg4_nf)

#fcm.id <- fcm_reg4$Groups %>% as.data.frame(xy = TRUE)

shan_conus <- ggplot() +
  #geom_raster(aes(x = fcm.id$x, y = fcm.id$y, fill = as.factor(fcm.id$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = shan_h_sf, aes(fill = shan_div)) +
  #geom_sf(data = reg4_union_dif, fill = NA, color = "black") +
  #scale_fill_brewer(palette = "Set2") +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
shan_conus

## breadcrumb: add region and state boundaries

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

shan_conus_states <- ggplot() +
  #geom_raster(aes(x = fcm.id$x, y = fcm.id$y, fill = as.factor(fcm.id$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.1) +
  geom_sf(data = states, fill = NA, color = "black") +
  geom_sf(data = shan_h_sf, aes(fill = shan_div)) +
  #geom_sf(data = reg4_union_dif, fill = NA, color = "black") +
  #scale_fill_brewer(palette = "Set2") +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
shan_conus_states
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/shan_conus_states_", Sys.Date(), ".png"), plot = shan_conus_states, width = 12, height = 12, dpi = 300)  


shan_conus_reg <- ggplot() +
  #geom_raster(aes(x = fcm.id$x, y = fcm.id$y, fill = as.factor(fcm.id$Groups))) +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black") +
  geom_sf(data = shan_h_sf, aes(fill = shan_div)) +
  #geom_sf(data = reg4_union_dif, fill = NA, color = "black") +
  #scale_fill_brewer(palette = "Set2") +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
shan_conus_reg


#----What does this mean for NEPA assessment times?----

# load pals data

pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")

# selected forests from PMRC draft
pals_df_sel <- pals_df %>%
  filter(FOREST_ID %in% c("0511", "0909", "0402", "0801")) %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  group_by(FOREST_ID, `DECISION TYPE`) %>%
  summarise(ave_days = mean(`ELAPSED DAYS`, na.rm = TRUE),
            count = n())

pals_count <- pals_df %>% 
  select(FOREST_ID, `DECISION TYPE`) %>%
  filter(FOREST_ID %in% c("0511", "0909", "0402", "0801")) %>%
  group_by(FOREST_ID, `DECISION TYPE`) %>% 
  summarise(count = n())

pals_df_edays <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  group_by(FOREST_ID, `DECISION TYPE`) %>%
  summarise(ave_days = mean(`ELAPSED DAYS`, na.rm = TRUE),
            count = n())
shan_df <- shan_h %>%
  select(FORESTORGC, shan_div)

pals_edays_shanh <- left_join(pals_df_edays, shan_df, by = c("FOREST_ID" = "FORESTORGC"))

edays_shanh_plot <- ggplot(pals_edays_shanh, aes(shan_div, ave_days)) +
  geom_point(aes(color = factor(`DECISION TYPE`)))
edays_shanh_plot

hist(pals_edays_shanh$ave_days)
hist(pals_edays_shanh$shan_div)

test_hist <- ggplot(pals_edays_shanh) +
  geom_histogram(aes(mapping = shan_div, color = factor(`DECISION TYPE`)))
test_hist

test_shandiv <- pals_edays_shanh %>%
  ggplot( aes(x=shan_div, fill=factor(`DECISION TYPE`))) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#404")) +
  facet_wrap(~factor(`DECISION TYPE`)) +
  theme_bw() +
  labs(fill="")
test_shandiv

test_days <- pals_edays_shanh %>%
  ggplot( aes(x=ave_days, fill=factor(`DECISION TYPE`))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#404")) +
  theme_bw() +
  labs(fill="")
test_days
