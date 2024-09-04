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

fcm_pmrc_poli_attri <- rast("data/processed/rast_fcm_pmrc2024_ploi_2024-07-22.tif")
fcm_pmrc_poli_result <- rast("data/processed/FCM_pmrc_poli_2024-07-22.tif")
fcm_pmrc_poli <- readRDS("data/processed/FCM_pmrc_poli_2024-07-22.rds")

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

nf_buffs <- nf_buffers(fs_nf.crop, nf_union_dif)

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
  mutate(proportion = (total_arch_area/sum(total_arch_area)))

areas <- areas %>% 
  replace_na(list(value = 0))

shan_h <- areas %>%
  dplyr::select(FORESTORGC, proportion_pct) %>%
  group_by(FORESTORGC) %>%
  summarise(shan_div = -sum(proportion_pct * log(proportion_pct)))

# I did something here - check an earlier version
shan_h <- function(areas) {
  shan_h <- areas %>%
    dplyr::select(FORESTORGC, proportion) %>%
    group_by(FORESTORGC) %>%
    summarise(shan_div = -sum(proportion * log(proportion)))
  return (shan_h)
}

# join to sf!
shan_h_sf <- shan_h %>%
  left_join(fs_nf.crop, by = "FORESTORGC")
shan_h_sf <- st_as_sf(shan_h_sf)

# I did something here - check an earlier version
shan_h_sf <- function(shan_h){
  shan_h_sf <- shan_h %>%
  left_join(fs_nf.crop, by = "FORESTORGC")
  shan_h_sf <- st_as_sf(shan_h_sf)
  return(shan_h_sf)
}

shan_conus <- ggplot() +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = shan_h_sf, aes(fill = shan_div)) +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
shan_conus

## Add region and state boundaries

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
  geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 0.1) +
  geom_sf(data = states, fill = NA, color = "black") +
  geom_sf(data = shan_h_sf, aes(fill = shan_div)) +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
shan_conus_states
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/shan_conus_states_pmrc_poli_", Sys.Date(), ".png"), plot = shan_conus_states, width = 12, height = 12, dpi = 300)  


shan_conus_reg <- ggplot() +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black") +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black") +
  geom_sf(data = shan_h_sf, aes(fill = shan_div)) +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
shan_conus_reg
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/shan_conus_reg_pmrc_poli_", Sys.Date(), ".png"), plot = shan_conus_reg, width = 12, height = 12, dpi = 300)  

shan_conus_reg_state <- ggplot() +
  geom_sf(data = fs_nf.crop, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = states, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = fs_reg.crop, fill = NA, color = "black", linewidth = 1.5) +
  geom_sf(data = shan_h_sf, aes(fill = shan_div)) +
  labs(title = "Shannon Entropy (H) of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
shan_conus_reg_state
ggsave(paste0("~/Analysis/Archetype_Analysis/figures/shan_conus_reg_state_pmrc_poli_", Sys.Date(), ".png"), plot = shan_conus_reg, width = 12, height = 12, dpi = 300)  

#----Archetype Validation with Common Project Types----

# load pals data

pals_df <- read_delim("~/Analysis/NEPA_Efficiency/data/original/pals_ongoing_projects_11-2022.csv", delim = ";")

# Filter for date and select Forest Number and Purposes
pals_df_2009 <- pals_df %>%
  filter(as.Date(`INITIATION DATE`, format = "%m/%d/%Y") >= "2009-01-01") %>%
  dplyr::select(FOREST_ID, `FC Facility management – purpose`, 
       `FR Research – purpose`, `HF Fuels management – purpose`, `HR Heritage resource management – purpose`,
       `LM Land ownership management – purpose`, `LW Land acquisition – purpose`,
       `MG Minerals and geology – purpose`, `PN Land management planning – purpose`,
       `RD Road management – purpose`, `RG Grazing management – purpose`, `RO Regulations, directives, orders – purpose`,
       `RU Special area management – purpose`, `RW Recreation management – purpose`,
       `SU Special use management – purpose`, `TM Forest products – purpose`, 
       `VM Vegetation management (non-forest products) – purpose`,
       `WF Wildlife, fish, rare plants – purpose`, `WM Water management – purpose`) %>%
  group_by(FOREST_ID) %>%
  summarise(count_FC = sum(`FC Facility management – purpose`), 
            count_FR = sum(`FR Research – purpose`),
            count_HF = sum(`HF Fuels management – purpose`),
            count_HR = sum(`HR Heritage resource management – purpose`),
            count_LM = sum(`LM Land ownership management – purpose`), 
            count_LW = sum(`LW Land acquisition – purpose`), 
            count_MG = sum(`MG Minerals and geology – purpose`), 
            count_PN = sum(`PN Land management planning – purpose`),
            count_RD = sum(`RD Road management – purpose`), 
            count_RG = sum(`RG Grazing management – purpose`),
            count_RO = sum(`RO Regulations, directives, orders – purpose`),
            count_RU = sum(`RU Special area management – purpose`), 
            count_RW = sum(`RW Recreation management – purpose`), 
            count_SU = sum(`SU Special use management – purpose`),
            count_TM = sum(`TM Forest products – purpose`),
            count_VM = sum(`VM Vegetation management (non-forest products) – purpose`), 
            count_WF = sum(`WF Wildlife, fish, rare plants – purpose`), 
            count_WM = sum(`WM Water management – purpose`))

areas_wide <- areas %>%
  dplyr::select(-total_arch_area) %>%
  pivot_wider(names_from = value, values_from = proportion_pct)

pals_purpose_arch_pct_area <- left_join(pals_df_2009, areas_wide, by = join_by(FOREST_ID == FORESTORGC)) %>%
  mutate_if(is.numeric, coalesce, 0)

# could filter by forests with >70% in any specific archetype

# try with archetypes 1 and 4 first

arche1 <-  pals_purpose_arch_pct_area %>%
  filter(`1` >= 70.0) %>%
  pivot_longer(cols = starts_with("count"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "one", 
         pct_purpose = values/sum(values) * 100)

arche4 <- pals_purpose_arch_pct_area %>%
  filter(`4` >= 70.0) %>%
  pivot_longer(cols = starts_with("count"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "four", 
         pct_purpose = values/sum(values) * 100)

arche3 <- pals_purpose_arch_pct_area %>%
  filter(`3` >= 70.0) %>%
  pivot_longer(cols = starts_with("count"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "three", 
         pct_purpose = values/sum(values) * 100)

arche2 <- pals_purpose_arch_pct_area %>%
  filter(`2` >= 70.0) %>%
  pivot_longer(cols = starts_with("count"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "two", 
         pct_purpose = values/sum(values) * 100)

arche8 <- pals_purpose_arch_pct_area %>%
  filter(`8` >= 70.0) %>%
  pivot_longer(cols = starts_with("count"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "eight", 
         pct_purpose = values/sum(values) * 100)

arche6 <- pals_purpose_arch_pct_area %>%
  filter(`6` >= 70.0) %>%
  pivot_longer(cols = starts_with("count"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "six", 
         pct_purpose = values/sum(values) * 100)

arche_no_dom <- pals_purpose_arch_pct_area %>%
  filter(`1` < 70 & `2` < 70 & `3` < 70 & `4` < 70 & `5` < 70 & `6` < 70 & `7` < 70 & `8` < 70) %>%
  filter(FOREST_ID != "0000" | FOREST_ID != "1004" | FOREST_ID != "1005" | FOREST_ID != "2400" | FOREST_ID != "2403" | FOREST_ID != "2408") %>%
  pivot_longer(cols = starts_with("count"), 
               names_to = "purpose") %>%
  group_by(purpose) %>%
  summarise(values = sum(value)) %>%
  mutate(archetype = "no dominant archetype (>70%)",
         pct_purpose = values/sum(values) * 100)

#arche1.4 <- left_join(arche1, arche4)
arche_purposes <- rbind(arche2, arche8, arche_no_dom)

arche_purposes$archetype <- factor(arche_purposes$archetype,
                                   levels = c("two", "eight", "no dominant archetype (>70%)"))

ggplot(arche_no_dom, aes(x=purpose, y = pct_purpose)) +
  geom_bar(stat="identity", width = 0.7, fill = "steelblue") +
  theme_minimal()

arch28_purpose <- ggplot(arche_purposes, aes(x=purpose, y = pct_purpose, fill = archetype)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
  scale_fill_discrete(limits=c("two", "eight", "no dominant archetype (>70%)")) + 
  theme_minimal() + 
  theme_bw()
arch28_purpose
ggsave(filename = here::here("figures/archetype_validation_test_2024-08-26.png"), 
       plot = arch28_purpose, 
       width = 14, 
       height = 4, dpi = 300)


#----What does this mean for NEPA assessment times?----
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


summarizeClusters(df_all,
                  belongmatrix = SGFCM_result$Belongings,
                  weighted = TRUE, dec = 3)
