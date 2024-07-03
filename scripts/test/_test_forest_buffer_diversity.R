library(tidyverse)
library(terra)
library(sf)
library(ggplot2)

# Load the data
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")

fcm_no_gs_attri <- rast("data/processed/rast_fcm_no_gs_2024-06-20.tif")
fcm_no_gs_result <- rast("data/processed/FCM_no_gs_2024-06-20.tif")

## Reproject the forest service shapes and crop
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)

fs_nf.crop <- st_crop(fs_nf.proj, ext(fcm_no_gs_attri))

## Test creating a buffer around one NF

test_nf <- fs_nf.crop %>%
  filter(FORESTORGC == "0103")

test_buf <- st_buffer(test_nf, dist = 10000)

plot(test_buf$geometry)
plot(test_nf$geometry, add=TRUE, col="blue")

test_buf_dif <- st_difference(test_buf, test_nf)
plot(test_buf_dif$geometry, col = "green")

## Test for Region 4

reg4_nf <- fs_nf.crop %>%
  filter(REGION == "04")

# Make a union first
reg4_union <- st_union(reg4_nf)
plot(reg4_union)

# Create the buffer around the unioned shape
reg4_union_buf <- st_buffer(reg4_union, dist = 50000)
reg4_union_dif <- st_difference(reg4_union_buf, reg4_union)

# Create a buffer around the test NF
test_nf <- fs_nf.crop %>%
  filter(FORESTORGC == "0415")
test_nf_buf <- st_buffer(test_nf, dist = 50000)
plot(test_nf_buf$geometry)

# Get the intersection of the NF buffer and the differenced union buffer
test_nf_buf_int <- st_intersection(test_nf_buf, reg4_union_dif)
plot(test_nf_buf_int$geometry)
plot(test_nf, add = TRUE, col = "yellow")
plot(reg4_union_dif, add = TRUE)

# For all NF in region 4: use a for loop, function, or apply???

# Create a function
nf_buffers <- function(area_with_nf, area_with_nf_buf_dif){
  nf_buffs <- data.frame()
  for (nf in 1:12) {
    #print(reg4_nf$FORESTORGC[nf])
    tmp_nf <- area_with_nf %>%
      filter(FORESTORGC == area_with_nf$FORESTORGC[nf])
    tmp_nf_buf <- st_buffer(tmp_nf, dist = 50000)
    tmp_nf_int <- st_intersection(tmp_nf_buf, area_with_nf_buf_dif)
    nf_buffs <- rbind(nf_buffs, tmp_nf_int)
  }
  return(nf_buffs)
}

test_buffers <- nf_buffers(reg4_nf, reg4_union_dif)  

for1 <- test_buffers %>%
  filter(FORESTORGC == "0410")
for2 <- test_buffers %>%
  filter(FORESTORGC == "0417")
for3 <- test_buffers %>%
  filter(FORESTORGC == "0402")

plot(reg4_union_dif)
plot(for1$geometry, col = "blue", add = TRUE)
plot(for2$geometry, col = "red", add = TRUE)
plot(for3$geometry, col = "green", add = TRUE)

# Calculate a diversity metric for the archetypes within the buffer zone
## crop archetype attributes and fcm results to the buffer shape
## calculate the proportion of the area covered by each archetype (area archetype / total area)
## using the proportions, calculate Shannon Diversity Index (-sum(species_proportions * log(species_proportions)))

library(exactextractr)

test_v <- test_buffers
test_z <- crop(fcm_no_gs_result, test_v, mask = TRUE)

test_x <- exact_extract(test_z, test_v, coverage_area = TRUE)
names(test_x) <- test_v$FORESTORGC

test <- bind_rows(test_x, .id = "FORESTORGC") %>%
  group_by(FORESTORGC, value) %>%
  summarize(total_area = sum(coverage_area)) %>%
  group_by(FORESTORGC) %>%
  mutate(proportion = total_area/sum(total_area))

test <- test %>% 
  replace_na(list(value = 0))

test_shan <- test %>%
  select(FORESTORGC, proportion) %>%
  group_by(FORESTORGC) %>%
  summarise(shan_div = -sum(proportion * log(proportion)))

# join to sf!
test_shan_sf <- test_shan %>%
  left_join(reg4_nf, by = "FORESTORGC")

test_shan_sf <- st_as_sf(test_shan_sf)

# and plot
fcm_reg4 <- crop(fcm_no_gs_result, reg4_nf)

fcm.id <- fcm_reg4$Groups %>% as.data.frame(xy = TRUE)

test_shan_reg4 <- ggplot() +
  #geom_raster(aes(x = fcm.id$x, y = fcm.id$y, fill = as.factor(fcm.id$Groups))) +
  geom_sf(data = reg4_nf, fill = NA, color = "black") +
  geom_sf(data = test_shan_sf, aes(fill = shan_div)) +
  geom_sf(data = reg4_union_dif, fill = NA, color = "black") +
  #scale_fill_brewer(palette = "Set2") +
  labs(title = "Region 4: Shannon Diversity of Archetypes",
       subtitle = "Calculated from 50km buffer around National Forests") +
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),"mm"))
test_shan_reg4  

# from https://gis.stackexchange.com/questions/425821/calculating-proportion-of-a-polygon-covered-by-each-raster-category-using-r

f <- system.file("ex/lux.shp", package="terra")
v <- st_read(f)
v <- v[1:2,]
z <- rast(vect(v), resolution=.09, names="test")
values(z) <- 1:ncell(z)
levels(z) <- 0:ncell(z)

plot(z)
plot(v, add = T)

#extract the area of each cell that is contained within each polygon
x <- exact_extract(z, v, coverage_area = TRUE)

#add polygon names that the results will be grouped by
names(x) <- v$NAME_2

#bind the list output into a df and calculate the proportion cover for each category
test <- bind_rows(x, .id = "region") %>%
  group_by(region, value) %>%
  summarize(total_area = sum(coverage_area)) %>%
  group_by(region) %>%
  mutate(proportion = total_area/sum(total_area))


# Sample data: species counts
species_counts <- c(10, 20, 30, 40)

# Calculate proportions
species_proportions <- species_counts / sum(species_counts)

# Calculate Shannon Diversity Index
shannon_diversity <- -sum(species_proportions * log(species_proportions))
print(shannon_diversity)



