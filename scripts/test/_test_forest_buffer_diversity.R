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
  filter(FORESTORGC == "0402")
test_nf_buf <- st_buffer(test_nf, dist = 50000)

# Get the intersection of the NF buffer and the differenced union buffer
test_nf_buf_int <- st_intersection(test_nf_buf, reg4_union_dif)
plot(test_nf_buf_int$geometry)
plot(test_nf, add = TRUE, col = "yellow")

# For all NF in region 4: use a for loop, function, or apply???

# Create a function
nf_buffers <- function(area_with_nf, area_with_nf_buf_dif){
  nf_buffs <- data.frame()
  for (nf in 1:length(area_with_nf)) {
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

# Try apply
## make a list of the forest numbers

forest_list <- list()
  
for (nf in 1:length(reg4_nf)) {
  tmp <- reg4_nf$FORESTORGC[nf]
  forest_list <- append(forest_list, tmp)
}

# shpdatas becomes a list of sp-class objects:
shpdatas = lapply(shapefiles, read_in_and_check)
output = do.call(bind, shpdatas)






#--------------------test code-------------

for (nf in 1:length(reg4_nf)) {
  print(reg4_nf$FORESTORGC[nf])
  tmp_nf <- reg4_nf %>%
    filter(FORESTORGC == reg4_nf$FORESTORGC[nf])
  tmp_nf_buf <- st_buffer(tmp_nf, dist = 50000)
  tmp_int <- st_intersection(tmp_nf_buf, reg4_union_dif)
}

for (nf in 1:length(reg4_nf)) {
  print(reg4_nf$FORESTORGC[nf])
}

