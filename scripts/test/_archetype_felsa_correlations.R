library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(future)
library(classInt)
library(elsa)

#----Load the attribute rasters
rst_fcm_pmrc_poli_sc <- rast(here::here("data/processed/rst_fcm_pmrc_poli_sc_2024-08-12.tif"))
rst_fcm_pmrc_poli <- rast(here::here("data/processed/rast_fcm_pmrc2024_ploi_2024-08-12.tif"))


#----Load the felsa rasters----
fuzzy_elsa_all <- rast(here::here("data/processed/SGFCM_felsa_2024-09-04.tif"))
fuzzy_elsa_eco <- rast(here::here("data/processed/SGFCM_eco_felsa_2024-09-03.tif"))
fuzzy_elsa_soc <- rast(here::here("data/processed/SGFCM_soc_felsa_2024-09-04.tif"))

fuzzy_elsa_eco_k8 <- rast(here::here("data/processed/SGFCM_eco_felsa_k8_2024-09-12.tif"))
fuzzy_elsa_soc_k8 <- rast(here::here("data/processed/SGFCM_soc_felsa_k8_2024-09-12.tif"))

#----Load the National Forest and USFS Region boundaries----
fs_nf <- st_read("data/original/S_USA.AdministrativeForest.shp")
fs_reg <- st_read("data/original/S_USA.AdministrativeRegion.shp")
projection <- "epsg: 5070"

##---Set the projection and crop to the fuzzy elsa all raster
fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(fuzzy_elsa_all))
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(fuzzy_elsa_all))

#----Create a 50km buffer around each National Forest
nf_create_buffers <- function(area_with_nf, dist_m){
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

nf_buffers <- nf_create_buffers(fs_nf.crop, 50000)

#----Calculate the average fuzzy ELSA score for each NF----
# Calculate the average f-ELSA score for each forest
nf_calc_elsa <- function(area_to_calc, elsa_rast){
  l <- nrow(area_to_calc)
  felsa_means <- data.frame()
  for (nf in 1:l) {
    tmp_nf <- area_to_calc %>%
      filter(FORESTORGC == area_to_calc$FORESTORGC[nf])
    e <- terra::extract(elsa_rast, tmp_nf, fun=mean, na.rm = TRUE)
    e$ID <- tmp_nf$FORESTORGC
    felsa_means <- rbind(felsa_means, e)
  }
  return(felsa_means)
} 

felsa_mean_all <- nf_calc_elsa(nf_buffers, fuzzy_elsa_all) 
felsa_mean_all <- rename(felsa_mean_all, c(FORESTORGC = ID,
                                           felsa_all = pct_forpay_fill))

felsa_mean_soc <- nf_calc_elsa(nf_buffers, fuzzy_elsa_soc) 
felsa_mean_soc <- rename(felsa_mean_soc, c(FORESTORGC = ID,
                                           felsa_soc = pct_forpay_fill))

felsa_mean_soc_k8 <- nf_calc_elsa(nf_buffers, fuzzy_elsa_soc_k8) 
felsa_mean_soc_k8 <- rename(felsa_mean_soc, c(FORESTORGC = ID,
                                           felsa_soc_k8 = pct_forpay_fill))

felsa_mean_eco <- nf_calc_elsa(nf_buffers, fuzzy_elsa_eco_k8)
felsa_mean_eco <- rename(felsa_mean_eco, c(FORESTORGC = ID,
                                           felsa_eco_k8 = mean_forprod_fia))

felsa_means <- left_join(felsa_mean_all, felsa_mean_eco)
felsa_means <- left_join(felsa_means, felsa_mean_soc)

felsa_means <- felsa_means %>%
  mutate(sum_eco_soc = felsa_eco + felsa_soc,
         mean_eco_soc = (felsa_eco + felsa_soc)/2)


# plot the distribution of FELSAs
hist(felsa_means$felsa_all)
hist(felsa_means$felsa_eco)
hist(felsa_means$felsa_soc)

# join the calculated f-ELSA averages to the NF shapes
felsa_sf <- left_join(felsa_mean_all, fs_nf.crop, by = "FORESTORGC") %>%
  dplyr::select(FORESTORGC, felsa_all, geometry)
felsa_sf <- left_join(felsa_sf, felsa_mean_eco, by = "FORESTORGC")
felsa_sf <- left_join(felsa_sf, felsa_mean_soc, by = "FORESTORGC")

felsa_sf <- st_as_sf(felsa_sf)

# Add the Regional Number to data frame
felsa_sf <- felsa_sf %>% 
  mutate(REGION = str_extract(FORESTORGC, "\\b[0-9]{2}"))

#----Calculate the average fuzzy ELSA score for each Region----
# get the average elsa score for each region
reg_calc_elsa <- function(area_to_calc, elsa_rast){
  l <- nrow(area_to_calc)
  felsa_means <- data.frame()
  for (nf in 1:l) {
    tmp_nf <- area_to_calc %>%
      filter(REGION == area_to_calc$REGION[nf])
    e <- terra::extract(elsa_rast, tmp_nf, fun=mean, na.rm = TRUE)
    e$ID <- tmp_nf$REGION
    felsa_means <- rbind(felsa_means, e)
  }
  return(felsa_means)
} 

# get the average elsa score for each region
reg_elsa_raw <- function(area_to_calc, elsa_rast){
  l <- nrow(area_to_calc)
  felsa_regs <- data.frame()
  for (reg in 1:l) {
    tmp_reg <- area_to_calc %>%
      filter(REGION == area_to_calc$REGION[reg])
    e <- terra::extract(elsa_rast, tmp_reg, method="simple")
    e$ID <- tmp_reg$REGION
    felsa_regs <- rbind(felsa_regs, e)
  }
  return(felsa_regs)
} 

felsa_mean_all_reg <- reg_calc_elsa(fs_reg.proj, fuzzy_elsa_all)
felsa_mean_all_reg <- rename(felsa_mean_all_reg, c(REGION = ID,
                                                   felsa_all = pct_forpay_fill))

felsa_all_reg <- reg_elsa_raw(fs_reg.proj, fuzzy_elsa_all)
felsa_all_reg <- rename(felsa_all_reg, c(REGION = ID,
                                         felsa_all = pct_forpay_fill))

felsa_mean_soc_reg <- reg_calc_elsa(fs_reg.proj, fuzzy_elsa_soc) 
felsa_mean_soc_reg <- rename(felsa_mean_soc_reg, c(REGION = ID,
                                                   felsa_soc = pct_forpay_fill))

felsa_soc_reg <- reg_elsa_raw(fs_reg.proj, fuzzy_elsa_soc)
felsa_soc_reg <- rename(felsa_soc_reg, c(REGION = ID,
                                         felsa_soc = pct_forpay_fill))

felsa_mean_eco_reg <- reg_calc_elsa(fs_reg.proj, fuzzy_elsa_eco)
felsa_mean_eco_reg <- rename(felsa_mean_eco_reg, c(REGION = ID,
                                                   felsa_eco = mean_forprod_fia))

felsa_eco_reg <- reg_elsa_raw(fs_reg.proj, fuzzy_elsa_eco)
felsa_eco_reg <- rename(felsa_eco_reg, c(REGION = ID,
                                         felsa_eco = mean_forprod_fia))

felsa_means_reg <- left_join(felsa_mean_all_reg, felsa_mean_eco_reg)
felsa_means_reg <- left_join(felsa_means_reg, felsa_mean_soc_reg)

felsa_means_reg <- felsa_means_reg %>%
  mutate(sum_eco_soc = felsa_eco + felsa_soc,
         mean_eco_soc = (felsa_eco + felsa_soc)/2)


# Boxplot of F-ELSA values for each Region

felsa_all_reg %>%
  ggplot( aes(x=REGION, y=felsa_all, fill=REGION)) +
  geom_boxplot() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot") +
  xlab("Regions")

felsa_soc_reg %>%
  ggplot( aes(x=REGION, y=felsa_soc, fill=REGION)) +
  geom_boxplot() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot") +
  xlab("Regions")

felsa_eco_reg %>%
  ggplot( aes(x=REGION, y=felsa_eco, fill=REGION)) +
  geom_boxplot() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot") +
  xlab("Regions")

# scatter plot
test_scatter <- felsa_sf %>%
  ggplot(aes(x=felsa_eco, y=felsa_soc, group=FORESTORGC, color=REGION)) +
  geom_point() + 
  #scale_x_continuous(breaks = seq(10, 110, by = 10)) +
  theme_bw() +
  theme(legend.position="right")
test_scatter
ggsave(here::here("figures/test_felsa_scatter_2024-09-04.png"), test_scatter, 
       width = 6, height = 4, dpi = 300)


# join the calculated fuzzy elsa averages to the USFS Region shapes
felsa_reg_sf <- left_join(felsa_mean_all_reg, fs_reg.crop, by = "REGION") %>%
  dplyr::select(REGION, felsa_all, geometry)
felsa_reg_sf <- left_join(felsa_reg_sf, felsa_mean_eco_reg, by = "REGION")
felsa_reg_sf <- left_join(felsa_reg_sf, felsa_mean_soc_reg, by = "REGION")

felsa_reg_sf <- st_as_sf(felsa_reg_sf)

#----Testing out correlation----
# create a raster stack of the fuzzy elsas
felsa_all_eco <- c(fuzzy_elsa_all, fuzzy_elsa_eco)
felsa_all_soc <- c(fuzzy_elsa_all, fuzzy_elsa_soc)

names(felsa_all_eco) <- c("f_elsa_all", "f_elsa_eco")
names(felsa_all_soc) <- c("f_elsa_all", "f_elsa_soc")

felsa_all_eco_cor_5 <- focalPairs(felsa_all_eco, w = 5, cor) 
felsa_all_eco_cor_5
plot(felsa_all_eco_cor_5$lyr1)

felsa_all_soc_cor_5 <- focalPairs(felsa_all_soc, w = 5, cor) 
felsa_all_soc_cor_5
plot(felsa_all_soc_cor_5$lyr1)

### with the k=8 F-ELSAs
felsa_all_eco_k8 <- c(fuzzy_elsa_all, fuzzy_elsa_eco_k8)
felsa_all_soc_k8 <- c(fuzzy_elsa_all, fuzzy_elsa_soc_k8)

names(felsa_all_eco_k8) <- c("f_elsa_all", "f_elsa_eco") 
names(felsa_all_soc_k8) <- c("f_elsa_all", "f_elsa_soc")

felsa_all_eco_k8_cor_5 <- focalPairs(felsa_all_eco_k8, w = 5, cor) 
felsa_all_eco_k8_cor_5
plot(felsa_all_eco_k8_cor_5$lyr1)

felsa_all_soc_k8_cor_5 <- focalPairs(felsa_all_soc_k8, w = 5, cor) 
felsa_all_soc_k8_cor_5
plot(felsa_all_soc_k8_cor_5$lyr1)


# using terra::xapp

test_cor <- xapp(fuzzy_elsa_all, fuzzy_elsa_eco, fun = cor)
plot(test_cor)

# from https://stackoverflow.com/questions/77803800/calculating-pixelwise-correlation-between-two-multi-layer-rasters-in-r-with-the
my.cor <- function(x, y) {
  if (any(is.na(x)) || any(is.na(y))) return(c(NA, NA))
  unlist(cor.test(x, y)[c("estimate", "p.value")])
}

z <- xapp(fuzzy_elsa_all, fuzzy_elsa_eco, my.cor)

z


naproof.cor.test <- function(x, y) {
  if (any(is.na(x)) || any(is.na(y))) return(NA)
  cor.test(x, y)$p.value
}

xtest <- xapp(fuzzy_elsa_all, fuzzy_elsa_eco, naproof.cor.test)


# did not work
# trying from here https://stackoverflow.com/questions/23939920/raster-correlation-and-p-values-from-cor-test
funcal <- function(xy) {    
  xy <- na.omit(matrix(xy, ncol=2))
  if (ncol(xy) < 2 | nrow(xy)<1) {
    NA
  } else {
    cor.test(xy[, 1], xy[, 2], method="pearson")$p.value
  }
}



# replace NA?
fuzzy_elsa_all[is.na(fuzzy_elsa_all[])] <- 2
fuzzy_elsa_eco[is.na(fuzzy_elsa_eco[])] <- 2
fuzzy_elsa_soc[is.na(fuzzy_elsa_soc[])] <- 2

test_cor <- xapp(fuzzy_elsa_all, fuzzy_elsa_eco, fun = cor)
plot(test_cor)

plot(fuzzy_elsa_all)
plot(fuzzy_elsa_eco)
plot(fuzzy_elsa_soc)

test_corlocal <- corLocal(fuzzy_elsa_all, fuzzy_elsa_eco, ngb = 1, method = "pearson")

# testing xapp
r <- rast(ncols=10, nrows=10, nlyr=5)

set.seed(1)
r <- init(r, runif)
s <- init(r, runif)
plot(r)
plot(s)

x <- xapp(r, s, fun=cor)
plot(x)

Cor<-corLocal(fuzzy_elsa_all,fuzzy_elsa_eco, test = T)

