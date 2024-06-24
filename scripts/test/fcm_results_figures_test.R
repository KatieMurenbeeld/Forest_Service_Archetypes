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

### Read in the rds of the fcm objects
fcm_all_model <- readRDS("data/processed/FCM_all_2024-06-20.rds")
fcm_no_gs_model <- readRDS("data/processed/FCM_no_gs_2024-06-20.rds")
fcm_rursu01_model <- readRDS("data/processed/FCM_ru_rs_u_01_2024-06-20.rds")
fcm_eco_model <- readRDS("data/processed/FCM_eco_2024-06-21.rds")

## Reproject the forest service shapes to NAD83
projection <- "epsg: 5070"

fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(fcm_all_attri))

## Check and rename the attributes in each raster stack
names(fcm_no_gs_attri)
fcm_no_gs_attri_labs <- c("% Forest Pay", "Community Capital",
                          "Net Migration 2020-2023", "% Less High School",
                          "% Families Housing Burden", "% Household Energy Cost",
                          "PM2.5 particle/m3", "Dist. to wilderness area, m", 
                          "Dist. to critical habitat, m", "Wildfire Hazard Potential",
                          "% Change in Mill Capacity 2019-2024", "Precip. Seasonality",
                          "Temp. Seasonality", "Topo. Roughness", 
                          "Travel time to >20000 city, min", "% Area Tree Cover", 
                          "# Federal Agencies", "% Area with Forest Gain 2000-2012",
                          "Ave Stand Age", "Ave. Forest Productivity cuft/ac2/yr")

names(fcm_eco_attri)
fcm_eco_attri_labs <- c("PM2.5 particle/m3", "Wildfire Hazard Potential",
                        "Precip. Seasonality", "Temp. Seasonality", "Topo. Roughness")

names(fcm_rursu01_attri)
fcm_rursu01_attri_labs <- c("% Forest Pay", "Community Capital",
                            "% Families Housing Burden", "% Household Energy Cost",
                            "Travel time to >20000 city, min", "% Area Tree Cover", 
                            "% Area with Forest Gain 2000-2012", "Ave Stand Age", 
                            "Ave. Forest Productivity cuft/ac2/yr")

## Calculate the variance of the attributes in each archetype/group
### Create data frames with the attribute values and the group assignments from the model
### testing things about variance for myself ###
df_test <- as.data.frame(fcm_no_gs_attri, na.rm = TRUE)
head(df_test)
df_test <- cbind(df_test, fcm_no_gs_model$Groups)

df_test_group_means <- df_test %>%
  group_by(`fcm_no_gs_model$Groups`) %>%
  summarise(across(, ~ mean(.x)))
head(df_test_group_means)

df_test_group_sds <- df_test %>%
  group_by(`fcm_no_gs_model$Groups`) %>%
  summarise(across(, ~ sd(.x)))
head(df_test_group_sds)

df_test_total_means <- df_test %>%
  select(-`fcm_no_gs_model$Groups`) %>%
  summarise(across(, ~ mean(.x)))

df_test_total_sds <- df_test %>%
  select(-`fcm_no_gs_model$Groups`) %>%
  summarise(across(, ~ sd(.x)))

## Find the attributes that most describe the archetype. Mean value is >1sd of global
# for (attribute in table 1) {
#    are values in column of table 1 greater than value in column in table 2?
#    TRUE or FALSE?
#}
# If true, then what? 

df_test_group_means$pct_pay_focal_mean - df_test_total_means$pct_pay_focal_mean[1]
# Make a new data frame with the difference in means
df_mean_diff <- df_test_group_means
df_mean_diff[2] <- df_test_group_means[2] - df_test_total_means$pct_pay_focal_mean[1]

for (i in 1:20) {
  df_mean_diff[i+1] <- df_test_group_means[i+1] - df_test_total_means[[i]]
}

df_mean_diff_1sd <- df_mean_diff
for (i in 1:20) {
  df_mean_diff_1sd[i+1] <- abs(df_mean_diff[i+1]) > df_test_total_sds[[i]]
}
df_mean_diff_0.5sd <- df_mean_diff
for (i in 1:20) {
  df_mean_diff_0.5sd[i+1] <- abs(df_mean_diff[i+1]) > (df_test_total_sds[[i]]/2)
}

df_test_zscore <- data.frame()
?data.frame()
df_test_zscore$test_zscore <- mean((df_test_group_means[,2] - df_test_total_means[,1]) / df_test_total_sds[,1])
  
df_test_zscore <- df_test_group_means %>%
  select(-`fcm_no_gs_model$Groups`)

df_groups <- as.data.frame(df_test_group_means$`fcm_no_gs_model$Groups`)
for (i in 1:20){
  print(df_test_zscore[,i] - df_test_total_means[,i]) / df_test_total_sds[,i]
  df_groups <- cbind(tmp)
}

no_gs_var <- df_no_gs_var %>%
  select(-rn) %>%
  group_by(`fcm_no_gs_model$Groups`) %>%
  summarise(across(, ~ var(.x, na.rm = TRUE)))

df_no_gs_var <- as.data.frame(scale(fcm_no_gs_attri), na.rm = TRUE)
df_no_gs_var <- cbind(df_no_gs_var, fcm_no_gs_model$Groups)
df_no_gs_var$rn <- row.names(df_no_gs_var)

df_no_gs_range <- as.data.frame(fcm_no_gs_attri, na.rm = TRUE)
df_no_gs_range <- apply(df_no_gs_range, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
df_no_gs_scales <- as.data.frame(df_no_gs_range)
df_no_gs_scaled <- cbind(df_no_gs_scales, fcm_no_gs_model$Groups)

df_rurs1_var <- as.data.frame(scale(fcm_rursu01_attri), na.rm = TRUE)
df_rurs1_var <- cbind(df_rurs1_var, fcm_rursu01_model$Groups)
df_rurs1_var$rn <- row.names(df_rurs1_var)
df_rurs1_all_test <- left_join(df_no_gs_var, df_rurs1_var, by = "rn") 
df_rurs1_all <- df_rurs1_all_test %>%
  select(-pct_pay_focal_mean.y, -COMMCAP_focal_mean.y, -HSEF_focal_mean.y, 
         -HBF_PFS_focal_mean.y, -EBF_PFS_focal_mean.y, -forestgain_focal_mean.y, 
         -stand_age_focal_mean.y, -tree_cov_focal_mean, - travel_time_to_cities_1_focal_mean, 
         -for_prod_focal_mean, -`fcm_no_gs_model$Groups`)

df_eco_var <- as.data.frame(scale(fcm_eco_attri), na.rm = TRUE)
df_eco_var <- cbind(df_eco_var, fcm_eco_model$Groups)
df_eco_var$rn <- row.names(df_eco_var)
df_eco_all_test <- left_join(df_no_gs_var, df_eco_var, by = "rn") 
df_eco_all <- df_eco_all_test %>%
  select(-PM25F_PFS_focal_mean.y, -WHP_focal_mean.y, -temp_seas_3000m_focal_mean.y, 
         -prec_seas_3000m_focal_mean.y, -roughness_focal_mean.y, -`fcm_no_gs_model$Groups`)

no_gs_var <- df_no_gs_var %>%
  select(-rn) %>%
  group_by(`fcm_no_gs_model$Groups`) %>%
  summarise(across(, ~ var(.x, na.rm = TRUE)))

df_no_gs_z <- as.data.frame(fcm_no_gs_attri, na.rm = TRUE)
df_no_gs_z <- cbind(df_no_gs_z, fcm_no_gs_model$Groups)
df_no_gs_z$rn <- row.names(df_no_gs_z)

# Calculate the z value instead of the variance. Need to use the unscaled attributes
no_gs_z <- df_no_gs_z %>%
  select(-rn) %>%
  group_by(`fcm_no_gs_model$Groups`) %>%
  summarise(across(, ~ mean((.x - mean(.x))/sd(.x), na.rm = TRUE)))

no_gs_var_scaled <- df_no_gs_scaled %>%
  group_by(`fcm_no_gs_model$Groups`) %>%
  summarise(across(, ~ var(.x, na.rm = TRUE)))

rursu1_var <- df_rurs1_var %>%
  group_by(`fcm_rursu01_model$Groups`) %>%
  summarise(across(, ~ var(.x, na.rm = TRUE)))

eco_var <- df_eco_var %>%
  group_by(`fcm_eco_model$Groups`) %>%
  summarise(across(, ~ var(.x, na.rm = TRUE)))

eco_all_var <- df_eco_all %>%
  group_by(`fcm_eco_model$Groups`) %>%
  summarise(across(, ~ var(.x, na.rm = TRUE)))

rursu1_all_var <- df_rurs1_all %>%
  group_by(`fcm_rursu01_model$Groups`) %>%
  summarise(across(, ~ var(.x, na.rm = TRUE)))

no_gs_var_test <- data.frame(Archetype = no_gs_var$`fcm_no_gs_model$Groups`,
                             social_variance = (no_gs_var[2] + no_gs_var[3] + no_gs_var[4] + 
                                                  no_gs_var[5] + no_gs_var[6] + 
                                                  no_gs_var[7] + no_gs_var[9] + no_gs_var[10] + 
                                                  no_gs_var[12] + no_gs_var[16] + no_gs_var[18]) / 11,
                             ecolog_variance = (no_gs_var[8] + 
                                                  no_gs_var[11] + no_gs_var[14] + 
                                                  no_gs_var[13] + no_gs_var[15] + no_gs_var[17] + 
                                                  no_gs_var[19] + no_gs_var[20] + no_gs_var[21]) / 9)


eco_all_var_test <- data.frame(Archetype = eco_all_var$`fcm_eco_model$Groups`,
                             social_variance = (eco_all_var[2] + eco_all_var[3] + eco_all_var[4] + 
                                                  eco_all_var[5] + eco_all_var[6] + 
                                                  eco_all_var[7] + eco_all_var[9] + eco_all_var[10] + 
                                                  eco_all_var[12] + eco_all_var[16] + eco_all_var[18]) / 11,
                             ecolog_variance = (eco_all_var[8] + 
                                                  eco_all_var[11] + eco_all_var[14] + 
                                                  eco_all_var[13] + eco_all_var[15] + eco_all_var[17] + 
                                                  eco_all_var[19] + eco_all_var[20] + eco_all_var[21]) / 9)

rursu1_all_var_test <- data.frame(Archetype = rursu1_all_var$`fcm_rursu01_model$Groups`,
                               social_variance = (rursu1_all_var[2] + rursu1_all_var[3] + rursu1_all_var[4] + 
                                                    rursu1_all_var[5] + rursu1_all_var[6] + 
                                                    rursu1_all_var[7] + rursu1_all_var[9] + rursu1_all_var[10] + 
                                                    rursu1_all_var[12] + rursu1_all_var[17] + rursu1_all_var[22]) / 11,
                               ecolog_variance = (rursu1_all_var[8] + 
                                                    rursu1_all_var[11] + rursu1_all_var[14] + 
                                                    rursu1_all_var[13] + rursu1_all_var[15] + rursu1_all_var[16] + 
                                                    rursu1_all_var[18] + rursu1_all_var[19] + rursu1_all_var[20]) / 9)

no_gs_var_test_scaled <- data.frame(Archetype = no_gs_var_scaled$`fcm_no_gs_model$Groups`,
                                    social_variance = (no_gs_var_scaled[2] + no_gs_var_scaled[3] + no_gs_var_scaled[4] + 
                                                         no_gs_var_scaled[5] + no_gs_var_scaled[6] + 
                                                         no_gs_var_scaled[7] + no_gs_var_scaled[9] + no_gs_var_scaled[10] + 
                                                         no_gs_var_scaled[12] + no_gs_var_scaled[16] + no_gs_var_scaled[18]) / 11,
                                    ecolog_variance = (no_gs_var_scaled[8] + 
                                                         no_gs_var_scaled[11] + no_gs_var_scaled[14] + 
                                                         no_gs_var_scaled[13] + no_gs_var_scaled[15] + no_gs_var_scaled[17] + 
                                                         no_gs_var_scaled[19] + no_gs_var_scaled[20] + no_gs_var_scaled[21]) / 9)


names(no_gs_var_test) <- c("Archetype", "social_variance", "ecolog_variance")
names(eco_all_var_test) <- c("Archetype", "social_variance", "ecolog_variance")
names(rursu1_all_var_test) <- c("Archetype", "social_variance", "ecolog_variance")
names(no_gs_var_test_scaled) <- c("Archetype", "social_variance", "ecolog_variance")

no_gs_prelim_quad <- ggplot(data = no_gs_var_test, 
       aes(x=social_variance, y=ecolog_variance,
           color = Archetype)) +
  geom_point(size = 10) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  xlim(0,2) + 
  ylim(0,2) + 
  scale_color_brewer(labels=c("Arch 1", "Arch 2", 
                              "Arch 3", "Arch 4",
                              "Arch 5", "Arch 6",
                              "Arch 7"),
                     palette = "Set2") +
  labs(title = "Preliminary Quadrants for Potental Fit",
       subtitle = "SES Archetypes",
       x = "Average of Social Attribute Variance",
       y = "Average of Ecological Attribute Variance") +
  theme_bw() + 
  theme(text = element_text(size = 20))
no_gs_prelim_quad
ggsave(paste0("~/Analysis/NEPA_Efficiency/figures/fcm_no_gs_prelim_quadrant_", Sys.Date(), ".png"), 
        plot = no_gs_prelim_quad, width = 8, height = 8, dpi = 300)

ggplot(data = eco_all_var_test, 
       aes(x=social_variance, y=ecolog_variance,
           color = Archetype)) +
  geom_point() +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  xlim(0,2) + 
  ylim(0,2) + 
  scale_color_brewer(labels=c("Arch 1", "Arch 2", 
                              "Arch 3", "Arch 4",
                              "Arch 5", "Arch 6",
                              "Arch 7"),
                     palette = "Set2") +
  labs(title = "Preliminary Quadrants for Potental Fit",
       subtitle = "Ecological Archetypes",
       x = "Average of Social Attribute Variance",
       y = "Average of Ecological Attribute Variance") +
  theme_bw()

ggplot(data = rursu1_all_var_test, 
                  aes(x=social_variance, y=ecolog_variance,
                      color = Archetype)) +
  geom_point() +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  xlim(0,2) + 
  ylim(0,2) + 
  scale_color_brewer(labels=c("Arch 1", "Arch 2", 
                             "Arch 3", "Arch 4",
                             "Arch 5", "Arch 6",
                             "Arch 7"),
                    palette = "Set2") +
  labs(title = "Preliminary Quadrants for Potental Fit",
       subtitle = "RU-RS-U(A) Archetypes",
       x = "Average of Social Attribute Variance",
       y = "Average of Ecological Attribute Variance") +
  theme_bw()





