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
fcm_all_attri <- rast("data/processed/rast_fcm_all_2024-07-22.tif")
fcm_all_result <- rast("data/processed/FCM_all_2024-07-22.tif") 
fcm_all <- readRDS("data/processed/FCM_all_2024-07-22.rds")

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

# create dataframes
df_all <- as.data.frame(fcm_all_attri, na.rm = TRUE)
df_pmrc <- as.data.frame(fcm_pmrc_attri, na.rm = TRUE)
df_pmrc_poli <- as.data.frame(fcm_pmrc_poli_attri, na.rm = TRUE)
df_poli_nogs <- as.data.frame(fcm_poli_nogs_attri, na.rm = TRUE)

## Which variables were most different from the mean?
df_all_groups <- cbind(df_all, fcm_all$Groups)

df_all_group_means <- df_all_groups %>%
  group_by(`fcm_all$Groups`) %>%
  summarise(across(, ~ mean(.x)))

df_all_group_sds <- df_all_groups %>%
  group_by(`fcm_all$Groups`) %>%
  summarise(across(, ~ sd(.x)))

df_all_total_means <- df_all_groups %>%
  dplyr::select(-`fcm_all$Groups`) %>%
  summarise(across(, ~ mean(.x)))

df_all_total_sds <- df_all_groups %>%
  dplyr::select(-`fcm_all$Groups`) %>%
  summarise(across(, ~ sd(.x)))

df_all_mean_diff <- df_all_group_means
for (i in 1:23) {
  df_all_mean_diff[i+1] <- df_all_group_means[i+1] - df_all_total_means[[i]]
}
df_all_mean_diff_1sd <- df_all_mean_diff
for (i in 1:23) {
  df_all_mean_diff_1sd[i+1] <- abs(df_all_mean_diff[i+1]) > df_all_total_sds[[i]]
}

# Let's try to make the above a function

mean_diff <- function(attribute_data, groups){
  df_groups <- cbind(attribute_data, groups)
  df_group_means <- df_groups %>%
    group_by(`groups`) %>%
    summarise(across(, ~ mean(.x)))
  
  df_group_sds <- df_groups %>%
    group_by(`groups`) %>%
    summarise(across(, ~ sd(.x)))
  
  df_total_means <- df_groups %>%
    dplyr::select(-`groups`) %>%
    summarise(across(, ~ mean(.x)))
  
  df_total_sds <- df_groups %>%
    dplyr::select(-`groups`) %>%
    summarise(across(, ~ sd(.x)))
  
  df_mean_diff <- df_group_means
  for (i in 1:length(attribute_data)) {
    df_mean_diff[i+1] <- df_group_means[i+1] - df_total_means[[i]]
  }
  df_mean_diff_1sd <- df_mean_diff
  for (i in 1:length(attribute_data)) {
    df_mean_diff_1sd[i+1] <- abs(df_mean_diff[i+1]) > df_total_sds[[i]]
  }
  return(list(df_group_means, df_group_sds, df_mean_diff, df_mean_diff_1sd))
}

all_means <- as.data.frame(mean_diff(df_all, fcm_all$Groups)[1])
pmrc_means <- as.data.frame(mean_diff(df_pmrc, fcm_pmrc$Groups)[1])
pmrc_poli_means <- as.data.frame(mean_diff(df_pmrc_poli, fcm_pmrc_poli$Groups)[1])
poli_nogs1_means <- as.data.frame(mean_diff(df_poli_nogs, fcm_poli_nogs_1$Groups)[1])
poli_nogs2_means <- as.data.frame(mean_diff(df_poli_nogs, fcm_poli_nogs_2$Groups)[1])

all_diff_1sd <- as.data.frame(mean_diff(df_all, fcm_all$Groups)[4])
pmrc_diff_1sd <- as.data.frame(mean_diff(df_pmrc, fcm_pmrc$Groups)[4])
pmrc_poli_diff_1sd <- as.data.frame(mean_diff(df_pmrc_poli, fcm_pmrc_poli$Groups)[4])
poli_nogs1_diff_1sd <- as.data.frame(mean_diff(df_poli_nogs, fcm_poli_nogs_1$Groups)[4])
poli_nogs2_diff_1sd <- as.data.frame(mean_diff(df_poli_nogs, fcm_poli_nogs_2$Groups)[4])



