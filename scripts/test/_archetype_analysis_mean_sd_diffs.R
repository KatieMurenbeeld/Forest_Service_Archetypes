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
library(reshape2)

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

df_all_sc <- as.data.frame((fcm_all_attri - global(fcm_all_attri, "min", na.rm=TRUE)[,1])/(global(fcm_all_attri, "max", na.rm=TRUE)[,1] - global(fcm_all_attri, "min", na.rm=TRUE)[,1]),
                           na.rm = TRUE)
df_pmrc_sc <- as.data.frame((fcm_pmrc_attri - global(fcm_pmrc_attri, "min", na.rm=TRUE)[,1])/(global(fcm_pmrc_attri, "max", na.rm=TRUE)[,1] - global(fcm_pmrc_attri, "min", na.rm=TRUE)[,1]),
                            na.rm = TRUE)
df_pmrc_poli_sc <- as.data.frame((fcm_pmrc_poli_attri - global(fcm_pmrc_poli_attri, "min", na.rm=TRUE)[,1])/(global(fcm_pmrc_poli_attri, "max", na.rm=TRUE)[,1] - global(fcm_pmrc_poli_attri, "min", na.rm=TRUE)[,1]),
                                 na.rm = TRUE)
df_poli_nogs_sc <- as.data.frame((fcm_poli_nogs_attri - global(fcm_poli_nogs_attri, "min", na.rm=TRUE)[,1])/(global(fcm_poli_nogs_attri, "max", na.rm=TRUE)[,1] - global(fcm_poli_nogs_attri, "min", na.rm=TRUE)[,1]),
                                 na.rm = TRUE)

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

all_means <- as.data.frame(mean_diff(df_all_sc, fcm_all$Groups)[1])
pmrc_means <- as.data.frame(mean_diff(df_pmrc_sc, fcm_pmrc$Groups)[1])
pmrc_poli_means <- as.data.frame(mean_diff(df_pmrc_poli_sc, fcm_pmrc_poli$Groups)[1])
poli_nogs1_means <- as.data.frame(mean_diff(df_poli_nogs_sc, fcm_poli_nogs_1$Groups)[1])
poli_nogs2_means <- as.data.frame(mean_diff(df_poli_nogs_sc, fcm_poli_nogs_2$Groups)[1])

all_sd <- as.data.frame(mean_diff(df_all_sc, fcm_all$Groups)[2])
pmrc_sd <- as.data.frame(mean_diff(df_pmrc_sc, fcm_pmrc$Groups)[2])
pmrc_poli_sd <- as.data.frame(mean_diff(df_pmrc_poli_sc, fcm_pmrc_poli$Groups)[2])
poli_nogs1_sd <- as.data.frame(mean_diff(df_poli_nogs_sc, fcm_poli_nogs_1$Groups)[2])
poli_nogs2_sd <- as.data.frame(mean_diff(df_poli_nogs_sc, fcm_poli_nogs_2$Groups)[2])

all_diff_1sd <- as.data.frame(mean_diff(df_all_sc, fcm_all$Groups)[4])
pmrc_diff_1sd <- as.data.frame(mean_diff(df_pmrc_sc, fcm_pmrc$Groups)[4])
pmrc_poli_diff_1sd <- as.data.frame(mean_diff(df_pmrc_poli_sc, fcm_pmrc_poli$Groups)[4])
poli_nogs1_diff_1sd <- as.data.frame(mean_diff(df_poli_nogs_sc, fcm_poli_nogs_1$Groups)[4])
poli_nogs2_diff_1sd <- as.data.frame(mean_diff(df_poli_nogs_sc, fcm_poli_nogs_2$Groups)[4])

## test out heat maps of means and standard deviations

all_means <- data.frame(all_means[,-1], row.names = all_means[,1])
pmrc_means <- data.frame(pmrc_means[,-1], row.names = pmrc_means[,1])
pmrc_poli_means <- data.frame(pmrc_poli_means[,-1], row.names = pmrc_poli_means[,1])
poli_nogs1_means <- data.frame(poli_nogs1_means[,-1], row.names = poli_nogs1_means[,1])
poli_nogs2_means <- data.frame(poli_nogs2_means[,-1], row.names = poli_nogs2_means[,1])

all_sd <- data.frame(all_sd[,-1], row.names = all_sd[,1])
pmrc_sd <- data.frame(pmrc_sd[,-1], row.names = pmrc_sd[,1])
pmrc_poli_sd <- data.frame(pmrc_poli_sd[,-1], row.names = pmrc_poli_sd[,1])
poli_nogs1_sd <- data.frame(poli_nogs1_sd[,-1], row.names = poli_nogs1_sd[,1])
poli_nogs2_sd <- data.frame(poli_nogs2_sd[,-1], row.names = poli_nogs2_sd[,1])

heatmap(as.matrix(all_means), scale = "none", Colv = NA)
heatmap(as.matrix(pmrc_means), scale = "none", Colv = NA)
heatmap(as.matrix(pmrc_poli_means), scale = "none", Colv = NA)
heatmap(as.matrix(poli_nogs1_means), scale = "none", Colv = NA)
heatmap(as.matrix(poli_nogs2_means), scale = "none", Colv = NA)

heatmap(as.matrix(all_sd), scale = "none", Colv = NA)
heatmap(as.matrix(pmrc_sd), scale = "none", Colv = NA)
heatmap(as.matrix(pmrc_poli_sd), scale = "none", Colv = NA)
heatmap(as.matrix(poli_nogs1_sd), scale = "none", Colv = NA)
heatmap(as.matrix(poli_nogs2_sd), scale = "none", Colv = NA)

#all
df_all_means <- melt(as.matrix(all_means))
colnames(df_all_means) <- c("x", "y", "mean")
df_all_sds <- melt(as.matrix(all_sd))
colnames(df_all_sds) <- c("x", "y", "sd")

all_means_hm <- ggplot(df_all_means, aes(x = x, y = y, fill = mean)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
all_means_hm

all_sds_hm <- ggplot(df_all_sds, aes(x = x, y = y, fill = sd)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
all_sds_hm

#pmrc
df_pmrc_means <- melt(as.matrix(pmrc_means))
colnames(df_pmrc_means) <- c("x", "y", "mean")
df_pmrc_sds <- melt(as.matrix(pmrc_sd))
colnames(df_pmrc_sds) <- c("x", "y", "sd")

pmrc_means_hm <- ggplot(df_pmrc_means, aes(x = x, y = y, fill = mean)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
pmrc_means_hm

pmrc_sds_hm <- ggplot(df_pmrc_sds, aes(x = x, y = y, fill = sd)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
pmrc_sds_hm

#pmrc poli
df_pmrc_poli_means <- melt(as.matrix(pmrc_poli_means))
colnames(df_pmrc_poli_means) <- c("x", "y", "mean")
df_pmrc_poli_sds <- melt(as.matrix(pmrc_poli_sd))
colnames(df_pmrc_poli_sds) <- c("x", "y", "sd")

pmrc_poli_means_hm <- ggplot(df_pmrc_poli_means, aes(x = x, y = y, fill = mean)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
pmrc_poli_means_hm

pmrc_poli_sds_hm <- ggplot(df_pmrc_poli_sds, aes(x = x, y = y, fill = sd)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
pmrc_poli_sds_hm

#poli nogs1
df_poli_nogs1_means <- melt(as.matrix(poli_nogs1_means))
colnames(df_poli_nogs1_means) <- c("x", "y", "mean")
df_poli_nogs1_sds <- melt(as.matrix(poli_nogs1_sd))
colnames(df_poli_nogs1_sds) <- c("x", "y", "sd")

poli_nogs1_means_hm <- ggplot(df_poli_nogs1_means, aes(x = x, y = y, fill = mean)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
poli_nogs1_means_hm

poli_nogs1_sds_hm <- ggplot(df_poli_nogs1_sds, aes(x = x, y = y, fill = sd)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
poli_nogs1_sds_hm

#poli nogs2
df_poli_nogs2_means <- melt(as.matrix(poli_nogs2_means))
colnames(df_poli_nogs2_means) <- c("x", "y", "mean")
df_poli_nogs2_sds <- melt(as.matrix(poli_nogs2_sd))
colnames(df_poli_nogs2_sds) <- c("x", "y", "sd")

poli_nogs2_means_hm <- ggplot(df_poli_nogs2_means, aes(x = x, y = y, fill = mean)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
poli_nogs2_means_hm

poli_nogs2_sds_hm <- ggplot(df_poli_nogs2_sds, aes(x = x, y = y, fill = sd)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "forestgreen")
poli_nogs2_sds_hm

# might be easier way to do this?
summarizeClusters(df_all,
                  belongmatrix = fcm_all$Belongings,
                  weighted = TRUE, dec = 3)


