library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(raster)

# Load previous models
fcm_all <- readRDS("data/processed/FCM_all_2024-07-22.rds")
fcm_pmrc <- readRDS("data/processed/FCM_pmrc_2024-07-22.rds")
fcm_pmrc_poli <- readRDS("data/processed/FCM_pmrc_poli_2024-07-22.rds")
fcm_poli_nogs_1 <- readRDS("data/processed/FCM_poli_nogs_01_2024-07-22.rds")
fcm_poli_nogs_2 <- readRDS("data/processed/FCM_poli_nogs_02_2024-07-22.rds")

# Run bootstrap 
# only nsim=100 and maxiter=100 to save time

#----All----
all_bootvalues <- boot_group_validation(fcm_all, nsim = 100, maxiter = 100, 
                                    tol = 0.0001, verbose = FALSE)
saveRDS(all_bootvalues, here::here("data/processed/all_bootvalues.RDS"))

all_melted_df <- reshape2::melt(all_bootvalues$group_consistency)

all_melted_df$variable <- as.factor(all_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = all_melted_df, bins = 30) +
  labs(title = "all attributes: stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=3)

#----PMRC----
pmrc_bootvalues <- boot_group_validation(fcm_pmrc, nsim = 100, maxiter = 100, 
                                        tol = 0.0001, verbose = FALSE)
saveRDS(pmrc_bootvalues, here::here("data/processed/pmrc_bootvalues.RDS"))

pmrc_melted_df <- reshape2::melt(pmrc_bootvalues$group_consistency)

pmrc_melted_df$variable <- as.factor(pmrc_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = pmrc_melted_df, bins = 30) +
  labs(title = "pmrc attributes: stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=2)

pmrc_boot <- readRDS(here::here("data/processed/pmrc_bootvalues.RDS"))

df_pmrc_gp6 <- pmrc_boot$group_centers[["group6"]]

melted_df <- reshape2::melt(df_pmrc_gp6)

melted_df$variable <- as.factor(melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = melted_df, bins = 100) +
  labs(title = "stability of group 6 centers", subtitle = "for 100 iterations") +
  xlim(-3,3)+
  facet_wrap(vars(variable), ncol=4)

#---PMRC-Poli----
pmrc_poli_bootvalues <- boot_group_validation(fcm_pmrc_poli, nsim = 100, maxiter = 100, 
                                         tol = 0.0001, verbose = FALSE)
saveRDS(pmrc_poli_bootvalues, here::here("data/processed/pmrc_poli_bootvalues.RDS"))


pmrc_poli_melted_df <- reshape2::melt(pmrc_poli_bootvalues$group_consistency)

pmrc_poli_melted_df$variable <- as.factor(pmrc_poli_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = pmrc_poli_melted_df, bins = 30) +
  labs(title = "pmrc with aip: stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=2)

pmrc_poli_boot <- readRDS(here::here("data/processed/pmrc_poli_bootvalues.RDS"))

df_pmrc_poli_gp8 <- pmrc_poli_boot$group_centers[["group8"]]

melted_df <- reshape2::melt(df_pmrc_poli_gp8)

melted_df$variable <- as.factor(melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = melted_df, bins = 100) +
  labs(title = "stability of group 8 centers", subtitle = "for 100 iterations") +
  xlim(-3,3)+
  facet_wrap(vars(variable), ncol=4)

#----Poli-no-GS-01----
poli_nogs01_bootvalues <- boot_group_validation(fcm_poli_nogs_1, nsim = 100, maxiter = 100, 
                                              tol = 0.0001, verbose = FALSE)
saveRDS(poli_nogs01_bootvalues, here::here("data/processed/poli_nogs01_bootvalues.RDS"))

poli_nogs01_melted_df <- reshape2::melt(poli_nogs01_bootvalues$group_consistency)

poli_nogs01_melted_df$variable <- as.factor(poli_nogs01_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = poli_nogs01_melted_df, bins = 30) +
  labs(title = "no GS with aip v01:stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=3)

#----Poli-no-GS-02----
poli_nogs02_bootvalues <- boot_group_validation(fcm_poli_nogs_2, nsim = 100, maxiter = 100, 
                                                tol = 0.0001, verbose = FALSE)
saveRDS(poli_nogs02_bootvalues, here::here("data/processed/poli_nogs02_bootvalues.RDS"))

poli_nogs02_melted_df <- reshape2::melt(poli_nogs02_bootvalues$group_consistency)

poli_nogs02_melted_df$variable <- as.factor(poli_nogs02_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = poli_nogs02_melted_df, bins = 30) +
  labs(title = "no GS with aip v02: stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=2)

poli_nogs2_boot <- readRDS(here::here("data/processed/poli_nogs02_bootvalues.RDS"))

df_poli_nogs2_grp2 <- poli_nogs2_boot$group_centers[["group2"]]

melted_df <- reshape2::melt(df_poli_nogs2_grp2)

melted_df$variable <- as.factor(melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = melted_df, bins = 100) +
  labs(title = "stability of group 2 centers", subtitle = "for 100 iterations") +
  xlim(-3,3)+
  facet_wrap(vars(variable), ncol=4)

