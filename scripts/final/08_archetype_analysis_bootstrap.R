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


all_melted_df <- reshape2::melt(all_bootvalues$group_consistency)

all_melted_df$variable <- as.factor(all_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = all_melted_df, bins = 30) +
  labs(title = "stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=3)

#----PMRC----
pmrc_bootvalues <- boot_group_validation(fcm_pmrc, nsim = 100, maxiter = 100, 
                                        tol = 0.0001, verbose = FALSE)

pmrc_melted_df <- reshape2::melt(pmrc_bootvalues$group_consistency)

pmrc_melted_df$variable <- as.factor(pmrc_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = pmrc_melted_df, bins = 30) +
  labs(title = "stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=2)

#---PMRC-Poli----
pmrc-poli_bootvalues <- boot_group_validation(fcm_pmrc_poli, nsim = 100, maxiter = 100, 
                                         tol = 0.0001, verbose = FALSE)

pmrc-poli_melted_df <- reshape2::melt(pmrc-poli_bootvalues$group_consistency)

pmrc-poli_melted_df$variable <- as.factor(pmrc-poli_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = pmrc-poli_melted_df, bins = 30) +
  labs(title = "stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=2)

#----Poli-no-GS-01----
poli_nogs01_bootvalues <- boot_group_validation(fcm_poli_nogs_1, nsim = 100, maxiter = 100, 
                                              tol = 0.0001, verbose = FALSE)

poli_nogs01_melted_df <- reshape2::melt(poli_nogs01_bootvalues$group_consistency)

poli_nogs01_melted_df$variable <- as.factor(poli_nogs01_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = poli_nogs01_melted_df, bins = 30) +
  labs(title = "stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=2)

#----Poli-no-GS-02----
poli_nogs02_bootvalues <- boot_group_validation(fcm_poli_nogs_2, nsim = 100, maxiter = 100, 
                                                tol = 0.0001, verbose = FALSE)

poli_nogs02_melted_df <- reshape2::melt(poli_nogs02_bootvalues$group_consistency)

poli_nogs02_melted_df$variable <- as.factor(poli_nogs02_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = poli_nogs02_melted_df, bins = 30) +
  labs(title = "stability of clusters", subtitle = "for 100 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=2)