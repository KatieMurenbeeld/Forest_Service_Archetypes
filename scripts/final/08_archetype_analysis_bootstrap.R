library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(raster)
library(future)

# Load previous models
fcm_all <- readRDS("data/processed/FCM_all_2024-07-22.rds")
fcm_pmrc <- readRDS("data/processed/FCM_pmrc_2024-07-22.rds")
fcm_pmrc_poli <- readRDS("data/processed/FCM_pmrc_poli_2024-07-22.rds")
fcm_poli_nogs_1 <- readRDS("data/processed/FCM_poli_nogs_01_2024-07-22.rds")
fcm_poli_nogs_2 <- readRDS("data/processed/FCM_poli_nogs_02_2024-07-22.rds")
sgfcm_pmrc_poli <- readRDS("data/processed/SGFCM_result_pmrc_poli_2024-08-22.rds")

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
                                         tol = 0.0001, verbose = TRUE)
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

#----SGFCM PMRC Poli-----
rst_fcm_pmrc_poli_sc <- rast(here::here("data/processed/rst_fcm_pmrc_poli_sc_2024-08-12.tif"))
rst_fcm_pmrc_poli <- rast(here::here("data/processed/rast_fcm_pmrc2024_ploi_2024-08-12.tif"))

# Format for use in geocmeans
dataset_pmrc_poli <- lapply(names(rst_fcm_pmrc_poli_sc), function(n){
  aband <- rst_fcm_pmrc_poli_sc[[n]]
  return(aband)
})
names(dataset_pmrc_poli) <- names(rst_fcm_pmrc_poli_sc)

w1 <- matrix(1, nrow = 3, ncol = 3)

SGFCM_result <- SGFCMeans(dataset_pmrc_poli, k = 8, m = 1.6, standardize = FALSE,
                          lag_method = "mean",
                          window = w1, alpha = 1.3, beta = 0.1,
                          seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")
## Check the result
class(SGFCM_result)
maps_sgfcm <- mapClusters(object = SGFCM_result, undecided = 0.2)
# plotting the most likely categories
maps_sgfcm$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set3")

# For some reason the boot_group_validation() function isn't working
future::plan(future::multisession(workers=2))

# set SGFCM_result$isRaster <- FALSE
# see line lines 68-91...I think that the wdata can be calculated from the SGFCM results without
# having to use terra::values() which is causing the error
# to avoid this for now set SGFCM_result$isRaster <- FALSE 
# code from https://github.com/JeremyGelb/geocmeans/blob/master/R/boostrap_clust_validation.R
#SGFCM_result$isRaster <- FALSE
sgfcm_pmrc_poli_bootvalues <- boot_group_validation(SGFCM_result, nsim = 10, maxiter = 10, 
                                        tol = 0.0001, verbose = TRUE)

saveRDS(sgfcm_pmrc_poli_bootvalues, here::here("data/processed/sgfcm_pmrc_poli_bootvalues_test.RDS"))

sgfcm_melted_df <- reshape2::melt(sgfcm_pmrc_poli_bootvalues$group_consistency)

sgfcm_melted_df$variable <- as.factor(sgfcm_melted_df$variable)

ggplot() +
  geom_histogram(mapping = aes(x = value), data = sgfcm_melted_df, bins = 30) +
  labs(title = "all attributes: stability of clusters", subtitle = "for 1000 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=3)


### Troubleshooting
# code from https://github.com/JeremyGelb/geocmeans/blob/master/R/boostrap_clust_validation.R

rast_test <- SGFCM_result$rasters[[1]]
vec_test <- rep(NA, times = terra::ncell(rast_test))
vec[SGFCM_result$missing] <- SGFCM_result$Data[,i]


dataset <- lapply(1:ncol(SGFCM_result$Data), function(i){
  rast <- SGFCM_result$rasters[[1]]
  vec <- rep(NA, times = terra::ncell(rast))
  vec[SGFCM_result$missing] <- SGFCM_result$Data[,i]
  terra::values(rast, mat = FALSE) <- vec
  return(rast)
})

boot_group_validation_test <- function(object, nsim = 1000, maxiter = 1000, tol = 0.01, init = "random", verbose = TRUE, seed = NULL){
  
  if(object$algo %in% c("FCM", "GFCM", "SFCM", "SGFCM") == FALSE){
    stop('bootstrap group validation can only be performed for FCMres object
         if algo is one of "FCM", "GFCM", "SFCM", "SGFCM"')
  }
  
  ## calulating the lagged dataset if necessary -----------------------
  if(object$algo %in% c("SGFCM", "SFCM")){
    if(object$isRaster == FALSE){
      wdata <- calcLaggedData(object$Data, object$listw, object$lag_method)
    }else{
      dataset <- lapply(1:ncol(object$Data), function(i){
        rast <- object$rasters[[1]]
        vec <- rep(NA, times = terra::ncell(rast))
        vec[object$missing] <- object$Data[,i]
        terra::values(rast) <- vec # take out the mat = FALSE argument in values()
        return(rast)
      })
      wdata <- calcWdataRaster(object$window, dataset, object$lag_method, object$missing)
    }
    
  }else{
    wdata <- NULL
  }
  
  if(is.null(seed) == FALSE){
    set.seed(seed)
  }
  
  k <- ncol(object$Belongings)
  
  ## Starting the iteration of the boostraping -----------------------
  if(verbose){
    pb <- txtProgressBar(1, nsim, style = 3)
    print("Starting the bootstrap iterations...")
  }
  
  all_perm <- lapply(1:nsim, function(i){
    su_res <- boot_worker(object, wdata, tol, maxiter, init)
    if (verbose){
      setTxtProgressBar(pb, i)
    }
    
    return(su_res)
  })
  
  ## calculating the consistency of clusters -----------------------
  
  # the format of this table is :
  # rows are the permutated clusters
  # columns are the original clusters
  if(verbose){
    print("Calculating the Jaccard values...")
  }
  
  cons_values <- lapply(all_perm, function(results){
    matX <- results$Belongings
    matY <- object$Belongings[results$idx,]
    qual_mat <- calc_jaccard_mat(matY,matX)
    colnames(qual_mat) <- 1:ncol(qual_mat)
    rownames(qual_mat) <- 1:nrow(qual_mat)
    
    # we must pair the clusters of the original partition
    # and the permutated ones, starting by the best pair
    # and then decreasing. To do so, I edit the matrix
    # and remove the rows / columns used.
    clst_consist <- rep(-1, times = k)
    matidx <- rep(-1, times = k)
    for (j in 1:(k-1)){
      d <- j-1
      best <- which(qual_mat == max(qual_mat), arr.ind = TRUE)
      if(length(best) > 2){
        best <- best[1,]
      }
      c1 <- as.numeric(colnames(qual_mat)[[best[[2]]]])
      r1 <- as.numeric(rownames(qual_mat)[[best[[1]]]])
      
      clst_consist[[c1]] <- qual_mat[best[[1]], best[[2]]]
      matidx[[c1]] <- r1
      rkeep <- 1:(k-j+1)
      rkeep <- rkeep != best[[1]]
      ckeep <- 1:(k-j+1)
      ckeep <- ckeep != best[[2]]
      qual_mat <- qual_mat[rkeep,ckeep]
    }
    
    missing <- (1:k)[(1:k) %in% matidx == F]
    clst_consist[clst_consist == -1] <- qual_mat
    matidx[matidx == -1] <- missing
    
    return(list(clst_consist,matidx))
    
  })
  mat_valid <- do.call(rbind, lapply(cons_values, function(v){v[[1]]}))
  mat_idx <- do.call(rbind, lapply(cons_values, function(v){v[[2]]}))
  
  ## creating a list with the centres values boostraped of clusters -----------------------
  print("Extracting the centres of the clusters...")
  
  clust_table <- do.call(rbind,lapply(1:nsim, function(j){
    all_perm[[j]]$Centers[mat_idx[j,],]
  }))
  
  clust_table <- as.data.frame(clust_table)
  colnames(clust_table) <- colnames(object$Data)
  
  gp <- rep(1:k, times = nsim)
  clust_centers <- split(clust_table,f = as.factor(gp))
  names(clust_centers) <- paste("group",1:k, sep = "")
  mat_valid <- data.frame(mat_valid)
  names(mat_valid) <- paste("group",1:k, sep = "")
  return(
    list("group_consistency" = mat_valid,
         "group_centers" = clust_centers
    )
  )
}

sgfcm_pmrc_poli_bootvalues_test <- boot_group_validation_test(SGFCM_result, nsim = 10, maxiter = 10, 
                                                    tol = 0.0001, verbose = TRUE)

sgfcm_pmrc_poli_bootvalues <- boot_group_validation_test(SGFCM_result, nsim = 100, maxiter = 100, 
                                                              tol = 0.0001, verbose = TRUE)

sgfcm_pmrc_poli_bootvalues1000 <- boot_group_validation_test(SGFCM_result, nsim = 1000, maxiter = 1000, 
                                                         tol = 0.0001, verbose = TRUE)
# Keeps crashing due to memory. Need to run on Borah.

#sgfcm_melted_df <- reshape2::melt(sgfcm_pmrc_poli_bootvalues_test$group_consistency)
sgfcm_melted_df <- reshape2::melt(sgfcm_pmrc_poli_bootvalues_test$group_consistency)

sgfcm_melted_df$variable <- as.factor(sgfcm_melted_df$variable)

sgfcm_pmrc_poli_boot100 <- ggplot() +
  geom_histogram(mapping = aes(x = value), data = sgfcm_melted_df, bins = 30) +
  labs(title = "all attributes: stability of clusters", subtitle = "for 1000 iterations",
       x = "Jaccard index") +
  facet_wrap(vars(variable), ncol=3)
sgfcm_pmrc_poli_boot100

ggsave(filename = paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/figures/sgfcm_pmrc_poli_boot100_", Sys.Date(), ".png"),
       plot = sgfcm_pmrc_poli_boot100, height = 4, width = 4, dpi = 300)

