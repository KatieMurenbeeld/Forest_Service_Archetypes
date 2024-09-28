library(geocmeans)
library(terra)

Arcachon <- terra::rast(system.file("extdata/Littoral4_2154.tif", package = "geocmeans"))
names(Arcachon) <- c("blue", "green", "red", "infrared", "SWIR1", "SWIR2")

dataset <- lapply(names(Arcachon), function(n){
  aband <- Arcachon[[n]]
  return(aband)
})

# giving a name to each band
names(dataset) <- names(Arcachon)

w1 <- matrix(1, nrow = 3, ncol = 3)

SGFCM_result <- SGFCMeans(dataset, k = 7, m = 1.5, standardize = TRUE,
                          lag_method = "mean",
                          window = w1, alpha = 0.9, beta = 0.5,
                          seed = 789, tol = 0.001, verbose = TRUE, init = "kpp")

# running the bootstrap validation
bootvalues <- boot_group_validation(SGFCM_result, nsim = 10, maxiter = 10, 
                                    tol = 0.0001, verbose = FALSE)


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

bootvalues_test <- boot_group_validation_test(SGFCM_result, nsim = 10, maxiter = 10, 
                                              tol = 0.0001, verbose = TRUE)
