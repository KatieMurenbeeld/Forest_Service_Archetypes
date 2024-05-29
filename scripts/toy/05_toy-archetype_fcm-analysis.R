library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)

#---Load the data-----
rst_fcm <- rast(here::here("data/toy/processed/toy_rast_fcm_2024-05-29.tif"))

# Scale the data
rst_fcm_sc <- scale(rst_fcm)

# Convert to a simple list of SpatRaster
dataset <- lapply(names(rst_fcm_sc), function(n){
  aband <- rst_fcm_sc[[n]]
  return(aband)
})

names(dataset) <- names(rst_fcm_sc)

#---Run the FCM---- 
FCM_result <- CMeans(dataset, k = 6, m = 1.75, standardize = FALSE)
map.res <- rast(FCM_result$rasters)
writeRaster(map.res[["Groups"]], filename = paste0("data/toy/processed/FCM_", Sys.Date(), ".tif"), overwrite = TRUE)


##---save the iteration, k, m as a dataframe----
aa_iteration <- data.frame(iteration_name = character(),
                           attris = character(),
                           k = numeric(),
                           m = numeric())

iteration_name <- "SPASES_toy_"
attris <- paste(names(rst_fcm), collapse= ", ")
k <- 6
m <- 1.75

aa_iteration[nrow(aa_iteration) + 1,] <- list(iteration_name, attris, k, m)

write_csv(aa_iteration, here::here("outputs/toy/aa_iteration.csv"), append = TRUE)
