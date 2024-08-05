library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(raster)

# Load the data
rst_fcm_pmrc_poli_sc <- rast(here::here("data/processed/rst_fcm_pmrc_poli_sc.tif"))

# Format for use in geocmeans
dataset_pmrc_poli <- lapply(names(rst_fcm_pmrc_poli_sc), function(n){
  aband <- rst_fcm_pmrc_poli_sc[[n]]
  return(aband)
})
names(dataset_pmrc_poli) <- names(rst_fcm_pmrc_poli_sc)

#----Generalized Fuzzy Cmeans----
## Find optimal beta value
GFCMvalues <- select_parameters.mc(algo = "GFCM", data = dataset_pmrc_poli,
                                   k = 8, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
                                   spconsist = FALSE, verbose = TRUE, init = "kpp",
                                   indices = c("XieBeni.index", "Explained.inertia",
                                               "Negentropy.index", "Silhouette.index"))

ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = beta, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

## Run the GFCM standardize is set to FALSE because the data is scaled
GFCM_result <- GCMeans(dataset_pmrc_poli, k = 8, m = 1.625, beta = 0.125, standardize = FALSE,
                       verbose = FALSE, seed = 6891, tol = 0.001)

##----Check the probability of belonging for each cluster----
### using different undecided thresholds
maps2 <- mapClusters(object = GFCM_result, undecided = 0.45)
maps2$ClusterPlot + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Set3")

maps2$ProbaMaps[[1]] + theme(legend.position = "bottom")

maps2$ProbaMaps[[2]] + theme(legend.position = "bottom")

maps2$ProbaMaps[[3]] + theme(legend.position = "bottom")

maps2$ProbaMaps[[4]] + theme(legend.position = "bottom")

maps2$ProbaMaps[[5]] + theme(legend.position = "bottom")

maps2$ProbaMaps[[6]] + theme(legend.position = "bottom")

maps2$ProbaMaps[[7]] + theme(legend.position = "bottom")

maps2$ProbaMaps[[8]] + theme(legend.position = "bottom")

