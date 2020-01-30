#'@author  Fernando Prudencio
#'this script calculate different index from mod09a1 dataset
#'installing packages
rm(list = ls())
pkg <- c('DescTools', 'dplyr', 'raster', 'dfcrm', 'rgdal', 'doParallel', 'foreach')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) }})

library(doParallel)
library(foreach)

#'links, these could change with ubication of files
rutIN  <- 'D:/01-DataSet/2-myd09a1/withFILTER/'
b2 <- list.files(rutIN, pattern = 'refl_b02', full.names = T)
b1 <- list.files(rutIN, pattern = 'refl_b01', full.names = T)

#'reading functions
source('scripts/functions.R')

#'Define how many cluster you want to use
UseCores <- detectCores() - 2

#'make and register cluster
cluster  <- makeCluster(UseCores)
registerDoParallel(cluster)

#'Use foreach loop and %dopar% command to run in parallel
foreach(i = c(1:805)) %dopar% {
  library(dplyr)
  library(raster)
  library(rgdal)
  file  = indexMODIS('ndvi', nirBand = raster(b2[i]), redBand = raster(b1[i]))
  name  = b2[i] %>% strsplit('/') %>% sapply('[',5) %>% substr(26,35)
  name2 = paste('data/ndvi_myd09a1/MYD09A1.006_sur_NDVI_', name, '.tif', sep = '')
  writeRaster(file, name2)
}

#'end cluster
stopCluster(cluster)