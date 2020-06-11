#'@author  Fernando Prudencio
#'this script calculate a monthly composite about maximum value of
#'vegetation index
#'installing packages
rm(list = ls())
pkg <- c('sf', 'dplyr', 'raster', 'dfcrm', 'rgdal', 'doParallel', 'foreach')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) } })

library(raster)
library(tidyverse)

#'reading data in .RDtada files
load('info/mod09a01_timeSerie.RData')

#'reading raster data
index_list <- list.files('data/index/ndwi_mod09a1/', pattern = '.tif', full.names = T)

#'making a raster composite
for (year in 2004:2019) {
for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')) {
    df = tibble(date = ts) %>% mutate(id = c(1:length(date))) %>%
                            filter(substr(date,1,4) == year & substr(date,6,7) == month)
    index = stack(index_list[df$id]) %>% max(na.rm = T)
    name  = index_list[df$id][1] %>% strsplit('/') %>% sapply('[',4) %>% substr(1,28)
    name2 = paste('data/index/ndwi_mod09a1/monthly_composite/', name, month, '_composite.tif', sep = '')
    writeRaster(index, name2, overwrite = T)
  }
}

writeRaster('')