#'@author  Fernando Prudencio
#'this script does a cluster analysis
rm(list = ls())
pkg <- c('sf', 'tidyverse', 'raster')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) } })

library(sf)
library(tidyverse)
library(raster)

#'reading raster data
index_list = list.files('data/index/ndwi_mod09a1/monthly_composite_b2_and_b7/', pattern = '.tif', full.names = T)

for (i in 1:12) {
  month = c('01','02','03','04','05','06','07','08','09','10','11','12')
  month.lbl = month.name %>% substr(1,3)
  df = tibble(date = seq(as.Date('2002-01-01'), as.Date('2019-12-01'), by = 'month'), id = 1:length(date)) %>% 
    filter(!(substr(date,1,4) %in% c(2005,2010,2016,2019)) & substr(date,6,7) == month[i])
  index = stack(index_list[df$id]) %>% mean(na.rm = T)
  index[index < -1] = NA
  index[index > 1] = NA
  name  = index_list[df$id][1] %>% strsplit('/') %>% sapply('[',5) %>% substr(1,21)
  name2 = paste('data/index/ndwi_mod09a1/climatology/', name, month[i], month.lbl[i], '.tif', sep = '')
  writeRaster(index, name2, overwrite = T)
}
