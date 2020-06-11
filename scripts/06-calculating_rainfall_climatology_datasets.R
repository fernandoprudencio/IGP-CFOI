#'@author  Fernando Prudencio
#'this script calculate a rainfall climatology
rm(list = ls())
pkg <- c('sf', 'tidyverse', 'raster')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) } })

library(sf)
library(tidyverse)
library(raster)

#'reading raster data
pp <- brick('data/rainfall/PISCOp_v2.1.1.nc')
for (i in 1:12) {
  month = c('01','02','03','04','05','06','07','08','09','10','11','12')
  month.lbl = month.name %>% substr(1,3)
  df    = tibble(date = seq(as.Date('1981-01-01'), as.Date('2018-02-01'), by = "month")) %>% mutate(id = 1:length(date))
  bands = df %>% dplyr::filter(substr(date,1,4) %in% c(2002:2017) &
                                  !(substr(date,1,4) %in% c('2005', '2010', '2016')) & substr(date,6,7) == month[i])
  img   = pp[[bands$id]] %>% mean(na.rm = T)
  writeRaster(img, paste('data/rainfall/climatology/PISCOp_v2.1.1_', month[i], month.lbl[i], '.tif', sep = ''))
}
