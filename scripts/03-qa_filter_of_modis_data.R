#'@author  Fernando Prudencio
#'this script filter mod09a1 dataset
#'installing packages
rm(list = ls())
pkg <- c('DescTools', 'dplyr', 'raster', 'dfcrm')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) }})

library(DescTools)
library(dplyr)
library(raster)
#'reading functions
load('scripts/functions.R')

#'links
rutIN  <- 'G:/data/mod09a1/withoutFILTER/'
rutOUT <- 'G:/data/mod09a1/withFILTER/'

#'this is the order of 16 bits of the quality band
# (15)(14)(13)(12)(11)(10)(09)(08)(07)(06)(05)(04)(03)(02)(01)(00) - MODIS NOMENCLATURE
# (01)(02)(03)(04)(05)(06)(07)(08)(09)(10)(11)(12)(13)(14)(15)(16) - R NOMENCLATURE

filter <- list(c('01','10'), NA, c('000', '010', '011', '100', '101', '110', '111'),
                 "11", NA, '1', NA, '1', '1', NA, '1')
#'list of qa and reflectivity data
band_list <- list.files(rutIN, pattern = 'refl_b02', full.names = T)
qa_list   <- list.files(rutIN, pattern = 'state', full.names = T)

sapply(c(1:5), FUN = function(x){ file = qaFilter(band_list[x], qa_list[x], 'mod09a1', filter)
                                         name = band_list[x] %>% strsplit('/') %>% sapply('[',5) %>% 
                                                        substr(1,35) %>% paste('_qafilter.tif', sep = '')
                                         writeRaster(file, paste(rutOUT, name, sep = '')) })
