#'@author  Fernando Prudencio
#'this script filter mod09a1 dataset
#'installing packages
rm(list = ls())
pkg <- c('DescTools', 'dplyr', 'raster', 'dfcrm', 'rgdal')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) }})

library(DescTools)
library(dplyr)
library(raster)
library(rgdal)
#'reading functions
source('scripts/functions.R')

#'links, these could change with ubication of files
rutIN  <- 'D:/01-DataSet/2-myd09a1/withoutFILTER/'
rutOUT <- 'D:/01-DataSet/2-myd09a1/withFILTER/'

#'this is the order of 16 bits of the quality band
# (15)(14)(13)(12)(11)(10)(09)(08)(07)(06)(05)(04)(03)(02)(01)(00) - MODIS NOMENCLATURE
# (01)(02)(03)(04)(05)(06)(07)(08)(09)(10)(11)(12)(13)(14)(15)(16) - R NOMENCLATURE
#' Cloud State ------------------------------> bit 00-01
#' Cloud shadow -----------------------------> bit 02
#' Land/Water Flag --------------------------> bit 03-05
#' Aerosol Quantity -------------------------> bit 06-07
#' Cirrus detected --------------------------> bit 08-09
#' Internal Cloud Algorithm Flag ------------> bit 10
#' Internal Fire Algorithm ------------------> bit 11
#' MOD35 snow/ice flag ----------------------> bit 12
#' Pixel adjacent to cloud ------------------> bit 13
#' BRDF correction performed ----------------> bit 14
#' Internal Snow Mask -----------------------> bit 15

filter <- list(c('01','10'), NA, c('000', '010', '011', '100', '101', '110', '111'),
                 c('11'), c('11'), '1', NA, '1', NA, '1', '1')
#'list of qa and reflectivity data
band_list <- list.files(rutIN, pattern = 'refl_b02', full.names = T)
qa_list   <- list.files(rutIN, pattern = 'state', full.names = T)

sapply(4, FUN = function(x){file = qaFilter(band_list[x], qa_list[x], 'mod09a1', filter)
                                 name = band_list[x] %>% strsplit('/') %>% sapply('[',5) %>% 
                                            substr(1,35) %>% paste('_qafilter.tif', sep = '')
                                 writeRaster(file, paste(rutOUT, name, sep = '')) })
