#' @title
#' calculate different index vegetation
#'
#' @description
#' this script calculate different index from mod09a1 dataset
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster", "rgdal", "foreach", "doParallel", "stringr")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' LOAD PACKAGES
library(doParallel)
library(foreach)
library(filesstrings)

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' BUILD A REFERENCE RASTER
grd.ref <- raster(
  nrow = 4441, ncol = 3176, xmn = -81.50417,
  xmx = -68.27083, ymn = -18.4125, ymx = 0.09166667
) %>%
  "res<-"(0.004166667) %>%
  "values<-"(0)

#' FILE DATASET LINKS (INPUT)
rut.in <- "data/raster/mod09a1/withFILTER"

#' LIST OF BANDS TO CALCULATE VEGETATION INDEX
b1 <- list.files(rut.in, pattern = "refl_b01_doy", full.names = T)
b2 <- list.files(rut.in, pattern = "refl_b02_doy", full.names = T)
b3 <- list.files(rut.in, pattern = "refl_b03_doy", full.names = T)
b4 <- list.files(rut.in, pattern = "refl_b04_doy", full.names = T)
b5 <- list.files(rut.in, pattern = "refl_b05_doy", full.names = T)
b6 <- list.files(rut.in, pattern = "refl_b06_doy", full.names = T)
b7 <- list.files(rut.in, pattern = "refl_b07_doy", full.names = T)

#' DEFINE HOW MANY CLUSTER YOU WANT TO USE
use.cores <- detectCores() - 2

#' MAKE AND REGISTER CLUSTER
cluster <- makeCluster(use.cores)
registerDoParallel(cluster)

#' USE foreach() LOOP AND %dopar% COMMAND TO RUN IN PARALLEL
foreach(i = 1:length(b1)) %dopar% {
  #' load packages
  library(tidyverse)
  library(raster)
  library(rgdal)
  library(stringr)

  ndvi <- indexMODIS("ndvi",
    redBand = raster(b1[i]) / 10000,
    nirBand = raster(b2[i]) / 10000
  ) %>% resample(grd.ref) %>% "*"(10000)

  ndwi <- indexMODIS("ndwi",
    nirBand = raster(b2[i]) / 10000,
    swir3Band = raster(b7[i]) / 10000
  ) %>% resample(grd.ref) %>% "*"(10000)

  savi <- indexMODIS("savi",
    redBand = raster(b1[i]) / 10000,
    nirBand = raster(b2[i]) / 10000
  ) %>% resample(grd.ref) %>% "*"(10000)

  evi <- indexMODIS("evi",
    blueBand = raster(b3[i]) / 10000,
    redBand = raster(b1[i]) / 10000,
    nirBand = raster(b2[i]) / 10000
  ) %>% resample(grd.ref) %>% "*"(10000)

  ndii <- indexMODIS("ndii",
    nirBand = raster(b2[i]) / 10000,
    swir2Band = raster(b6[i]) / 10000
  ) %>% resample(grd.ref) %>% "*"(10000)

  gemi <- indexMODIS("gemi",
    redBand = raster(b1[i]) / 10000,
    nirBand = raster(b2[i]) / 10000
  ) %>% resample(grd.ref) %>% "*"(10000)

  gvmi <- indexMODIS("gvmi",
    nirBand = raster(b2[i]) / 10000,
    swir2Band = raster(b6[i]) / 10000
  ) %>% resample(grd.ref) %>% "*"(10000)

  date <- basename(b1[i]) %>%
    str_sub(26, 35)

  index <- c("ndvi", "ndwi", "savi", "evi", "ndii", "gemi", "gvmi")
  
  for (j in index) {
    if (j == "ndvi") {
      name <- sprintf(
        "data/raster/index/%s_%s_%s_%s.tif",
        j, "mod09a1/MOD09A1.006_sur",
        toupper(j), date
      )
    } else {
      name <- c(
        name,
        sprintf(
        "data/raster/index/%s_%s_%s_%s.tif",
        j, "mod09a1/MOD09A1.006_sur",
        toupper(j), date
        )
      )
    }
  }
  
  storage.mode(ndvi[]) = "integer"
  storage.mode(ndwi[]) = "integer"
  storage.mode(savi[]) = "integer"
  storage.mode(evi[]) = "integer"
  storage.mode(ndii[]) = "integer"
  storage.mode(gemi[]) = "integer"
  storage.mode(gvmi[]) = "integer"
  
  writeRaster(ndvi, name[1], overwrite = T)
  writeRaster(ndwi, name[2], overwrite = T)
  writeRaster(savi, name[3], overwrite = T)
  writeRaster(evi, name[4], overwrite = T)
  writeRaster(ndii, name[5], overwrite = T)
  writeRaster(gemi, name[6], overwrite = T)
  writeRaster(gvmi, name[7], overwrite = T)
}

#' END CLUSTER
stopCluster(cluster)

#' MOVE FILES
yr <- b1[1] %>%
  basename() %>%
  str_sub(29, 32)

lst.files <- list.files(rut.in, "^.*\\.tif$", full.names = T)
sapply(lst.files, FUN = file.move, sprintf("%s/%s", rut.in, yr))