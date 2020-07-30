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
pkg <- c("tidyverse", "raster", "rgdal", "foreach", "doParallel")

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

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' CONSTANTS
k.year <- 2020

#' LOAD REFERENCE RASTER
ref <- raster(
  "data/raster/index/gvmi_mod09a1/MOD09A1.006_sur_GVMI_doy2002033.tif"
)

#' FILE DATASET LINKS (INPUT)
rut.in <- "data/raster/mod09a1/withFILTER/"

b2 <- list.files(rut.in,
  pattern = sprintf("refl_b02_doy%s", k.year),
  full.names = T
)

b6 <- list.files(rut.in,
  pattern = sprintf("refl_b06_doy%s", k.year),
  full.names = T
)

#' DEFINE HOW MANY CLUSTER YOU WANT TO USE
use.cores <- detectCores() - 2

#' MAKE AND REGISTER CLUSTER
cluster <- makeCluster(use.cores)
registerDoParallel(cluster)

#' USE foreach() LOOP AND %dopar% COMMAND TO RUN IN PARALLEL
foreach(i = 1:length(b2)) %dopar% {
  #' load packages
  library(tidyverse)
  library(raster)
  library(rgdal)

  index <- "gvmi"
  file <- indexMODIS(index,
    nirBand = raster(b2[i]) / 10000,
    swir2Band = raster(b6[i]) / 10000
  ) %>% resample(ref)

  date <- b2[i] %>%
    strsplit("/") %>%
    sapply("[", 5) %>%
    substr(26, 35)

  name <- sprintf("data/raster/index/%s%s%s%s%s%s",
    index,
    "_mod09a1/MOD09A1.006_sur_",
    toupper(index),
    "_", date, ".tif"
  )

  writeRaster(file, name, overwrite = T)
}

#' END CLUSTER
stopCluster(cluster)