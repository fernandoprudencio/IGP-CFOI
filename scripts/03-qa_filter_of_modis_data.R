#' @title
#' quality filter for mod09a1 dataset
#'
#' @description
#' this script filter mod09a1 dataset
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster", "rgdal", "foreach", "doParallel", "DescTools")

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
library(tidyverse)

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' CONSTANTS
k.year <- 2001

#' FILE DATASET LINKS (INPUT AND OUTPUT)
rut.in <- "data/raster/mod09a1/withoutFILTER/"
rut.out <- "data/raster/mod09a1/withFILTER/"

#' DESCRIPTION OF QUALITY BAND
#'   This is the order of 16 bits of the quality band
#'     (15)(14)(13)(12)(11)(10)(09)(08)(07)(06)(05)(04)(03)(02)(01)(00) - MODIS NOMENCLATURE
#'     (01)(02)(03)(04)(05)(06)(07)(08)(09)(10)(11)(12)(13)(14)(15)(16) - R NOMENCLATURE
#'
#'   Cloud State ------------------------------> bit 00-01
#'   Cloud shadow -----------------------------> bit 02
#'   Land/Water Flag --------------------------> bit 03-05
#'   Aerosol Quantity -------------------------> bit 06-07
#'   Cirrus detected --------------------------> bit 08-09
#'   Internal Cloud Algorithm Flag ------------> bit 10
#'   Internal Fire Algorithm ------------------> bit 11
#'   MOD35 snow/ice flag ----------------------> bit 12
#'   Pixel adjacent to cloud ------------------> bit 13
#'   BRDF correction performed ----------------> bit 14
#'   Internal Snow Mask -----------------------> bit 15
filter <- list(
  NA, "1", c("000", "010", "011", "100", "101", "110", "111"),
  c("11"), c("11"), "1", NA, "1", NA, "1", "1"
)

#' LIST OF QA DATASET
qa.list <- list.files(rut.in,
  pattern = sprintf("state_500m_doy%s", k.year),
  full.names = T
)

#' DEFINE HOW MANY CLUSTER YOU WANT TO USE
use.cores <- detectCores() - 2

#' FILTER MOD091A1
for (j in 1:7) {
  #' list reflectivity datset
  band.list <- list.files(rut.in,
    pattern = sprintf("refl_b0%s_doy%s", j, k.year),
    full.names = T
  )

  #' make and register cluster
  cluster <- makeCluster(use.cores)
  registerDoParallel(cluster)

  #' use foreach() loop and %dopar% command to run in parallel
  foreach(i = 1:length(qa.list)) %dopar% {
    #' load packages
    library(DescTools)
    library(dplyr)
    library(raster)
    library(rgdal)

    file <- qaFilter(
      raster(band.list[i]),
      raster(qa.list[i]),
      "mxd09a1", filter
    )

    name <- band.list[i] %>%
      strsplit("/") %>%
      sapply("[", 5) %>%
      substr(1, 35) %>%
      sprintf(fmt = "%s_qafilter.tif")

    writeRaster(file, sprintf("%s%s", rut.out, name), overwrite = T)
  }

  #' end cluster
  stopCluster(cluster)
}