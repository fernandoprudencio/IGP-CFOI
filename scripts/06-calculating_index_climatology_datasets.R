#' @title
#' vegetation index climatology
#'
#' @description
#' this script calculates the climatology of vegetation index from MOD09A1 data
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("sf", "tidyverse", "raster", "stringr")

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
library(sf)
library(tidyverse)
library(raster)
library(stringr)

#' DEFINE CONSTANTS
k.omit.yrs <- c(2000, 2001, 2005, 2010, 2016, 2020)
k.dry.yrs <- c(2005, 2010, 2016)
k.yrs <- k.omit.yrs
k.type <-
  if (identical(k.yrs, k.omit.yrs)) {
    k.type <- "norm"
  } else {
    k.type <- "dry"
  }

#' CREATE DATE VECTOR
date <- Sys.Date()
year <- str_sub(date, 1, 4) %>% as.numeric()
month <- str_sub(date, 6, 7) %>% as.numeric()
day <- str_sub(date, 9, 10) %>% as.numeric()

for (i in 2000:year) {
  if (i == 2000) {
    ts <- c(
      as.Date("2000-02-26"),
      seq(as.Date("2000-03-06"), as.Date("2000-12-31"), by = "8 day")
    )
  }
  if (i >= 2001 & i <= year - 1) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
        as.Date(sprintf("%s-02-26", i)),
        by = "8 day"
      ),
      seq(as.Date(sprintf("%s-03-06", i)),
        as.Date(sprintf("%s-12-31", i)),
        by = "8 day"
      )
    )
  }
  if (i == year & as.numeric(month) <= 2) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
        as.Date(sprintf("%s-%s-%s", i, month, day)),
        by = "8 day"
      )
    )
  }
  if (i == year & as.numeric(month) > 2) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
        as.Date(sprintf("%s-02-26", i)),
        by = "8 day"
      ),
      seq(as.Date(sprintf("%s-03-06", i)),
        as.Date(sprintf("%s-%s-%s", i, month, day)),
        by = "8 day"
      )
    )
  }
}

#' LOAD RASTER DATA
index.list <-
  list.files(
    "data/raster/index/gvmi_mod09a1/",
    pattern = ".tif",
    full.names = T
  )

#' CALULATE IV CLIMATOLOGY
for (i in 1:46) {
  #month <- sprintf("%.02d", 1:12)
  month <- str_sub(ts, 6, 10)[40:85]

  if (identical(k.yrs, k.omit.yrs)) {
    df <- tibble(
      date = ts[1:length(index.list)],
      id = 1:length(date)
    ) %>%
      filter(
        !(str_sub(date, 1, 4) %in% k.yrs) & substr(date, 6, 10) == month[i]
      )
  } else {
    df <- tibble(
      date = ts[1:length(index.list)],
      id = 1:length(date)
    ) %>%
      filter(
        (str_sub(date, 1, 4) %in% k.yrs) & substr(date, 6, 10) == month[i]
      )
  }

  index <- stack(index.list[df$id]) %>% mean(na.rm = T)
  index[index < -1] <- NA
  index[index > 1] <- NA

  if (i == 1) {
    stck <- index
    names(stck)[i] <- sprintf("band%s", i)#month.abb[i]
  } else {
    stck <- stack(stck, index)
    names(stck)[i] <- sprintf("band%s", i)#month.abb[i]
  }
}

#' SAVE IV CLIMATOLOGY
name <- index.list[1] %>%
  str_split("/") %>%
  sapply("[", 5) %>%
  str_sub(1, 20)

for (k in 1:46) {
  writeRaster(stck[[k]],
    sprintf(
      "data/raster/index/gvmi_mod09a1/climatology/%s/%s_%s_8day_clim_%s.nc",
      k.type, name, k.type, month[k]
    ),
    overwrite = T
  )
}