#' @title
#' IV seasonal behavior by elevation interval and cluster region
#'
#' @description
#' this script plots the seasonal behavior of vegetation index
#'   (remote sensing) by elevation interval from homogeneous regions (clusters)
#'
#' @author Fernando Prudencio
#'
#' @data
#' 'k.regions', number (location) of cluster region
#' 'region', homogeneous regions built from a cluster analysis
#' 'GVMI', vegetation index calculated from MOD09A1 data
#' 'dem', SRTM digital elevation model resampled to 10km

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "sf", "tidyverse", "ncdf4", "raster", "lattice", "stringr",
  "arules", "reshape2", "RColorBrewer", "magrittr", "latticeExtra", "grid",
  "cptcity"
)

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
library(arules)
library(stringr)
library(ncdf4)
library(raster)
library(RColorBrewer)
library(reshape2)
library(magrittr)
library(lattice)
library(latticeExtra)
library(grid)
library(cptcity)

#' CHANGE TO ENGLISH LANGUAGE
Sys.setlocale(category = "LC_ALL", locale = "english")

#' DEFINE CONSTANTS
k.regions <- c(8)
k.type <- "dry"

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

#' LOAD VECTORIAL DATA
lyrs.cls <- rgdal::ogrListLayers(
  "data/vector/cluster_region.gpkg"
)

sf.region <- st_read(
  dsn = "data/vector/cluster_region.gpkg",
  layer = lyrs.cls[1], quiet = T, as_tibble = T
) %>%
  group_by(gridcode) %>%
  summarise(nfeature = length(gridcode)) %>%
  dplyr::filter(gridcode %in% k.regions)

#' LOAD RASTER DATA
list.iv <- list.files(
  sprintf("data/raster/index/gvmi_mod09a1/climatology/%s/", k.type),
  pattern = ".nc", full.names = T
)

dem <- raster("data/raster/dem/SRTM_500m_resampled_to_MOD09A1.nc")

#'   stack bands
stack.data <- stack(list.iv, dem)

#'   change names of bands
names(stack.data) <- c(sprintf("band%0.2d", 1:46), "elev")

#' BUILD DATAFRAME TO PLOT HEATMAP
df.val <- stack.data %>%
  crop(sf.region) %>%
  mask(sf.region) %>%
  getValues() %>%
  as_tibble() %>%
  drop_na()

#'   edit dataframe
df <- df.val %>%
  mutate(category = cut(elev, breaks = c(seq(0, 4800, 200), 6000))) %>%
  group_by(category) %>%
  summarise_each(funs(mean)) %>%
  dplyr::filter(elev >= 1400) %>%
  mutate(
    inter = c(
      sprintf(
        "( %s - %s ]",
        seq(1400, 4600, 200),
        seq(1600, 4800, 200)
      ),
      "( 4800 - 5600 ]"
    )
  ) %>%
  dplyr::select(inter, sprintf("band%0.2d", 1:46)) %>%
  gather(key = "variable", value = "value", -inter) %>%
  mutate(inter = as.factor(inter)) %>%
  as_tibble() %>%
  rename(mes = variable, iv = value) %>%
  mutate(mes = as.factor(mes))

#' DEFINE THE COLOR PALETTE
#   hm.palette <- colorRampPalette(brewer.pal(9, "YlGnBu"), space = "Lab")
#   http://soliton.vm.bytemark.co.uk/pub/cpt-city/cmocean/

#' X LABELS OF PLOT
iv.days <- str_sub(ts, 6, 10)[40:85]

for (j in 1:12) {
  lbl <- iv.days[str_sub(iv.days, 1, 2) == sprintf("%.02d", j)] %>%
    str_sub(4, 5)

  iv.days[str_sub(iv.days, 1, 2) == sprintf("%.02d", j)] <-
    sprintf("%s-%s", month.abb[j], lbl)
}

#' SAVE PLOT
png(
  sprintf(
    "exports/iv_by_elevation_heatmap_8day_clim_cluster%s_%s.png",
    k.regions, k.type
  ),
  width = 35, height = 15, units = "cm", res = 1000
)

#' PLOT HEATMAP
levelplot(iv ~ mes * inter,
  data = df,
  at = seq(-.04, .34, .02),
  margin = c(1, 1),
  xlab = NULL,
  ylab = NULL,
  col.regions = cpt(
    pal = "cmocean_curl", n = 100, colorRampPalette = FALSE,
    rev = T
  ),
  colorkey = list(
    at = seq(-.04, .34, .02),
    space = "top", # location of legend
    labels = list(at = seq(-.04, .34, .04)), font = 1
    # height = .95, width = 1.4,
  ),
  # aspect = "iso",
  scale = list(
    x = list(rot = 45, cex = 1, fontface = "bold", label = iv.days),
    y = list(cex = 1.2, font = 1)
  ),
  contour = T, # show contour of data interpolated
  labels = list(cex = 1.1, col = "black"),
  label.style = "align",
  lwd = 1,
  region = T,
  # border = "black",
  par.settings = list(
    panel.background = list(col = "white"),
    axis.line = list(lwd = 1.2)
  )
)

grid::grid.text(
  "GVMI",
  y = unit(.915, "npc"),
  rot = 0, x = unit(.085, "npc"),
  gp = gpar(
    fontsize = 22,
    fontface = "bold",
    col = rgb(104, 22, 89, maxColorValue = 255)
    #col = rgb(21, 29, 68, maxColorValue = 255)
    #col = rgb(52, 13, 53, maxColorValue = 255)
  )
)

grid::grid.text(
  "elevation",
  y = unit(.5, "npc"),
  rot = 270, x = unit(.99, "npc"),
  gp = gpar(fontsize = 20, fontface = "bold")
)

#' CLOSE THE SAVED OF PLOT
dev.off()