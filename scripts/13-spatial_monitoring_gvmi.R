#' @title
#' Plot average temperature map of july, august, september, ovtober and november
#'
#' @description
#' this script plot average temperature map of july, august, september, october
#'   and november from climatology of average and maximum monthly data
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "ncdf4", "sf", "lattice", "extrafont",
  "cptcity", "latticeExtra", "rasterVis", "maptools", "grid"
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

#' LOAD PACKAGE
library(tidyverse)
library(raster)
library(ncdf4)
library(sf)
library(lattice)
library(extrafont)
library(cptcity)
library(latticeExtra)
library(rasterVis)
library(maptools)
library(grid)

#' CONSTANTS
k.dep <- c(
  "Piura", "Cajamarca", "La Libertad", "Ancash", "Loreto", "Huancavelica",
  "Amazonas", "Madre de Dios", "Cusco", "Apurimac", "Puno", "Huanuco", "Pasco",
  "Junin"
)

#' LOAD RASTER DATA
img <- raster("data/raster/index/anom_gvmi_junio_2020.tif")

#' DEFINE COLOR PALETTE
cb.palette <-
  c(
    "#005700", "#006300", "#006F00", "#007B00", "#008700", "#009300", "#009F00",
    "#00AB00", "#00B700", "#00C300", "#00CF00", "#00DB00", "#00E700", "#00F300",
    "#00FF00", "#D1D1D1", "#D1D1D1", "#FFFF00", "#FFEB00", "#FFD600", "#FFC200",
    "#FFAD00", "#FF9900", "#FF8500", "#F3740A", "#E76313", "#DB531D", "#CF4227",
    "#C33230", "#B7213A", "#AB1143", "#9F004D"
  )

#' LOAD VECTOR DATA TO PLOT WHIT RASTER OF TEMPERATURE
#'   load world countries limits
#'     load sf data
sf.world <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "world_countries", quiet = T, as_tibble = T
)
#'     load sp data
sp.world <- as(st_geometry(sf.world), Class = "Spatial")

#'   load Peru departaments points
#'     load sf data
sf.peru <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "peru_departaments", quiet = T, as_tibble = T
) %>%
  st_centroid(of_largest_polygon = FALSE) %>%
  filter(Departamen %in% k.dep) %>%
  mutate(Departamen = as.character(Departamen))
#'     load sp data
sp.peru <- as(st_geometry(sf.peru), Class = "Spatial")


#' SAVE PLOT
png(
  "exports/gvmi_anom_junio_2020.png",
  width = 20, height = 28, units = "cm", res = 1000
)

levelplot(img %>% mask(sf.world %>% filter(COUNTRY == "Peru")),
  main = list(
    "Anomalías de GVMI - Junio 2020", cex = 2,
    side = 1, line = .5, fontfamily = "Source Sans Pro"
  ),
  scales = list(
    x = list(limits = c(-81.5, -68.4)),
    y = list(limits = c(-18.5, .09))
  ),
  col.regions = rev(cb.palette),
  margin = F,
  pretty = T,
  maxpixels = 15e6,
  at = seq(-.4, .4, .025),
  colorkey = list(
    at = seq(-.4, .4, .025),
    space = "right", # location of legend
    labels = list(at = seq(-.4, .4, .1), cex = 1.1),
    font = list(family = "Source Sans Pro")
  ),
  xlab = NULL,
  ylab = NULL,
  par.settings = list(
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    par.xlab.text = list(fontfamily = "Source Sans Pro"),
    par.ylab.text = list(fontfamily = "Source Sans Pro"),
    par.main.text = list(fontfamily = "Source Sans Pro"),
    par.sub.text = list(fontfamily = "Source Sans Pro")
  )
) +
  latticeExtra::layer(
    sp.lines(sp.world, col = "black", lwd = 2),
    # sp.lines(sp.dep, col = "black", lwd = .8),
    sp.points(sp.peru, pch = 20, cex = 1, col = "black"),
    sp.text(
      coordinates(sp.peru), txt = sf.peru$Departamen, pos = 1, cex = 1.2,
      fontfamily = "Source Sans Pro")
  )

#' CLOSE THE SAVED OF PLOT
dev.off()