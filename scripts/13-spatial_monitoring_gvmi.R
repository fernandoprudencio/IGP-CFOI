#' @title
#' Plot the spatial distribution of GVMI an GVMI anomalies
#'
#' @description
#' this script plots, as monitoring, the spatial distribution of GMVI and GVMI
#'   anomalies for each month of the current year
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
      install.packages(x, dependencies = T)
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
library(RCurl)
library(magick)
library(stringr)

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' CONSTANTS
k.data <- file.choose()

k.dep <- c(
  "Piura", "Cajamarca", "La Libertad", "Ancash", "Loreto", "Huancavelica",
  "Amazonas", "Madre de Dios", "Cusco", "Apurimac", "Puno", "Huanuco", "Pasco",
  "Junin"
)

k.yr <- as.character(readline(prompt = "\nEnter year: \n"))
k.mth <- as.numeric(readline(prompt = "\nEnter month: \n"))

#' DEFINE COLOR PALETTE
if (grepl(k.data, pattern = "anom")) {
  cb.palette <-
    c(
      "#005700", "#006300", "#006F00", "#007B00", "#008700", "#009300", "#009F00",
      "#00AB00", "#00B700", "#00C300", "#00CF00", "#00DB00", "#00E700", "#00F300",
      "#00FF00", "#D1D1D1", "#D1D1D1", "#FFFF00", "#FFEB00", "#FFD600", "#FFC200",
      "#FFAD00", "#FF9900", "#FF8500", "#F3740A", "#E76313", "#DB531D", "#CF4227",
      "#C33230", "#B7213A", "#AB1143", "#9F004D"
    )
} else {
  cb.palette <-
    c(
      "#0000E1", "#0055EB", "#00AAF5", "#00FFFF", "#1BA704", "#14BD03", "#0ED302",
      "#07E901", "#00FF00", "#40FF00", "#80FF00", "#BFFF00", "#FFFF00", "#F7D400",
      "#EFA900", "#E87F00", "#E05400", "#D82900", "#B92E00", "#993300", "#737373"
    )
}

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
  st_centroid(of_largest_polygon = T) %>%
  filter(Departamen %in% k.dep) %>%
  mutate(Departamen = as.character(Departamen))
#'     load sp data
sp.peru <- as(st_geometry(sf.peru), Class = "Spatial")


#' READ RASTER DATA
index <- raster(k.data) %>%
  mask(sf.world %>% filter(COUNTRY == "Peru"))

#' PLOT
if (grepl(k.data, pattern = "anom_gvmi")) {
  name <- "exports/anom_gvmi.png"
  title <- sprintf(
    "Anomalías de GVMI - %1$s %2$s",
    translate.date(month.name[k.mth]) %>% tolower(),
    k.yr
  )
  intrv <- seq(-.4, .4, .025)
  intrv.lbl <- seq(-.4, .4, .1)
} else {
  name <- "exports/gvmi.png"
  title <- sprintf(
    "Condiciones de GVMI - %1$s %2$s",
    translate.date(month.name[k.mth]) %>% tolower(),
    k.yr
  )
  intrv <- c(-.1, seq(0, .5, .025))
  intrv.lbl <- c(-.1, seq(0, .5, .1))
}

png(name, width = 20, height = 28, units = "cm", res = 500)

levelplot(index,
  main = list(
    title,
    cex = 2, side = 1, line = .5, fontfamily = "Source Sans Pro"
  ),
  scales = list(
    x = list(limits = c(-81.8, -68.2)),
    y = list(limits = c(-18.7, .4))
  ),
  col.regions = rev(cb.palette),
  margin = F,
  pretty = T,
  maxpixels = 15e6,
  at = intrv,
  colorkey = list(
    at = intrv,
    space = "right", # location of legend
    labels = list(at = intrv.lbl, cex = 1.1),
    font = list(family = "Source Sans Pro")
  ),
  xlab = NULL,
  ylab = NULL,
  aspect.fill = T,
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
    # sp.lines(sp.dep, col = "black", lwd = .8)
    sp.points(sp.peru, pch = 20, cex = 1, col = "black"),
    sp.text(
      coordinates(sp.peru),
      txt = sf.peru$Departamen, pos = 1, cex = 1.2,
      fontfamily = "Source Sans Pro"
    )
  )

#' CLOSE THE SAVED OF PLOT
dev.off()

#' TRIM FIGURE
img <- magick::image_read(name) %>% image_trim()

#' SAVE FIGURE
image_write(img, path = name, format = "png")

#' UPLOAD TO LAMAR SERVER
path <- "/data/users/lamar/DATA/FIG_WEB_LAMAR/PROJECTS/FIRE_ANDES/GVMI"
host <- "181.177.244.92"
nam <- basename(name) %>% str_sub(1, -5)

ftpUpload(
  sprintf("exports/%s.png", nam),
  sprintf("ftp://%1$s/%2$s/%3$s.png", host, path, nam),
  userpwd = "amazonia:l@mar1044"
)

ftpUpload(
  sprintf("exports/%s.png", nam),
  sprintf(
    "ftp://%1$s/%2$s/%4$s/%3$s/%4$s%5$02d_%3$s.png",
    host, path, nam, k.yr, k.mth
  ),
  userpwd = "amazonia:l@mar1044"
)