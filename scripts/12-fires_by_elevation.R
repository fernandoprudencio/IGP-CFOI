#' @title
#'
#' @description
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("sf", "tidyverse", "raster", "ggplot2", "grid")

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
library(raster)
library(tidyverse)
library(ggplot2)
library(grid)

#' CHANGE TO ENGLISH LANGUAGE
Sys.setlocale(category = "LC_ALL", locale = "english")

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' DEFINE CONSTANTS
k.omit.yrs <- c(2000, 2001, 2019)

#' LOAD VECTOIAL DATA
lyrs.frs <- rgdal::ogrListLayers(
  "data/vector/fire_register.gpkg"
)

sf.fires <-
  st_read("data/vector/fire_register.gpkg",
    layer = lyrs.frs[3], quiet = T, as_tibble = T
  ) %>%
  dplyr::filter(!ANO %in% c(k.omit.yrs)) %>%
  rename(month.num = MES)

#' LOAD RASTER DATA
dem <- raster("data/raster/dem/dem.tif")

#' ploting monthly fires distribution by elevation intervals in a boxplot
df <- sf.fires %>%
  mutate(elev = raster::extract(dem, sf.fires)) %>%
  left_join(
    tibble(
      month.num = c(1:12),
      month.txt = sprintf("%s-%s", letters[1:12], month.abb)
    ),
    by = "month.num"
  ) %>%
  dplyr::select(month.txt, elev) %>%
  arrange(month.txt) %>%
  drop_na()

plt <- ggplot(df, mapping = aes(month.txt, elev)) +
  geom_boxplot(
    alpha = .1, width = .5, #outlier.size = NULL
    fatten = 2, lwd = .6,
    fill = rgb(0, 128, 0, maxColorValue = 255),
    color = rgb(0, 128, 0, maxColorValue = 255)
  ) +
#  geom_boxplot(
#    alpha = 0, outlier.alpha = 1, width = 0.5,
#    color = rgb(237, 28, 36, maxColorValue = 255),
#    
#    outlier.shape = NA, fatten = 1.5, lwd = .8
#  ) +
  geom_jitter(
    shape = 16, size = .1,
    color = rgb(237, 28, 36, maxColorValue = 255),
    position = position_jitter(0.2)
  ) +
  stat_summary(
    fun.y = "mean",
    geom = "point",
    shape = 3, size = 4,
    colour = rgb(0, 128, 0, maxColorValue = 255)
  ) +
  theme_bw() +
  scale_x_discrete(
    label = substr(month.name, 1, 3),
    expand = c(.07, .07)
  ) +
  scale_y_continuous(
    breaks = seq(0, 5250, 500),
    limits = c(0, 5250), expand = c(0, 0)
  ) +
  annotation_ticks(
    sides = "l",
    ticklength = 1 * unit(.1, "cm"),
    color = rgb(0, 128, 0, maxColorValue = 255)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(1, .5, 1, 1, "cm"),
    axis.title = element_blank(),
    axis.text.x = element_text(
      size = 10, colour = "black", face = "bold", angle = 45, vjust = .7
    ),
    axis.text.y = element_text(
      size = 11, face = "bold",
      color = rgb(0, 128, 0, maxColorValue = 255)
    ),
    axis.ticks.y = element_line(color = rgb(0, 128, 0, maxColorValue = 255)),
    axis.ticks.length.x = unit(.12, "cm"),
    axis.ticks.length.y = unit(.2, "cm"),
    axis.line.y = element_line(
      size = .8, color = rgb(0, 128, 0, maxColorValue = 255)
    ),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = .5)
  )

title.axis <- textGrob(
  label = "elev", check.overlap = F,
  x = unit(0, "lines"),
  y = unit(-.9, "lines"),
  hjust = -.6,
  gp = gpar(
    fontsize = 18,
    fontface = "bold",
    col = rgb(0, 128, 0, maxColorValue = 255)
  )
)

p <- gridExtra::arrangeGrob(plt, top = title.axis)
grid.draw(p)

ggsave(p,
  filename = "Fires_distribution_by_elevation.png",
  width = 10, height = 15, units = "cm", dpi = 1000
)