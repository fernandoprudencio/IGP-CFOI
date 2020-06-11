#' @title
#' Seasonal behavior of IV VS cumulative fire log frequency
#'
#' @description
#' This script plots the seasonal behavior of vegetation index
#'   (normal years and dry years) vs cumulative fire log frequency by
#'   cluster region
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "sf", "tidyverse", "raster", "doParallel", "foreach", "ggplot2", "mapview",
  "xts"
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
library(raster)
library(tidyverse)
library(ggplot2)
# library(doParallel)
# library(foreach)
# library(mapview)
# library(xts)
# library(ggpubr)

#' CHANGE TO ENGLISH LANGUAGE
Sys.setlocale(category = "LC_ALL", locale = "english")

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' LOAD .RData FILES
#load("data/rdata/gvmi_avr_vls_reg6.RData")

#' CONSTANTS
k.elev <- c(1500, 4000)
k.regions <- c(8)
k.omit.yrs <- c(2019, 2020)
k.dry.yrs <- c(2005, 2010, 2016)

#' CREATE DATE VECTOR
curr.date <- Sys.Date()
year <- str_sub(curr.date, 1, 4) %>% as.numeric()
month <- str_sub(curr.date, 6, 7) %>% as.numeric()
day <- str_sub(curr.date, 9, 10) %>% as.numeric()

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
#'   read cluster region
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

#'   read fires log
lyrs.frs <- rgdal::ogrListLayers(
  "data/vector/fire_register.gpkg"
)

sf.fires <- st_read(
  dsn = "data/vector/fire_register.gpkg",
  layer = lyrs.frs[3], quiet = T, as_tibble = T
) %>%
  dplyr::filter(!ANO %in% c(k.omit.yrs)) %>%
  st_intersection(sf.region)

#' LOAD RASTER DATA
#'   load IV files list
lst.iv <- list.files(
  "data/raster/index/gvmi_mod09a1/",
  pattern = ".tif", full.names = T
)

#'   load and resample DEM
grd.dem <- raster("data/raster/dem/dem.tif") %>%
  resample(raster(lst.iv[1])) %>%
  crop(sf.region) %>%
  mask(sf.region)

#' SELECT ELEVATION RANGE TO BE CONSIDERED IN THE ANALYSIS
grd.dem[grd.dem < k.elev[1] | grd.dem > k.elev[2]] <- NA
grd.dem[!is.na(grd.dem)] <- 1

#' EXTRACT VALUES FROM "IV" DATA
index.avr.vle <-
  lapply(
    lst.iv,
    function(x) {
      return((raster(x) * grd.dem) %>% getValues() %>% mean(na.rm = T))
    }
  ) %>%
  as.numeric()

save(
  index.avr.vle,
  file = sprintf("data/rdata/gvmi_avr_vls_reg%s_1500-4000.RData", k.regions)
)

#' CALCULATE AVERAGE SEASONAL BEHAVIOR FOR NORMAL YEARS AND DRY YEARS EVERY
#'   8 DAYS
df.iv <- tibble(
  date = ts[1:length(index.avr.vle)],
  value = index.avr.vle,
) %>%
  mutate(
    year = sprintf("yr.%s", str_sub(date, 1, 4)),
    oct.day = str_sub(date, 6, 10)
  ) %>%
  dplyr::select(-date) %>%
  spread(year, value) %>%
  rowwise() %>%
  mutate(
    dry.iv = mean(c(yr.2005, yr.2010, yr.2016), na.rm = T),
    norm.iv = mean(
      c(
        yr.2000, yr.2001, yr.2002, yr.2003, yr.2004, yr.2006, yr.2007, yr.2008,
        yr.2009, yr.2011, yr.2012, yr.2013, yr.2014, yr.2015, yr.2017, yr.2018,
        yr.2019
      ),
      na.rm = T
    )
  ) %>%
  ungroup() %>%
  dplyr::select(oct.day, dry.iv, norm.iv) %>%
  mutate(
    oct.day = sprintf("2020-%s", oct.day) %>% as.Date()
  )

#' CALCULATE AVERAGE CUMULATIVE FREQUENCY OF FIRES LOG FOR NORMAL YEARS AND
#'   DRY YEARS EVERY 8 DAYS

#'   calculate frequency of fires log per day
df.fires.log <- sf.fires %>%
  mutate(
    date = sprintf("%s-%s-%s", ANO, MES, DIA) %>% as.Date()
  ) %>%
  dplyr::select(date) %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(
    nfires = length(date) %>% as.numeric()
  ) %>%
  as_tibble() %>%
  dplyr::select(-geom)

#'   calculate frequency of fires log every 8 days
for (i in 2002:2018) {
  #' normal-year
  if (i %% 4 != 0) {
    df.day.fire <- df.fires.log %>%
      right_join(
        tibble(
          date = seq(
            sprintf("%s-01-01", i) %>% as.Date(),
            sprintf("%s-01-03", i + 1) %>% as.Date(),
            by = "1 day"
          )
        ),
        by = "date"
      ) %>%
      mutate(nfires = if_else(is.na(nfires), 0, nfires)) %>%
      as.data.frame()

    df.oct.day.fire <-
      #' create time serie XTS
      xts(df.day.fire[, 2], order.by = df.day.fire[, 1]) %>%
      period.sum(seq(8, 361, 8)) %>%
      as_tibble() %>%
      rename(nfires = V1) %>%
      mutate(
        date = ts[str_sub(ts, 1, 4) %in% c(i, i + 1)][1:length(nfires)]
      )
    
    if (i == 2002) {
      df.fires.tmp <- df.oct.day.fire
    } else {
      df.fires.tmp <- rbind(df.fires.tmp, df.oct.day.fire)
    }
    #' leap-year
  } else {
    df.day.fire <- df.fires.log %>%
      right_join(
        tibble(
          date = seq(
            sprintf("%s-01-01", i) %>% as.Date(),
            sprintf("%s-01-02", i + 1) %>% as.Date(),
            by = "1 day"
          )
        ),
        by = "date"
      ) %>%
      mutate(nfires = if_else(is.na(nfires), 0, nfires)) %>%
      as.data.frame()

    df.oct.day.fire <-
      #' create time serie XTS
      xts(df.day.fire[, 2], order.by = df.day.fire[, 1]) %>%
      period.sum(seq(8, 361, 8)) %>%
      as_tibble() %>%
      rename(nfires = V1) %>%
      mutate(
        date = ts[str_sub(ts, 1, 4) == i]
      )

    df.fires.tmp <- rbind(df.fires.tmp, df.oct.day.fire)
  }
}

#'   calculate cumulative frequency of fires log every 8 days
df.fires <-
  tibble(
    oct.day = df.iv$oct.day,

    dry.fires =
      df.fires.tmp %>%
        filter(substr(date, 1, 4) %in% k.dry.yrs) %>%
        group_by(
          substr(date, 6, 10)
        ) %>%
        summarise(nfires = mean(nfires, na.rm = T)) %>%
        mutate(acum.freq = cumsum(nfires)) %>%
        dplyr::select(acum.freq) %>%
        pull(acum.freq),

    norm.fires =
      df.fires.tmp %>%
        filter(
          !(substr(date, 1, 4) %in% k.dry.yrs)
        ) %>%
        group_by(
          substr(date, 6, 10)
        ) %>%
        summarise(nfires = mean(nfires, na.rm = T)) %>%
        mutate(acum.freq = cumsum(nfires)) %>%
        dplyr::select(acum.freq) %>%
        pull(acum.freq)
  )

#' JOIN TABLES OF CUMULATIVE FIRES LOG AND IV FOR NORMAL YEARS AND DRY YEARS
#'   EVERY 8 DAYS 
df <- df.iv %>%
  left_join(df.fires, by = "oct.day")

#' HOMOGENIZE THE PRIMARY AND SECONDARY AXIS
ylim.prim <- c(
  #min(df$norm.iv, df$dry.iv),
  #max(df$norm.iv, df$dry.iv)
  0, .35
)

ylim.sec <- c(
  #min(df$norm.fires, df$dry.fires),
  #max(df$norm.fires, df$dry.fires)
  0, 350
)

#'   conversion coefficient
n <- diff(ylim.prim) / diff(ylim.sec)
m <- ylim.prim[1] - n * (ylim.sec[1])


plt <- ggplot(df, aes(oct.day, norm.iv)) +
  geom_line(
    color = rgb(0, 128, 0, maxColorValue = 255),
    size = .8,
  ) +
  geom_line(
    aes(oct.day, dry.iv),
    color = rgb(0, 128, 0, maxColorValue = 255),
    size = .8,
    linetype = "dotdash"
  ) +
  geom_line(
    aes(y = m + norm.fires * n), size = .8,
    colour = rgb(237, 28, 36, maxColorValue = 255)
  ) +
  geom_line(
    aes(y = m + dry.fires * n), size = .8,
    colour = rgb(237, 28, 36, maxColorValue = 255),
    linetype = "dotdash"
  ) +
#  geom_point(
#    aes(oct.day, norm.iv),
#    colour = rgb(0, 128, 0, maxColorValue = 255),
#    size = 2
#  ) +
#  geom_point(
#    aes(oct.day, dry.iv),
#    colour = rgb(0, 128, 0, maxColorValue = 255),
#    shape = 1, size = 1.5
#  ) +
#  geom_point(
#    aes(y = m + norm.fires * n),
#    colour = rgb(237, 28, 36, maxColorValue = 255),
#    shape = 1, size = 2
#  ) +
#  geom_point(
#    aes(y = m + dry.fires * n),
#    colour = rgb(237, 28, 36, maxColorValue = 255),
#    size = 2
#  ) +
  annotation_ticks(
    sides = "l",
    ticklength = 1.5 * unit(0.1, "cm"),
    color = rgb(0, 128, 0, maxColorValue = 255)
  ) +
  annotation_ticks(
    sides = "r",
    ticklength = 1.5 * unit(0.1, "cm"),
    color = rgb(237, 28, 36, maxColorValue = 255)
  ) +
  annotation_ticks(
    sides = "b",
    ticklength = 1 * unit(0.1, "cm")
  ) +
  annotation_ticks(
    sides = "t",
    ticklength = 1 * unit(0.1, "cm")
  ) +
  coord_cartesian(clip = "off") +
  scale_x_date(
    breaks = "1 month",
    date_labels = "%b",
    expand = c(.012, .012)
  ) + #
  scale_y_continuous(
    breaks = seq(0, .35, .05),
    limits = c(0, .36),
    sec.axis = sec_axis(~ (. - m) / n,
      name = "cumulative fire frequency\n",
      breaks = seq(0, 360, 50),
    ),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size = 12, colour = "black", hjust = -.15, face = "bold"
    ),
    axis.text.y.left = element_text(
      size = 13, face = "bold",
      colour = rgb(0, 128, 0, maxColorValue = 255)
    ),
    axis.text.y.right = element_text(
      size = 13, face = "bold",
      colour = rgb(237, 28, 36, maxColorValue = 255)
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y.left = element_line(
      color = rgb(0, 128, 0, maxColorValue = 255)
    ),
    axis.ticks.y.right = element_line(
      color = rgb(237, 28, 36, maxColorValue = 255)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    axis.line.y.left = element_line(
      size = .8, color = rgb(0, 128, 0, maxColorValue = 255)
    ),
    axis.line.y.right = element_line(
      size = .8, color = rgb(237, 28, 36, maxColorValue = 255)
    )
  )

title.axis.left <- textGrob(
  label = "GVMI", check.overlap = F,
  x = unit(0, "lines"),
  y = unit(-1.7, "lines"),
  hjust = -.6,
  gp = gpar(
    fontsize = 15,
    fontface = "bold",
    col = rgb(0, 128, 0, maxColorValue = 255)
  )
)

title.axis.right <- textGrob(
  label = "CFFF", check.overlap = F,
  x = unit(0, "lines"),
  y = unit(-.8, "lines"),
  hjust = -10.3,
  gp = gpar(
    fontsize = 15,
    fontface = "bold",
    col = rgb(237, 28, 36, maxColorValue = 255)
  )
)

plt.ge <- gridExtra::arrangeGrob(
  plt, top = title.axis.right
) %>%
  gridExtra::arrangeGrob(top = title.axis.left)

ggsave(
  plot = plt.ge,
  sprintf("exports/gvmi_vs_fires_reg%s.png", k.regions),
  width = 16, height = 12, units = "cm", dpi = 1000
)
