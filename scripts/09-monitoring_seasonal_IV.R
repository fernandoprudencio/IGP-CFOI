#' @title
#' Seasonal behavior plot of vegetation index
#'
#' @description
#' This script plots the seasonal behavior of vegetation index between 2000 and
#'   2020, highlighting dry years (2005, 2010 and 2016) and current year
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "sf", "ggplot2", "raster", "velox", "ncdf4", "stringr", "grid",
  "ggthemes", "scales", "gridExtra"
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
library(tidyverse)
library(sf)
library(ggplot2)
library(raster)
library(velox)
library(ncdf4)
library(stringr)
library(grid)
library(ggthemes)
library(scales)
library(gridExtra)

#' CHANGE TO ENGLISH LANGUAGE
Sys.setlocale(category = 'LC_ALL', locale = 'english')

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' LOAD .RData FILES
load("data/rdata/gvmi_avr_vls_Andes_2020193.RData")

#' CONSTANTS
k.elev <- c(1500, 4000)
k.regions <- c(6, 8)
k.index <- "gvmi"

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

#' LOAD RASTER DATA
lst.iv <- list.files(
  sprintf("data/raster/index/%s_mod09a1/", k.index),
  pattern = ".tif", full.names = T
)

grd.dem <- raster("data/raster/dem/dem.tif") %>%
  resample(raster(lst.iv[1])) %>%
  crop(sf.region) %>%
  mask(sf.region)

#' SELECT ELEVATION RANGE TO BE CONSIDERED IN THE ANALYSIS
grd.dem[grd.dem < k.elev[1] | grd.dem > k.elev[2]] <- NA
grd.dem[!is.na(grd.dem)] <- 1

#' EXTRACT VALUES FROM "IV" DATA
for (i in 1:length(lst.iv)) {
  print(i)
  if (i == 1) {
    index.avr.vle <- (
      (raster(lst.iv[i]) %>%
        crop(sf.region) %>%
        mask(sf.region)) * grd.dem
    ) %>%
      getValues() %>%
      mean(na.rm = T)
  } else {
    index.avr.vle <- c(
      index.avr.vle,
      (
        (raster(lst.iv[i]) %>%
          crop(sf.region) %>%
          mask(sf.region)) * grd.dem
      ) %>%
        getValues() %>%
        mean(na.rm = T)
    )
  }
}

save(
  index.avr.vle,
  file = sprintf("data/rdata/%s_avr_vls_Andes_2020193.RData", k.index)
)

#' BUILD A DATAFRAME TO PLOT A SEASONAL BEHAVIOR OF "IV"
month.lbl <- tibble(month = sprintf("%.02d", 1:12), lbl = month.abb)

df <- tibble(
  date = ts[1:length(index.avr.vle)],
  value = index.avr.vle,
) %>%
  mutate(
    year = str_sub(date, 1, 4),
    month = str_sub(date, 6, 7)
  ) %>%
  dplyr::select(-date) %>%
  group_by(year, month) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  group_by(month) %>%
  mutate(
    lbl.max = ifelse(value == max(value), year, NA),
    lbl.min = ifelse(value == min(value), year, NA),
    lbl.dry.05 = ifelse(year == "2005", year, NA),
    lbl.dry.10 = ifelse(year == "2010", year, NA),
    lbl.dry.16 = ifelse(year == "2016", year, NA),
    lbl.dry.20 = ifelse(year == "2020", year, NA),
    ubic.max = ifelse(value == max(value), value, NA),
    ubic.min = ifelse(value == min(value), value, NA),
    ubic.dry.05 = ifelse(year == "2005", value %>% as.numeric(), NA),
    ubic.dry.10 = ifelse(year == "2010", value %>% as.numeric(), NA),
    ubic.dry.16 = ifelse(year == "2016", value %>% as.numeric(), NA),
    ubic.dry.20 = ifelse(year == "2020", value %>% as.numeric(), NA),
  ) %>%
  ungroup() %>%
  left_join(month.lbl, by = "month")

#' BOXPLOT OF SEASONAL BEHABIOR OF VEGETATION INDEX BY MONTH
boxplt.iv <- ggplot(df, mapping = aes(month, value)) +
  geom_boxplot(
    alpha = 1, outlier.size = NULL, width = 0.5,
    fatten = 1.5, lwd = .8, color = "gray"
  ) +
  geom_jitter(
    shape = 16,
    size = 0.8, color = "gray",
    position = position_jitter(0.2)
  ) +
  stat_summary(
    fun.y = "mean",
    geom = "point",
    shape = 3, size = 4, colour = "red",
    fill = "red"
  ) +
  scale_x_discrete(
    label = month.lbl$lbl,
    expand = c(.07, .07)
  ) +
  scale_y_continuous(
    breaks = seq(0, .3, .02),
    limits = c(0, .3),
    expand = c(0, 0)
  ) +
  geom_text(
    aes(label = lbl.max),
    size = 3, na.rm = TRUE, hjust = 0.5,
    vjust = -.5, check_overlap = T, color = "gray"
  ) +
  geom_text(
    aes(label = lbl.min),
    size = 3, na.rm = TRUE, hjust = 0.5,
    vjust = 1.3, check_overlap = T, color = "gray"
  ) +
  geom_point(
    aes(month, ubic.max),
    color = "gray", size = 1.5
  ) +
  geom_point(
    aes(month, ubic.min),
    color = "gray", size = 1.5
  ) +
  geom_point(
    aes(month, ubic.dry.05),
    color = "blue", size = 1.5
  ) +
  geom_point(
    aes(month, ubic.dry.10),
    color = "black", size = 1.5
  ) +
  geom_point(
    aes(month, ubic.dry.16),
    color = "green", size = 1.5
  ) +
  geom_point(
    aes(month, ubic.dry.20),
    color = "red", size = 1.5
  ) +
  #  geom_text(
  #    aes(label = txt.dry.05),
  #    size = 3, na.rm = TRUE, hjust = 0.5,
  #    vjust = -.5, check_overlap = F, color = "blue"
  #  ) +
  #  geom_text(
  #    aes(label = txt.dry.10),
  #    size = 3, na.rm = TRUE, hjust = 0.5,
  #    vjust = -.5, check_overlap = F, color = "black"
  #  ) +
  labs(
    title = "Monthly GVMI distribution", subtitle = "from 2000 to 2020",
    y = "gvmi\n"
  ) +
  theme_bw() +
  annotation_ticks(
    sides = "l",
    ticklength = 2 * unit(0.1, "cm")
  ) +
  annotation_ticks(
    sides = "r",
    ticklength = 2 * unit(0.1, "cm")
  ) +
  coord_cartesian(clip = "off") +
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 15),
    axis.text.x = element_text(
      size = 10, colour = "black", hjust = .5
    ),
    axis.text.y = element_text(
      size = 13, colour = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    #panel.background = element_rect(fill = "linen"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      size = 0.3, color = "gray", linetype = "dashed"
    ),
    panel.border = element_rect(size = 1),
    legend.position = "right"
  )

ggsave(
  plot = boxplt.iv,
  sprintf("exports/monthly_gvmi_boxplot_2000-2020_reg%s.png", k.regions),
  width = 13, height = 18, units = "cm", dpi = 1000
)

#' BUILD A DATAFRAME TO PLOT A SEASONAL BEHAVIOR OF "IV"
month.lbl <- tibble(month = sprintf("%.02d", 1:12), lbl = month.abb)

df <- tibble(
  date = ts[1:length(index.avr.vle)],
  value = index.avr.vle,
) %>%
  mutate(
    year = str_sub(date, 1, 4),
    month = str_sub(date, 6, 7),
    day = str_sub(date, 9, 10),
  ) %>%
  dplyr::select(-date) %>%
  #  group_by(year, month) %>%
  #  summarise(value = mean(value, na.rm = T)) %>%
  group_by(month, day) %>%
  mutate(
    oct.day = sprintf("%s-%s", month, day),
    lbl.max = ifelse(value == max(value), year, NA),
    lbl.min = ifelse(value == min(value), year, NA),
    lbl.dry.05 = ifelse(year == "2005", year, NA),
    lbl.dry.10 = ifelse(year == "2010", year, NA),
    lbl.dry.16 = ifelse(year == "2016", year, NA),
    lbl.dry.20 = ifelse(year == "2020", year, NA),
    ubic.max = ifelse(value == max(value), value, NA),
    ubic.min = ifelse(value == min(value), value, NA),
    ubic.dry.05 = ifelse(year == "2005", value %>% as.numeric(), NA),
    ubic.dry.10 = ifelse(year == "2010", value %>% as.numeric(), NA),
    ubic.dry.16 = ifelse(year == "2016", value %>% as.numeric(), NA),
    ubic.dry.20 = ifelse(year == "2020", value %>% as.numeric(), NA),
  ) %>%
  ungroup() %>%
  left_join(month.lbl, by = "month") %>%
  arrange()

#' BOXPLOT OF SEASONAL BEHABIOR OF VEGETATION INDEX BY 8 DAYS
boxplt.iv <- ggplot(df, mapping = aes(oct.day, value)) +
  geom_boxplot(
    alpha = 1, outlier.size = NULL, width = 0.5,
    fatten = 1.5, lwd = .6, color = "gray"
  ) +
  stat_summary(
    fun.y = "mean",
    geom = "point",
    shape = 3, size = 4, colour = "red",
    fill = "red"
  ) +
  scale_x_discrete(
    expand = c(.035, .035)
  ) +
  scale_y_continuous(
    breaks = seq(0, .3, .02),
    limits = c(0, .3),
    expand = c(0, 0)
  ) +
  geom_text(
    aes(label = lbl.max),
    size = 2.5, na.rm = TRUE, hjust = 0.5,
    vjust = -.5, check_overlap = T, color = "gray"
  ) +
  geom_text(
    aes(label = lbl.min),
    size = 2.5, na.rm = TRUE, hjust = 0.5,
    vjust = 1.3, check_overlap = T, color = "gray"
  ) +
  geom_point(
    aes(oct.day, ubic.max),
    color = "gray", size = 1.5
  ) +
  geom_point(
    aes(oct.day, ubic.min),
    color = "gray", size = 1.5
  ) +
  geom_point(
    aes(oct.day, ubic.dry.05),
    color = "blue", size = 1.5
  ) +
  geom_point(
    aes(oct.day, ubic.dry.10),
    color = "black", size = 1.5
  ) +
  geom_point(
    aes(oct.day, ubic.dry.16),
    color = "green", size = 1.5
  ) +
  geom_point(
    aes(oct.day, ubic.dry.20),
    color = "red", size = 1.5
  ) +
  labs(
    title = "Monthly GVMI distribution", subtitle = "from 2000 to 2020",
    y = "gvmi"
  ) +
  annotation_ticks(
    sides = "l",
    ticklength = 2 * unit(0.1, "cm")
  ) +
  annotation_ticks(
    sides = "r",
    ticklength = 2 * unit(0.1, "cm")
  ) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 15),
    axis.text.x = element_text(
      size = 10, colour = "black",
      angle = 90, hjust = 1.5
    ),
    axis.text.y = element_text(
      size = 13, colour = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    #panel.background = element_rect(fill = "linen"),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(
      size = 0.3, color = "gray", linetype = "dashed"
    ),
    panel.border = element_rect(size = 1),
    legend.position = "right"
  )

ggsave(
  plot = boxplt.iv,
  sprintf("exports/octday_gvmi_boxplot_2000-2020_reg%s.png", k.regions),
  width = 25, height = 15, units = "cm", dpi = 1000
)

#' PLOT OF SEASONAL BEHAVIOR OF VEGETATION INDEX BY 8 DAYS
month.lbl <- tibble(month = sprintf("%.02d", 1:12), lbl = month.abb)

df <- tibble(
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
    dry.mean = mean(c(yr.2005, yr.2010, yr.2016), na.rm = T),
    norm.mean = mean(
      c(
        yr.2000, yr.2001, yr.2002, yr.2003, yr.2004, yr.2006, yr.2007, yr.2008,
        yr.2009, yr.2011, yr.2012, yr.2013, yr.2014, yr.2015, yr.2017, yr.2018,
        yr.2019
      ),
      na.rm = T
    ),
  ) %>%
  ungroup() %>%
  dplyr::select(oct.day, dry.mean, norm.mean, yr.2020) %>%
  gather(variable, value, -oct.day)


plt.iv <- ggplot(
  df,
  aes(
    x = oct.day,
    y = value,
    colour = variable,
    group = variable
  )
) +
  geom_line(size = 1.5) +
  geom_point(size = 2.5) +
  labs(
    title = "GVMI monthly distribution", subtitle = "from 2000 to 2020",
    y = "gvmi"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 15),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90, vjust = .5),
    axis.text.y = element_text(size = 13, colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.ticks.length = unit(.15, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(
      size = 0.3, color = "gray", linetype = "dashed"
    ),
    panel.border = element_rect(size = 1),
    legend.key.size = unit(.8, "cm"),
    legend.title = element_text(colour = "black", size = rel(1.5), face = "bold"),
    legend.text = element_text(colour = "black", size = rel(1.2)),
    legend.position = c(.85, .8),
    legend.background = element_rect(
      fill = "white",
      size = 0.4, linetype = "solid",
      colour = "black"
    )
  )

ggsave(
  plot = plt.iv,
  "exports/monthly_gvmi_2000-2020_v3.png",
  width = 25, height = 15, units = "cm", dpi = 1000
)

#' PLOT OF SEASONAL BEHAVIOR OF VEGETATION INDEX BY 8 DAYS
month.lbl <- tibble(month = sprintf("%.02d", 1:12), lbl = month.abb)

df <- tibble(
  date = ts[1:length(index.avr.vle)],
  value = index.avr.vle,
) %>%
  mutate(
    year = sprintf("yr.%s", str_sub(date, 1, 4)),
    oct.day = sprintf("%s-%s", str_sub(date, 6, 7), str_sub(date, 9, 10))
  ) %>%
  dplyr::select(-date) %>%
  spread(year, value) %>%
  rowwise() %>%
  mutate(
    dry.mean = mean(c(yr.2005, yr.2010, yr.2016), na.rm = T),
    norm.mean = mean(
      c(
        yr.2000, yr.2001, yr.2002, yr.2003, yr.2004, yr.2006, yr.2007, yr.2008,
        yr.2009, yr.2011, yr.2012, yr.2013, yr.2014, yr.2015, yr.2017, yr.2018,
        yr.2019
      ),
      na.rm = T
    ),
    max.val = max(
      c(
        yr.2000, yr.2001, yr.2002, yr.2003, yr.2004, yr.2005, yr.2006, yr.2007, yr.2008,
        yr.2009, yr.2010, yr.2011, yr.2012, yr.2013, yr.2014, yr.2015, yr.2016, yr.2017,
        yr.2018, yr.2019
      ),
      na.rm = T
    ),
    min.val = min(
      c(
        yr.2000, yr.2001, yr.2002, yr.2003, yr.2004, yr.2005, yr.2006, yr.2007, yr.2008,
        yr.2009, yr.2010, yr.2011, yr.2012, yr.2013, yr.2014, yr.2015, yr.2016, yr.2017,
        yr.2018, yr.2019
      ),
      na.rm = T
    )
  ) %>%
  ungroup() %>%
  dplyr::select(oct.day, dry.mean, norm.mean, yr.2005, yr.2010, yr.2016, yr.2020, max.val, min.val) %>%
  mutate(
    month = str_sub(oct.day, 1, 2),
    day = str_sub(oct.day, 4, 5)
  ) %>%
  left_join(month.lbl, by = "month") %>%
  mutate(
    lbl = sprintf("%s-%s", lbl, day),
    date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "8 day"),
    name.norm = "meanH"
  )

df.end <- df %>%
  dplyr::select(
    norm.mean, yr.2005, yr.2010, yr.2016, yr.2020, max.val, min.val, date
  ) %>%
  gather(key = "type", value = "value", -date, -max.val, -min.val)

lbls <- c("promedio años\ncondiciones normales", "año 2005", "año 2010", "año 2016", "año 2020")

plt.iv <- ggplot(df.end, aes(x = date, y = value, group = type)) +
  labs(
    title = "Evolución temporal del\níndice de humedad de vegetación (GVMI)\npara la región Andina (1500 - 4000m)"
  ) +
  geom_ribbon(
    aes(ymin = min.val, ymax = max.val),
    size = .2, fill = "gray", color = "gray", alpha = .1
  ) +
  geom_line(aes(linetype = type, color = type, size = type)) +
  geom_point(aes(shape = type, color = type), size = 1) +
  scale_linetype_manual(values = c("dashed", "solid", "solid", "solid", "solid"), labels = lbls) +
  scale_color_manual(values = c("gray", "blue", "black", "green", "red"), labels = lbls) +
  scale_size_manual(values = c(1, .5, .5, .5, .5), labels = lbls) +
  scale_shape_manual(values = c(NA, NA, NA, NA, 19), labels = lbls) +
  scale_x_date(
    limits = c(as.Date("2020-01-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "1 month"),
    date_labels = "%b", expand = expand_scale(mult = c(.02, 0))
  ) +
  scale_y_continuous(
    breaks = seq(0, .3, .04),
    limits = c(-.003, .3)
  ) +
  annotation_ticks(
    sides = "l",
    ticklength = 1 * unit(0.1, "cm"),
    color = "black"
  ) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin = margin(3, 7, 7, 7),
    #legend.key.size = unit(.8, "cm"),
    legend.key.width = unit(.9, "cm"),
    legend.key.height = unit(.4, "cm"),
    legend.position = c(0.82, 0.79),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "Source Sans Pro"),
    plot.title = element_text(size = 15, hjust = .5, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 12, colour = "black", family = "Source Sans Pro",
      face = "bold", angle = 0, vjust = .6
    ),
    axis.text.y = element_text(
      size = 13, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      size = 0.3, color = "gray", linetype = "dashed"
    ),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm"),
    axis.line.y = element_line(
      size = .8, color = "black"
    )
  )

title.axis <- textGrob(
  label = "GVMI", check.overlap = F,
  x = unit(0, "lines"),
  y = unit(-3.8, "lines"),
  hjust = -.3,
  gp = gpar(
    fontsize = 18,
    fontface = "bold",
    col = "black"
  )
)

plt <- gridExtra::arrangeGrob(plt.iv, top = title.axis)

ggsave(
  plot = plt,
  "exports/octday_gvmi_ssnl_behav_2000-2020_Andes_2020193.png",
  width = 20, height = 15, units = "cm", dpi = 1000
)