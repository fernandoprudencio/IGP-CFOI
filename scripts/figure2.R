#'@author  Fernando Prudencio
#'this script calculate different index from mod09a1 dataset
#'installing packages
rm(list = ls())
pkg <- c('sf', 'tidyverse', 'raster', 'doParallel', 'foreach', 'ggplot2', 'mapview', 'xts')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) } })

#'Charging the libraries
library(sf)
library(tidyverse)
library(raster)
library(doParallel)
library(foreach)
library(ggplot2)
library(mapview)
library(xts)
#library(ggpubr)

#'Chanching to English language
Sys.setlocale(category = 'LC_ALL', locale = 'english')

#'reading functions
source('scripts/functions.R')

#'reading data in .RData files
load('info/avg_mod09a1_vari_by_Andes_region.RData')#==== OPTIONAL ====
load('info/mod09a01_timeSerie.RData')

#'reading vectorial data
fires  <- read_sf('data/vector/fires_register/FiresRegister_2000_Dic2019.shp')
region <- read_sf('data/vector/bastian_region/bastian_region.shp')
waterb <- read_sf('data/vector/CuerposLoticos/Clasificacion_CuerposLoticos.shp')
glacia <- read_sf('data/vector/InvGlaciares/InvGlaciares.shp')
lagun  <- read_sf('data/vector/InvLagunasGla/InvLagunasAlto.shp')
depar  <- read_sf('data/vector/area_estudio/Peru_departamentos.shp')

#'reading raster data
dem <- raster('data/dem/dem_from_1500_to_4500_resampled_mask.tif') %>% mask(region %>% filter(Region %in% c('Andes west slope', 'Andes east slope')))
dem <- raster("data/dem/dem.tif")
#dem[dem < 1500] <- NA 
#dem[dem > 4000] <- NA
#dem[!is.na(dem)] <- 1
#dem <- resample(dem, raster(index_list[1]))
#writeRaster(dem, 'data/dem/dem_from_1500_to_4000_resampled_mask2.tif')
index_list <- list.files('data/raster/index/vari_mod09a1/', pattern = '.tif', full.names = T)

#'ploting vectorial data over base map ==== OPTIONAL ====
vctr  <- list(region[1], fires[1])
mapview(vctr, alpha.regions = F, cex = 1, legend = F)

#'extracting ndvi data by vector data
avg_index_by_Andes_region <- lapply(index_list, function(x){ return((raster(x)*dem) %>% getValues() %>% mean(na.rm = T)) })
save(avg_index_by_Andes_region, file = 'info/avg_mod09a1_vari_by_Andes_region.RData')

#'calculating monthly ndvi and every 8 days
df_index1 <- data.frame(date = ts, index = avg_index_by_Andes_region %>% as.numeric()) %>% 
              mutate(ANO = substr(date, 1, 4), MES = substr(date, 6, 7), id = rep(1:46, 18)) %>%
                filter(!(ANO %in% c(2005,2010,2016,2019))) %>% group_by(id) %>% summarise(index = mean(index, na.rm = T))
n <- df_index1$index
n[n<1 & n>-1] %>% plot(type = 'l')
df_index2 <- data.frame(date = ts, index = avg_index_by_Andes_region %>% as.numeric()) %>% 
              mutate(ANO = substr(date, 1, 4), MES = substr(date, 6, 7), id = rep(1:46, 18)) %>%
                filter((ANO %in% c(2005,2010,2016))) %>% group_by(id) %>% summarise(index = mean(index, na.rm = T))

#'ploting accumuled frequency of fires by date
df <- fires %>% mutate(date = paste(ANO, MES, DIA, sep = '-') %>% as.Date()) %>%
                  as.data.frame() %>% dplyr::select(date) %>% arrange(date) %>% 
                    group_by(date) %>% summarise(nfires = length(date) %>% as.numeric())
                      #filter(date >= '2005-01-01' & date <= '2005-12-31')

df_fires <- data.frame(row.names = 46)

for(i in 2002:2019){
  #i = 2004
  if (i %% 4 == 0) {
    date      = data.frame(date = seq(paste(i,'01','01', sep = '-') %>% as.Date(), paste(i+1,'01','02',sep = '-') %>% as.Date(), by = '1 day'))
    fire_acum = df %>% filter(substr(date,1,4) == i) %>% right_join(date, by = 'date') %>% mutate(nfires = if_else(is.na(nfires), 0, nfires)) %>% as.data.frame()
    fire_xts  = xts(fire_acum[,-1], order.by = fire_acum[,1])
    fire_rspl = period.sum(fire_xts, c(seq(8, 361, 8), 368)) %>% as.data.frame()
    names(fire_rspl) = 'nfires'
    fire_rspl = fire_rspl %>% mutate(date = seq(paste(i,'01','01', sep = '-') %>%
                  as.Date(), paste(i,'12','31',sep = '-') %>% as.Date(), by = '8 days')) #endpoints()
    df_fires  = rbind(df_fires, fire_rspl)
  }else{
    date      = data.frame(date = seq(paste(i,'01','01', sep = '-') %>% as.Date(), paste(i+1,'01','03',sep = '-') %>% as.Date(), by = '1 day'))
    fire_acum = df %>% filter(substr(date,1,4) == i) %>% right_join(date, by = 'date') %>% mutate(nfires = if_else(is.na(nfires), 0, nfires)) %>% as.data.frame()
    fire_xts  = xts(fire_acum[,-1], order.by = fire_acum[,1])
    fire_rspl = period.sum(fire_xts, c(seq(8, 361, 8), 368)) %>% as.data.frame()
    names(fire_rspl) = 'nfires'
    fire_rspl = fire_rspl %>% mutate(date = seq(paste(i,'01','01', sep = '-') %>%
                  as.Date(), paste(i,'12','31',sep = '-') %>% as.Date(), by = '8 days')) #endpoints()
    df_fires  = rbind(df_fires, fire_rspl)
  }
}

df_fires1 <- df_fires %>% mutate(id = c(rep(1:46,18))) %>% filter(!(substr(date,1,4) %in% c(2005,2010,2016,2019))) %>% 
                            group_by(id) %>% summarise(nfires = mean(nfires, na.rm = T)) %>% mutate(acum_freq = cumsum(nfires))

df_fires2 <- df_fires %>% mutate(id = c(rep(1:46,18))) %>% filter((substr(date,1,4) %in% c(2005,2010,2016))) %>% 
                            group_by(id) %>% summarise(nfires = mean(nfires, na.rm = T)) %>% mutate(acum_freq = cumsum(nfires))

df1 <- df_fires1 %>% left_join(df_index1, by = 'id') %>% mutate(date = seq(as.Date('2020-01-01'), as.Date('2020-12-31'), by = '8 days'))
df2 <- df_fires2 %>% left_join(df_index2, by = 'id') %>% mutate(date = seq(as.Date('2020-01-01'), as.Date('2020-12-31'), by = '8 days'))

df <- df1 %>% left_join(df2, by = 'id')

#'homogenizing the primary and secondary axis
ylim.prim <- c(min(df$index.x, df$index.y), max(df$index.x, df$index.y))
ylim.sec  <- c(min(df$acum_freq.x, df$acum_freq.y), max(df$acum_freq.x, df$acum_freq.y))

n <- diff(ylim.prim)/diff(ylim.sec)
m <- ylim.prim[1] - n*(ylim.sec[1])

X <- ggplot(df, aes(x = date.x, y = index.x)) +
      geom_line(stat = "identity", colour ="blue") +
      geom_line(aes(date.x, y = index.y), colour = "blue", linetype = 'dashed') +
      theme_bw() + ylab(label = 'average vari\n') +  xlab(label = '') +
      ggtitle('Monthly distribution of TERRA VARI (mod09a1) and\ncumulative fire frequency', subtitle = 'from 2002 to 2018') + 
      theme(plot.title = element_text(size=20, hjust = 0),
            plot.subtitle = element_text(size=15, hjust = 0, color = 'gray30'),
            axis.text.x = element_text(size=16, hjust = -0.2, angle = 0),
            axis.text.y = element_text(size=16),
            axis.title.y = element_text(size = 20),
            axis.ticks.length = unit(.15, "cm")) +
      scale_x_date(date_labels = '%b', breaks = '1 month', expand = c(0,0)) +#
      scale_y_continuous(breaks = seq(0.01,0.17,0.01), sec.axis = sec_axis(~ (. - m)/n, name = 'cumulative fire frequency\n',
                                                                        breaks = seq(0,700,50)), expand = c(0,0)) +#
      #' y limits for NDWI 0,0.26,0.02
      #' y limits for NDVI 0.25,0.5,0.02
      #' y limits for EVI 0.15,0.26,0.01
      #' y limits for SAVI 0.19,0.32,0.01
      #' y limits for NDII -0.14,0.03,0.01
      #' y limits for GEMI 0.45,0.54,0.01
      #' y limits for GVMI 0.01,0.17,0.01
      geom_line(aes(y = m + acum_freq.x*n), colour = 'red') +
      geom_line(aes(y = m + acum_freq.y*n), colour = 'red', linetype = 'dashed') +
      geom_point(aes(date.x, y = index.y), colour = 'blue', shape = 1, size = 2) +
      geom_point(aes(date.x, y = index.x), colour = 'blue', size = 2) +
      geom_point(aes(y = m + acum_freq.y*n), colour = 'red', shape = 1, size = 2) +
      geom_point(aes(y = m + acum_freq.x*n), colour = 'red', size = 2)
X
summary(df)
ggsave(X, filename = 'exports/Seasonal_distribution_of_VARI_TERRA_and_Accumulated_Frequency_of_fires.png',
       width = 22, height = 20, units = "cm", dpi = 1000)

#'ploting monthly fires distribution by elevation intervals in a boxplot
mn <- data.frame(MES = c(1:12), MES_txt = paste(letters[1:12], '-', substr(month.name, 1, 3), sep = ''))
df <- fires %>%
  mutate(elev = raster::extract(dem, fires)) %>%
  filter(ANO != 2019) %>%
  left_join(mn) %>%
  dplyr::select(MES_txt, elev) %>%
  arrange(MES_txt) %>%
  drop_na()

plt <- ggplot(df, mapping = aes(x = MES_txt, y = elev)) +
          geom_boxplot(alpha = 0, outlier.alpha = 1, width = 0.5, outlier.shape = NA,
                       fatten = 1.5, lwd = .8) +
          geom_jitter(shape = 16, position = position_jitter(0.2), size = 0.8, color = 'blue') +
          stat_summary(fun.y = 'mean', geom = 'point', shape = 3, size = 4, colour = 'red',
                       width = 0.3, fill = 'red') +
          theme_bw() +
          ggtitle('Monthly fires Distribution by Elevation intervals', subtitle = 'from 2000 to 2018') +
          labs(x = '', y = '') +
          scale_x_discrete(label = substr(month.name,1,3), expand = c(0.03,0.03)) +
          scale_y_continuous(breaks = seq(0, 5250, 500), limits = c(0, 5250), expand = c(0,0)) +
          theme(plot.title    = element_text(size = 15),
                plot.subtitle = element_text(size = 15),
                axis.text.x   = element_text(size = 15, colour = 'black'),
                axis.text.y   = element_text(size = 15, colour = 'black'),
                axis.title    = element_text(size = 20),
                axis.ticks.length = unit(.15, "cm"),
                panel.border = element_rect(colour = "black", fill = NA, size = 1.2))
plt
ggsave(plt, filename = 'exports/Fires_distribution_by_elevation_intervals_v3.png',
       width = 20, height = 20, units = "cm", dpi = 1000)

