#'@author  Fernando Prudencio
#'this script calculate different index from mod09a1 dataset
#'installing packages
rm(list = ls())
pkg <- c('sf', 'dplyr', 'raster', 'dfcrm', 'rgdal', 'doParallel', 'foreach')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) }})

library(doParallel)
library(dplyr)
library(sf)
library(ggplot2)
library(mapview)
library(ggpubr)
library(raster)
library(velox)
library(xts)

#'chanching to English language
Sys.setlocale(category = 'LC_ALL', locale = 'english')

#'reading functions
source('scripts/functions.R')

#'reading data in .RDtada files
load('info/avg_myd09a1_ndvi_by_Andes_region.RData')
load('info/myd09a01_ndvi_timeSerie.RData')

#'reading vectorial data
fires  <- read_sf('data/shp/fires_register/FiresRegister_2000_Dic2019.shp')
region <- read_sf('data/shp/bastian_region/bastian_region.shp')
depar  <- read_sf('data/shp/area_estudio/Peru_departamentos.shp')


#'reading raster data
dem <- raster('data/dem/dem.tif')
ndvi_list <- list.files('data/ndvi_myd09a1/', pattern = '.tif', full.names = T)

#'ploting vectorial data over base map
vctr  <- list(region[1], fires[1])
mapview(vctr, alpha.regions = F, cex = 1, legend = F)

#'extracting ndvi data by vector data
avg_ndvi_by_Andes_region <- lapply(ndvi_list, function(x){ extract_data(x, region %>% filter(Region %in% c('Andes west slope', 'Andes east slope'))) })
save(avg_ndvi_by_Andes_region, file = 'info/avg_ndvi_by_Andes_region.RData')

#'calculating monthly ndvi and every 8 days
df_ndvi <- data.frame(date = ts, ndvi = avg_ndvi_by_Andes_region %>% as.numeric()) %>% 
              mutate(ANO = substr(date, 1, 4), MES = substr(date, 6, 7)) %>%
                filter(ANO != 2019) %>% group_by(MES) %>% summarise(ndvi = mean(ndvi, na.rm = T))

df_ndvi1 <- data.frame(date = ts, ndvi = avg_ndvi_by_Andes_region %>% as.numeric()) %>% 
              mutate(ANO = substr(date, 1, 4), MES = substr(date, 6, 7), id = c(24:46, rep(1:46, 17))) %>%
                filter(!(ANO %in% c(2005,2010,2016,2019))) %>% group_by(id) %>% summarise(ndvi = mean(ndvi, na.rm = T))

df_ndvi2 <- data.frame(date = ts, ndvi = avg_ndvi_by_Andes_region %>% as.numeric()) %>% 
              mutate(ANO = substr(date, 1, 4), MES = substr(date, 6, 7), id = c(24:46, rep(1:46, 17))) %>%
                filter((ANO %in% c(2005,2010,2016))) %>% group_by(id) %>% summarise(ndvi = mean(ndvi, na.rm = T))


#'ploting accumuled frequency of fires by month
df <- fires %>% filter(ANO %in% c(2000:2004,2006:2009,2011:2015,2017:2018)) %>%
                    mutate(date = paste(ANO,MES,'01',sep = '-') %>% as.Date()) %>% 
                      group_by(date) %>% summarise(nfires = length(date)) %>% arrange(date) %>% 
                        mutate(MES = substr(date,6,7)) %>% as.data.frame() %>% dplyr::select(-geometry) %>%
                          group_by(MES) %>% summarise(nfires = mean(nfires, na.rm = T)) %>%
                            mutate(acum_freq = cumsum(nfires))

df <- fires %>% mutate(date = paste(ANO, MES, DIA, sep = '-') %>% as.Date()) %>%
                  as.data.frame() %>% dplyr::select(date) %>% arrange(date) %>% 
                    group_by(date) %>% summarise(nfires = length(date) %>% as.numeric())
                      #filter(date >= '2005-01-01' & date <= '2005-12-31')

df_fires <- data.frame(row.names = 46)

for(i in 2002:2019){
  #i = 2004
  if (i %% 4 == 0) {
    date      = data.frame(date = seq(paste(i,'01','01', sep = '-') %>% as.Date(), paste(i+1,'01','02',sep = '-') %>% as.Date(), by = '1 day'))
    fire_acum = df %>% filter(substr(date,1,4) == i) %>% right_join(date) %>% mutate(nfires = if_else(is.na(nfires), 0, nfires)) %>% as.data.frame()
    fire_xts  = xts(fire_acum[,-1], order.by = fire_acum[,1])
    fire_rspl = period.sum(fire_xts, c(seq(8, 361, 8), 368)) %>% as.data.frame()
    names(fire_rspl) = 'nfires'
    fire_rspl = fire_rspl %>% mutate(date = seq(paste(i,'01','01', sep = '-') %>%
                  as.Date(), paste(i,'12','31',sep = '-') %>% as.Date(), by = '8 days')) #endpoints()
    df_fires  = rbind(df_fires, fire_rspl)
  }else{
    date      = data.frame(date = seq(paste(i,'01','01', sep = '-') %>% as.Date(), paste(i+1,'01','03',sep = '-') %>% as.Date(), by = '1 day'))
    fire_acum = df %>% filter(substr(date,1,4) == i) %>% right_join(date) %>% mutate(nfires = if_else(is.na(nfires), 0, nfires)) %>% as.data.frame()
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

df1 <- df_fires1 %>% left_join(df_ndvi1) %>% mutate(date = seq(as.Date('2020-01-01'), as.Date('2020-12-31'), by = '8 days'))
df2 <- df_fires2 %>% left_join(df_ndvi2) %>% mutate(date = seq(as.Date('2020-01-01'), as.Date('2020-12-31'), by = '8 days'))

df <- df1 %>% left_join(df2, by = 'id')

#'homogenizing the primary and secondary axis
ylim.prim <- c(min(df1$ndvi, na.rm = T), max(df$ndvi, na.rm = T))   # in this example, INCENDIOS
ylim.sec <- c(min(df$acum_freq), max(df$acum_freq))    # in this example, PRECIPITACION

ylim.prim <- c(min(df$ndvi.x, df$ndvi.y), max(df$ndvi.x, df$ndvi.y))
ylim.sec  <- c(min(df$acum_freq.x, df$acum_freq.y), max(df$acum_freq.x, df$acum_freq.y))

n <- diff(ylim.prim)/diff(ylim.sec)
m <- ylim.prim[1] - n*(ylim.sec[1])

X <- ggplot(df, aes(x = date.x, y = ndvi.x)) +
      geom_line(stat = "identity", colour ="blue") +
      geom_line(aes(date.x, y = ndvi.y), colour = "blue", linetype = 'dashed') +
      theme_bw() + ylab(label = 'average ndvi\n') +  xlab(label = '') +
      theme(plot.title = element_text(size=20, hjust = 0.5),
            plot.subtitle = element_text(size=12, hjust = 0.5),
            axis.text.x = element_text(size=16, hjust = -0.2, angle = 0),
            axis.text.y = element_text(size=16),
            axis.title.y = element_text(size = 20),
            axis.ticks.length = unit(.15, "cm")) +
      scale_x_date(date_labels = '%b', breaks = '1 month', expand = c(0,0)) +#
      scale_y_continuous(breaks = seq(0.2,.5,0.02), sec.axis = sec_axis(~ (. - m)/n, name = 'cumulative fire frequency\n',
                                                                        breaks = seq(0,700,50)), expand = c(0,0)) +#
      geom_line(aes(y = m + acum_freq.x*n), colour = 'red') +
      geom_line(aes(y = m + acum_freq.y*n), colour = 'red', linetype = 'dashed') +
      geom_point(aes(date.x, y = ndvi.y), colour = 'blue', shape = 1, size = 2) +
      geom_point(aes(date.x, y = ndvi.x), colour = 'blue', size = 2) +
      geom_point(aes(y = m + acum_freq.y*n), colour = 'red', shape = 1, size = 2) +
      geom_point(aes(y = m + acum_freq.x*n), colour = 'red', size = 2)
X
ggsave(X, filename = 'exports/plot3_v2.png',
       width = 22, height = 20, units = "cm", dpi = 1000)

#'ploting monthly fires distribution by elevation intervals in a boxplot
mn <- data.frame(MES = c(1:12), MES_txt = paste(letters[1:12], '-', substr(month.name, 1, 3), sep = ''))
df <- fires %>% mutate(elev = raster::extract(dem, fires)) %>% left_join(mn)

plt <-  ggplot(df, mapping = aes(x = MES_txt, y = elev)) +
          geom_jitter(shape = 16, position = position_jitter(0.2), size = 0.5, color = 'blue') +
          geom_boxplot(alpha = 0, outlier.alpha = 1, width = 0.5, outlier.size = 1,
                       fatten = 1.5, lwd = .8) +
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

ggsave(plt, filename = 'exports/Fires_distribution_by_elevation_intervals_v2.png',
       width = 20, height = 20, units = "cm", dpi = 1000)

