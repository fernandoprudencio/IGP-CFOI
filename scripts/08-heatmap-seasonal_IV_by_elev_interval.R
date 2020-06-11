#'@author  Fernando Prudencio
#'this script does a cluster analysis
rm(list = ls())
pkg <- c('sf', 'tidyverse', 'raster', 'cluster')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) } })

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(velox)
library(sf)
library(ncdf4)
library(raster)

#'Chanching to English language
Sys.setlocale(category = 'LC_ALL', locale = 'english')

#'reading functions
source('scripts/functions.R')

#'READING VECTORIAL DATA
region <- read_sf('data/shp/bastian_region/bastian_region.shp')
region_elev <- read_sf('data/shp/inter_elev/inter_elev_200m_all_Andes_dissolve.shp') %>% arrange(gridcode)

#'READING RASTER DATA
pp <- stack(list.files('data/rainfall/climatology/', pattern = '.tif', full.names = T))
dem <- raster('data/dem/dem.tif')#%>% mask(region %>% filter(Region %in% c('Andes west slope')))
stck <- brick('data/rainfall/climatology/PISCOp_v2.1.1_climatology_2000_to_2017_with_DEM_and_without_dry_years.nc')
dem[dem <= 0] <- NA
#dem <- raster('data/dem/dem_Andes.tif')
#dem_rspl <- resample(dem, pp)
writeRaster(dem, 'data/dem/dem_Andes_west_slope_v2.tif')

#'RESAMPLING DATA TO DEM RASTER
rstr_stack <- stack(resample(dem, pp), pp)
#writeRaster(rstr_stack, 'data/rainfall/climatology/PISCOp_v2.1.1_climatology_2000_to_2017_with_DEM_and_without_dry_years.nc')

#'RECLASSIFYING RASTER
rc     <- rcls_vector(0, maxValue(dem), 200)
rclmat <- matrix(rc, ncol = 3, byrow = TRUE) %>% as.data.frame()
names(rclmat) <- c('elevmin','elevmax','gridcode')
rclmat <- rclmat %>% as.tibble()
#dem_rc <- reclassify(dem, rclmat)
#dem_rc[dem_rc == 0] <- NA
#dem_rc_mask <- dem_rc %>% mask(region %>% filter(Region %in% c('Andes west slope', 'Andes east slope')))

#'MAKING CLIP
region_elev <- region_elev %>% left_join(rclmat, by = 'gridcode')
area <- st_intersection(region_elev, region)
write_sf(region_elev, 'data/shp/inter_elev/inter_elev_200m_all_Andes_dissolve_v2.shp')

#'CONVERTING RASTER TO POLYGON
dem_sf <- rasterToPolygons(dem_rc_mask, dissolve = T) %>% disaggregate() %>% st_as_sf() %>%
            mutate(id = 1:15, area = st_area(geometry)/1000000)
write_sf(dem_sf, 'data/shp/inter_elev_200m_all_Andes.shp')

#'BUILDING DATAFRAME
df <- stck %>%
            mask(region %>% filter(Region == 'Andes west slope')) %>%
              getValues() %>% as_tibble() %>% drop_na() %>% arrange(X1)
names(df) <- c('elev', substr(month.name,1,3))

library(arules)
library(ggplot2)
df2 <- df %>% mutate(category = cut(elev, breaks = seq(1000, 5200, 200))) %>%
                  group_by(category) %>% summarise_each(funs(mean)) %>% dplyr::filter(elev >= 1500)

df2 <- df2 %>%
              mutate(minelev = seq(1400,5000,200), maxelev = seq(1600,5200,200), inter = paste('(', minelev, ' - ', maxelev, ']', sep = '')) %>%
                dplyr::select('inter', substr(month.name,1,3)) %>% melt(id.vars = 'inter')
head(df2)
names(df2)[2:3] <- c('mes','pp')
df2 <- df2 %>% mutate(pp = as.factor(pp)) %>% as.tibble()

hm.palette <- colorRampPalette(brewer.pal(9, 'YlGnBu'), space='Lab')

X <- ggplot(df2, aes(mes, inter)) + geom_raster(aes(fill=pp)) +
  scale_fill_gradientn(colours = hm.palette(100)) + theme_bw() +
  ggtitle('Seasonal Rainfall by Elevation', subtitle = 'from 1981 to 2018') + 
  labs(x = '', y = 'Elevation interval [m]\n', fill='[mm]') +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme(plot.title    = element_text(size = 15),
        plot.subtitle = element_text(size = 15),
        legend.title  = element_text(size = 12),
        axis.text.x   = element_text(size = 12),
        axis.text.y   = element_text(size = 12),
        axis.title    = element_text(size = 20))
X
ggsave(plot = X, filename = 'C:/Users/PC/Desktop/IGP_EXPO_v2/export/fig51.png',
       width = 17, height = 15, units = "cm", dpi = 500)

