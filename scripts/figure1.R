#'@author  Fernando Prudencio
#'this script plots a boxplot and violimplot about fires register
#'installing packages
rm(list = ls())
pkg <- c('sf', 'dplyr', 'raster', 'dfcrm', 'rgdal', 'doParallel', 'foreach')
sapply(pkg, function(x){ if(x %in% rownames(installed.packages()) == FALSE) { install.packages(x) }})

library(doParallel)
library(dplyr)
library(sf)
library(ggplot2)

#'CHANGING TO ENGLISH LANGUAGE
Sys.setlocale(category = 'LC_ALL', locale = 'english')

#'READING FUNCTIONS
source('scripts/functions.R')

#'CREATING A TIME SERIE
ts <- data.frame(date = seq(as.Date('2002-01-01'), as.Date('2019-12-31'), by = 'month'))

#'READING VECTORIAL DATA
fires <- read_sf('data/shp/fires_register/FiresRegister_2000_Dic2019.shp')

#'MAKING DATA FRAME TO PLOT A BOXPLOT ABOUT NUMBER OF FIRES BY MONTH
#'making dataframe to plot
df <- fires %>% dplyr::select(ANO, MES) %>% arrange(ANO, MES) %>% 
        mutate(date = paste(ANO, MES, '01', sep = '-') %>% as.Date()) %>% filter(ANO != 2019) %>%
           group_by(date, ANO, MES) %>% summarise(nfires = length(date) %>% as.numeric()) %>% as.data.frame() %>%
              right_join(ts, by = 'date') %>% mutate(MES = substr(date,6,7), nfires = if_else(is.na(nfires), 0, nfires)) %>%
                 dplyr::select(ANO, MES, nfires) %>% group_by(MES) %>%
                    mutate(outlier = ifelse(is_outlier(nfires), ANO, NA)) %>%
                      mutate(ANO = '2020', date = paste(ANO, MES, '01', sep = '-') %>% as.Date())

label.mth <- data.frame(MES = df$MES[1:12], MES.label = substr(month.name, 1, 3))

df <- df %>% left_join(label.mth)

#'PLOTING VIOLIMPLOT
X <- ggplot(df, aes(x = MES, y = nfires)) + 
      geom_violin(trim = T, fill = F, color="black", scale = 'width') +
      geom_jitter(color = 'blue', size = 0.7, shape = 16, position = position_jitter(0)) +
      geom_boxplot(alpha = 0, outlier.alpha = 1, color = 'black', width = 0.2, outlier.colour = 'black', outlier.size = 1.5) +
      stat_summary(fun.y = 'mean', geom = 'point', shape = 3, size = 2, colour = 'red',
                   width = 0.3, fill = 'red') +
      #geom_text(aes(label = outlier), size = 4, na.rm = TRUE, hjust = -0.1, vjust = -.1, check_overlap = T) +
      ggtitle('Monthly fires distribution', subtitle = 'from 2000 to 2018') + 
      labs(x = '', y = '') + theme_bw() +
      theme(plot.title    = element_text(size=15),
            plot.subtitle = element_text(size=15),
            axis.text.x   = element_text(size=13, colour = 'black'),
            axis.text.y   = element_text(size=13, colour = 'black'),
            axis.title    = element_text(size=20),
            axis.ticks.length = unit(.15, "cm")) +
      scale_x_discrete(label = df$MES.label) +
      scale_y_continuous(breaks = seq(0, 450, 50), limits = c(0, 450), expand = c(0,0))

X  
ggsave(plot = X, filename = 'exports/Fires_distribution_by_Bastian_regions_v2.png',
       width = 15, height = 15, units = "cm", dpi = 1000)
  