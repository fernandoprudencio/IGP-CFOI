
library(DescTools)
library(dplyr)
library(raster)

rm(list = ls())

setwd('H:/completo/NDWI/')
rutOUT<-'H:/completo/NDWI/FILTER/'
rutIN<-'H:/completo/NDWI/'

#----------------------------------------------------

dataBIN<-DecToBin(c(1:65535))
for (i in 1:length(dataBIN)) {
  nceros=16-nchar(dataBIN[i])
  dataBIN[i]=paste(substr('000000000000000',1,nceros),dataBIN[i],sep = '')
  
}


dataBIN_DF<-data.frame(bin=dataBIN)%>%mutate(dec=c(1:length(bin)))%>%
  filter(substr(bin ,11,13) == "000" | substr(bin ,11,13) == "010" | substr(bin ,11,13) == "011" |substr(bin ,11,13) == "100" |
           substr(bin ,11,13) == "101" | substr(bin ,11,13) == "110" |substr(bin ,11,13) == "111" | #AGUA
           substr(bin ,15,16) == "01" | substr(bin ,15,16) == "10"| #NUBES
           substr(bin ,6,6) == "1"|  #ALGORITMO DE NUBE
           substr(bin ,9,10) == "11"|#AEROSOLES
           substr(bin ,1,1) == "1"|#NIEVE
           substr(bin ,4,4) == "1"|#NIEVE/HIELO
           substr(bin ,3,3) == "1"#PIXEL ADYACENTE A LA NUBE
  )


#--------------------------------------------------------------------------------------------------------------------#


QA_rut<-list.files(pattern='state',full.names = T)
band_rut<-list.files(paste(rutIN),pattern = 'NDWI',full.names = T)

for (k in 1:782) {
  print(k)
  print(date())
  #NOMBRE DEL NUEVO ARCHIVO DE NDWI
  name = list.files(pattern='state')[k]%>%substr(36,42)
  #LLAMANDO A LAS BANDAS QA Y NDWI
  QA = raster(QA_rut[k])
  band = raster(band_rut[k])
  #BANDA QA A VALORES DE 1 Y NA
  QA[QA%in%dataBIN_DF$dec] = NA
  QA[!is.na(QA)] = 1
  #FILTRANDO LOS NDWI
  BAND_FILT = band*QA
  writeRaster(BAND_FILT, paste(rutOUT,'MOD09A1_NDWI_',name,'.tif',sep=''),overwrite=T)
  date()
  
}



