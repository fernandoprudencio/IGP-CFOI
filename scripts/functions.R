#'this function obtains a binary number with a defined digits number "bits"
binND <- function(number, nbits){#number in binary system, nbits is digits number
  nzeros     = nbits - nchar(number)
  return(paste(substr('0000000000000000',1,nzeros), number, sep = ''))
}

#'this function filters MODIS dataset by quality band
#'this is the order of 16 bits of the quality band
# (15)(14)(13)(12)(11)(10)(09)(08)(07)(06)(05)(04)(03)(02)(01)(00) - MODIS NOMENCLATURE
# (01)(02)(03)(04)(05)(06)(07)(08)(09)(10)(11)(12)(13)(14)(15)(16) - R NOMENCLATURE
#'
qaFilter <- function(band, qaband, type, filter){
  dataBIN = sapply(DecToBin(c(1:65535)), FUN = function(x) {binND(x, nbits = 16)}) %>% as.character()
  if (type == 'mxd09a1') {
  dataBIN_df = data.frame(bin = dataBIN) %>% mutate(dec = c(1:length(bin))) %>%
    filter(substr(bin ,15,16) %in% filter[[1]]  | # Cloud State
           substr(bin ,14,14) %in% filter[[2]]  | # Cloud shadow
           substr(bin ,11,13) %in% filter[[3]]  | # Land/Water Flag
           substr(bin ,9, 10) %in% filter[[4]]  | # Aerosol Quantity
           substr(bin ,7, 8)  %in% filter[[5]]  | # Cirrus detected
           substr(bin ,6, 6)  %in% filter[[6]]  | # Internal Cloud Algorithm Flag
           substr(bin ,5, 5)  %in% filter[[7]]  | # Internal Fire Algorithm
           substr(bin ,4, 4)  %in% filter[[8]]  | # MOD35 snow/ice flag
           substr(bin ,3, 3)  %in% filter[[9]]  | # Pixel adjacent to cloud
           substr(bin ,2, 2)  %in% filter[[10]] | # BRDF correction performed 
           substr(bin ,1, 1)  %in% filter[[11]])  # Internal Snow Mask
  }
#'changing the values of the quality band to NA and 1
  qaband[qaband %in% dataBIN_df$dec] = NA
  qaband[!is.na(qaband)] = 1
  return(band*qaband)
}

#'this function calculates different indexes related with vegetation
indexMODIS <- function(index, redBand, nirBand, blueBand, greenBand, swir1Band, swir2Band, swir3Band){
  if (index == 'ndvi') { return((nirBand - redBand)/(nirBand + redBand)) }
  if (index == 'savi') { return((nirBand - redBand)*(1+0.25)/(nirBand + redBand + 0.25)) }
  if (index == 'ndii') { return((nirBand - swir2Band)/(nirBand + swir2Band)) }
  if (index == 'gemi') { eta = ((2*((nirBand^2) - (redBand^2))) + (1.5*nirBand) + (0.5*redBand))/(nirBand + redBand + 0.5)
                         return((eta*(1 - (0.25*eta))) - ((redBand - 0.125)/(1 - redBand))) }
  if (index == 'ndwi') { return((nirBand - swir1Band)/(nirBand + swir1Band)) }
  if (index == 'vari') { return((greenBand - redBand)/(greenBand + redBand - blueBand)) }
  if (index == 'evi')  { return(2.5*(nirBand - redBand)/(nirBand + (6*redBand) - (7.5*blueBand) + 1)) }
  if (index == 'gvmi') { return(((nirBand + 0.1) - (swir2Band + 0.02))/((nirBand + 0.1) + (swir2Band + 0.02))) }
}

#'this function plots histogram
histPLOT <- function(x, field, width, fill, col, title, subtitle){
  plt = ggplot(x, aes(x %>% dplyr::select(field))) + geom_histogram(binwidth = width, fill = fill, col = col) +
    theme_bw() + labs(x = '', y = '') +
    #ggtitle(title, subtitle) +
    theme(plot.title    = element_text(size=15),
          plot.subtitle = element_text(size=13),
          axis.text.x   = element_text(size=12),
          axis.text.y   = element_text(size=12),
          axis.title    = element_text(size=20),
          panel.border  = element_rect(colour = "black", fill=NA, size=1)) +
    scale_y_continuous(breaks = seq(0,200,25), limits = c(0,200), expand = c(0,0))
  return(plt)
}

#'this function extrats average value of raster by polygon vector
extract_data <- function(file, st){
  return(raster(file) %>% mask(st) %>% getValues() %>% mean(na.rm = T))
}

#'this function return a logic value if it is an outlier vlaue or no
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
