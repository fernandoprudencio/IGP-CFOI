#' @title
#' Correlation Matrix
#'
#' @description
#' this script plots a correlation matrix of geophysical variables
#'
#' @author Fernando Prudencio
#'
#' @data
#' this data was obtained from Ricardos's results

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("corrplot", "tidyverse", "magrittr", "corrgram")

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
library(corrplot)
library(tidyverse)
library(magrittr)
library(corrgram)

#' LOAD DATA OF MATRIX
df <- read.csv("data/table/correlation_matrix.csv",
  header = T, sep = ";"
) %>% as.matrix()

rownames(df) <- colnames(df)

col <- colorRampPalette(c(
  "#BB4444", "#EE9988", "#FFFFFF",
  "#77AADD", "#4477AA"
))

#' PLOT CORRELATION MATRIX
#'   save plot
png("exports/cormatrix_geo_variables.png",
  width = 20, height = 20, units = "cm", res = 1000
)

corrplot(df,
  method = "pie",
  shade.col = NA,
  tl.col = "black",
  tl.cex = 1,
  tl.srt = 45,
  diag = F,
  type = "upper",
  col = col(200),
  number.cex = .8,
  cl.cex = 1,
  addCoef.col = "black",
  order = "AOE"
)

#'   close the saved of plot
dev.off()