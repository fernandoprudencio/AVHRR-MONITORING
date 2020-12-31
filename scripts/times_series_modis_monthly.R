#' @title
#' calculate monthly NDVI
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

#' LOAD PACKAGES
library(tidyverse)
library(raster)

#' LOAD MODIS DATE
load("data/rdata/modis_date.RData")
df.date <- tibble(date = modis.date) %>% mutate(id = 1:n())

#' LOAD LIST OF RASTER
lst <- list.files("data/raster/ndvi/octday", ".tif", full.names = T)

#' CALCULATE MONTHLY NDVI
monthly.date <- str_sub(modis.date, 1, 7) %>% table() %>% names()

for (i in monthly.date) {
  print(i)
  bands <- dplyr::filter(df.date, str_sub(date, 1, 7) == i)
  ndvi <- stack(lst[bands$id]) %>% max(na.rm = T)
  writeRaster(
    x = ndvi, overwrite = T, datatype = "INT2S",
    filename =
      sprintf("data/raster/ndvi/monthly/MOD09A1.006_NDVI_doy%1s.tif", i)
  )
}