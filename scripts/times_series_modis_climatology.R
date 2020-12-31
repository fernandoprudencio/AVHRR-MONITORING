#' @title
#' calculate NDVI climatology
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
df <- tibble(
  date = seq(as.Date("2002-01-01"), as.Date("2018-12-01"), by = "1 month")
) %>%
  mutate(id = 1:n())

#' LOAD LIST OF RASTER
lst <- list.files("data/raster/ndvi/monthly", ".tif", full.names = T)

#' BUILD A FUNCTION TO CALCULATE CLIMATOLOGY
fun.clim <- function(month, data) {
  grd.mt <- filter(df, str_sub(date, 6, 7) == month)

  ndvi <- data[grd.mt$id] %>%
    stack() %>%
    "*"(1) %>%
    mean(na.rm = T)

  writeRaster(
    ndvi,
    overwrite = T, datatype = "INT2S",
    filename =
      sprintf(
        "data/raster/ndvi/climatology/MOD09A1.006_NDVI_doy%1s.tif",
        month
      )
  )
}

#' APPLY fun.clim() FUNCTION
grd.clim <-
  sapply(
    sprintf("%02d", 1:12),
    FUN = fun.clim,
    data = lst
  )