#' @title
#' calculate NDVI anomalies
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
lst.month <-
  list.files("data/raster/ndvi/monthly", ".tif", full.names = T)[1:204]

lst.clim <-
  list.files("data/raster/ndvi/climatology", ".tif", full.names = T)

#' BUILD A FUNCTION TO CALCULATE CLIMATOLOGY
fun.anom <- function(month, data) {
  id.month <- filter(df, str_sub(date, 6, 7) == month)
  ndvi.clim <- raster(lst.clim[as.numeric(month)])
  
  for (i in id.month$id) {
    anom <- raster(data[i]) - ndvi.clim
    name <- basename(data[i]) %>% str_sub(-11, -5)
    
    writeRaster(
      anom/10000, overwrite = T,
      filename =
        sprintf(
          "data/raster/ndvi/anomalies/MOD09A1.006_anom_NDVI_doy%1s.tif", name
        )
    )
  }
}

#' APPLY fun.anom() FUNCTION
grd.clim <-
  sapply(
    sprintf("%02d", 1:12),
    FUN = fun.anom,
    data = lst.month
  )