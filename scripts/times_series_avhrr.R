#' @title this script donwloads times series of NDVI for Senamhi's stations
#' location
#' @author Fernando Prudencio

rm(list = ls())

#' LOAD PACKAGES
library(raster)
library(sf)
library(rgee)
library(tidyverse)
# library(DescTools)
# library(Hmisc)
# library(cptcity)

#' INITIALIZE GEE
ee_Initialize(drive = TRUE)

#' LOAD VECTORIAL DATA
station <- st_read("")

#' BUILD TYPE OF FILTER
bit1 <- ee$Number(2)$pow(1)$int()

#' SELECT DATASET SOURCE
latlon <- ee$Geometry$Point(-69.96, -12.84)
date()
ndvi_mak1 <- ee$ImageCollection("NOAA/CDR/AVHRR/NDVI/V5")$
  select("NDVI")$
  filterDate("1980-01-01", "2019-12-31")$
  filter(ee$Filter$calendarRange(1, field = "month"))
date()
ndvi_mak2 <- ee$ImageCollection("NOAA/CDR/AVHRR/NDVI/V5")$
  filterDate("1980-01-01", "2019-12-31")$
  filter(ee$Filter$calendarRange(1, field = "month"))$
  map(qaFilter)
date()

date()
ts <- ee_extract(ndvi_mak1, latlon, fun = ee$Reducer$mean()) %>% gather()
date()
ts2 <- ee_extract(ndvi_mak2, latlon, fun = ee$Reducer$mean()) %>% gather()
date()

x <- ts$value
x[x < 2000] <- NA
ts %>% left_join()

plot(x, type = "l")
#



# bit2 <- ee$Number(2)$pow(2)$int()
# qa_mask <- qa$bitwiseAnd(bit1)$eq(0)$
#   And(qa$bitwiseAnd(bit2)$eq(0))