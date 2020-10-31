#' @title this script donwloads times series of NDVI for Senamhi's stations
#'   location
#' @author Fernando Prudencio
#'

rm(list = ls())

#' LOAD PACKAGES
library(raster)
library(sf)
library(rgee)
library(tidyverse)

#' INITIALIZE GEE
ee_Initialize(drive = TRUE)

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' LOAD STATIONS AVAILABLE
load("data/rdata/available_rainfall_data.RData")

#' LOAD VECTORIAL DATA
#'   sf object
station <- st_read(
  dsn = "data/vector/senamhi_weather_stations.gpkg",
  layer = "weather_stations_HIST"
) %>%
  filter(cod %in% df.pp$cod) %>%
  mutate(cod = as.character(cod))
#'   ee object
ee_station <- station %>%
  st_geometry() %>%
  sf_as_ee()

#' BUILD TYPE OF FILTER
# bit1 <- ee$Number(2)$pow(1)$int()

#' MASK NDVI DATASET
ndvi_mak <- ee$ImageCollection("NOAA/CDR/AVHRR/NDVI/V5")$select("NDVI")
# map(function(x) qaFilter(x, bit1))

#' EXTRACT TIME SERIES
period <- seq(as.Date("1981-06-01"), as.Date("2019-12-01"), by = "1 month")
ts <- sapply(period, FUN = ts.extract, images = ndvi_mak, points = ee_station)

#' BUILD TABLE TO WRITE TIME SERIES
df <- t(as.data.frame(ts)) %>% as_tibble()
#'   rename fields
names(df) <- station$cod
#'   save dataframe
save(df, file = "data/rdata/ndvi_stations.RData")