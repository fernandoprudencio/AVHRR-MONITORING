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
library(mapview)

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
  mutate(cod = as.character(cod)) %>%
  filter(clus == 1)

for (i in 1:3) { # npts(station)
  #' ee VECTORIAL OBJECT
  ee_station <- station[i, ] %>%
    st_geometry() %>%
    sf_as_ee()

  #' BUILD TYPE OF FILTER
  #bit1 <- ee$Number(2)$pow(1)$int()

  #' MASK NDVI DATASET
  ndvi_mak <- ee$ImageCollection("NOAA/CDR/AVHRR/NDVI/V5")$
    filterDate("2019-01-01", "2019-12-31")$
    select("NDVI")$
    filter(ee$Filter$calendarRange(1, 12, field = "month"))#$
    #map(function(x) qaFilter(x, bit1))

  #' EXTRACT TIME SERIES
  ts <- ee_extract(ndvi_mak, ee_station, fun = ee$Reducer$mean()) %>%
    as_tibble() %>%
    gather() %>%
    rename("date" = "key") %>%
    mutate(
      date = str_sub(date, 2, 9) %>% as.Date(format = "%Y%m%d")
    )

  #' BUILD TABLE TO WRITE TIME SERIES
  if (i == 1) {
    df.ts <- tibble(
      date = seq(as.Date("1980-01-01"), as.Date("2019-12-31"), by = "1 day")
    ) %>%
      left_join(ts, by = "date")
  } else {
    df.ts <- df.ts %>% left_join(ts, by = "date")
  }
}

x <- df.ts %>%
  group_by(str_sub(date, 1, 7)) %>%
  summarise_all(.funs = function(x) mean(x, na.rm = T))

plot(df.ts$date, df.ts$value, type = "l", col = "black")
lines(df.ts$date, df.ts$value.y, type = "l", col = "red")
lines(df.ts$date, df.ts$value, type = "l", col = "blue")
#