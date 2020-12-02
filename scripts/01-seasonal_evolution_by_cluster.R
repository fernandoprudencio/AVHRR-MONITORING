#' @title seasonal evolution by cluster
#' @author Fernando Prudencio
#'
rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "sf", "tidyverse"
)

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
library(sf)
library(tidyverse)
library(Hmisc)

#' LOAD NDVI VALUES
load("data/rdata/ndvi_stations_AVHRR.RData")

#' CONSTANTS
k.cluster <- c(10, 12)
k.cluster <- 12
k.years.omit = c(2005, 2010, 2016)

#' SELECT STATION BY CLUSTER
sf.station <- st_read(
  dsn = "data/vector/senamhi_weather_stations.gpkg",
  layer = "weather_stations_HIST"
) %>%
  mutate(cod = as.character(cod)) %>%
  dplyr::filter(clus %in% k.cluster)

#' SELECT NDVI VALUES BY STATIONS
ndvi <-
  tibble(
    ndvi = dplyr::select(df, sf.station$cod) %>%#sf.station$cod  "qc00000635"
      apply(1, FUN = function(x) mean(x, na.rm = T))
  ) %>%
  mutate(
    date = seq(as.Date("1981-06-01"), as.Date("2019-12-01"), by = "1 month"),
    ndvi = ndvi * .0001
  )

normal.ndvi <-
  dplyr::filter(ndvi, str_sub(date, 1, 4) %nin% k.years.omit) %>%#str_sub(date, 1, 4) %nin% k.years.omit
  group_by(date = str_sub(date, 6, 7)) %>%
  summarise(ndvi = mean(ndvi, na.rm = T))

dry.ndvi <-
  dplyr::filter(ndvi, str_sub(date, 1, 4) %in% k.years.omit) %>%
  group_by(date = str_sub(date, 6, 7)) %>%
  summarise(ndvi = mean(ndvi, na.rm = T))

dry.2005 <-
  dplyr::filter(ndvi, str_sub(date, 1, 4) == 2005)
dry.2010 <-
  dplyr::filter(ndvi, str_sub(date, 1, 4) == 2010)
dry.2016 <-
  dplyr::filter(ndvi, str_sub(date, 1, 4) == 2016)


  plot(dry.2005$ndvi, type = "l", col = "red")
  lines(normal.ndvi$ndvi, type = "l", col = "black")
  # lines(dry.ndvi$ndvi, type = "l", col = "red")
  lines(dry.2010$ndvi, type = "l", col = "blue")
  lines(dry.2016$ndvi, type = "l", col = "green")
  
