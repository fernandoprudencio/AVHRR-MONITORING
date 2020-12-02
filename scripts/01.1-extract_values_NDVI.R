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
library(raster)

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

#' Extract values
df.modis <- sapply(
  list.files("data/raster/ndvi", ".tif", full.names = T),
  FUN = function(x) raster::extract(raster(x), sf.station)
) %>% t() %>%
  as_tibble()

names(df.modis) <- sf.station$cod

#' SAVE EXTRACTED VALUES
save(df.modis, file = "data/rdata/ndvi_stations_MODIS.RData")