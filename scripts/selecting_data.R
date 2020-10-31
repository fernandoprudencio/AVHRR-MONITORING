#' @title selecting NDVI data by stations
#' @author Fernando Prudencio
#'
rm(list = ls())

#' load packages
library(sf)
library(tidyverse)

#' load data (.RData)
load("data/rdata/ndvi_stations.RData")

#' read vector data
sf_station <- st_read(
  dsn = "data/vector/senamhi_weather_stations.gpkg",
  layer = "weather_stations_HIST"
) %>% mutate(cod = as.character(cod))

sf_basin <- st_read(
  dsn = "data/vector/limits.gpkg", layer = "peruvian_watersheds"
) %>% dplyr::filter(NOMBRE == "Cuenca Mantaro")

sf_points <- st_intersection(sf_station, sf_basin)

#' extract data
data <-
  tibble(
    date = seq(as.Date("1981-06-01"), as.Date("2019-12-01"), by = "1 month")
  ) %>%
  mutate(dplyr::select(df, sf_points$cod))

#' save data
write.csv(data, "data/tables/NDVI_Mantaro_stations.csv")