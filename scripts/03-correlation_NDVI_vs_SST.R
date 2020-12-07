#' @title
#' correlation between monthly NDVI and C index
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("sf", "tidyverse", "zoo")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

library(tidyverse)
library(sf)
library(zoo)

#' 1| LOAD DATA
load("data/rdata/ndvi_stations_MODIS.RData")

#' 2| C INDEX
c.monthly <- read.csv(
  "data/tables/EC_Index/EC_index.csv",
  header = T, sep = ";"
) %>%
  as_tibble() %>%
  mutate(date = sprintf("%1$s-%2$s-01", yy, mm) %>% as.Date()) %>%
  dplyr::select(date, C) %>%
  dplyr::filter(date >= "2002-01-01" & date <= "2013-12-31")

#'   2.1| calculate climatology
index.clim <-
  dplyr::filter(
    c.monthly,
    date >= "2002-09-01" & date <= "2013-08-31",
    !(
      (date >= "2005-09-01" & date <= "2006-08-31") |
        (date >= "2010-09-01" & date <= "2011-08-31")
    )
  ) %>%
  group_by(date = str_sub(date, 6, 7)) %>%
  summarise(index.clim = mean(C, na.rm = T)) %>%
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "1 month"))

#'   2.2| calculate NDVI values in years of extreme drought (2005)
index.dry05 <-
  dplyr::filter(
    c.monthly,
    date >= "2005-09-01" & date <= "2006-08-31"
  ) %>%
  group_by(date = str_sub(date, 6, 7)) %>%
  summarise(index.dry = mean(C, na.rm = T)) %>%
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "1 month"))

#'   2.3| calculate NDVI values in years of extreme drought (2010)
index.dry10 <-
  dplyr::filter(
    c.monthly,
    date >= "2010-09-01" & date <= "2011-08-31"
  ) %>%
  group_by(date = str_sub(date, 6, 7)) %>%
  summarise(index.dry = mean(C, na.rm = T)) %>%
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "1 month"))

#' 3| SELECT NDVI VALUES FOR PERIOD 2002 - 2013
ndvi.octday <- dplyr::filter(df.modis, date <= "2013-12-31")
station.name <- names(dplyr::select(df.modis, -date))

#'   3.1| calculate monthly NDVI
ndvi.monthly <-
  group_by(ndvi.octday, date = str_sub(date, 1, 7)) %>%
  summarise_all(.funs = function(x) mean(x, na.rm = T)) %>%
  mutate(date = as.Date(sprintf("%1$s-01", date)))

#'   3.2| calculate climatology
for (i in station.name) {
  print(i)
  vls.clim <-
    dplyr::select(ndvi.monthly, i, date) %>%
    dplyr::filter(
      date >= "2002-09-01" & date <= "2013-08-31",
      !(
        (date >= "2005-09-01" & date <= "2006-08-31") |
          (date >= "2010-09-01" & date <= "2011-08-31")
      )
    ) %>%
    rename("ndvi" = i) %>%
    group_by(date = str_sub(date, 6, 7)) %>%
    summarise(ndvi.clim = mean(ndvi, na.rm = T))
  
  if (i == station.name[1]) {
    ndvi.clim <- tibble(vls.clim$ndvi.clim)
  } else {
    ndvi.clim <- cbind(ndvi.clim, vls.clim$ndvi.clim)
  }
}

names(ndvi.clim) <- station.name

ndvi.clim <-
  as_tibble(ndvi.clim) %>%
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "1 month"))

#'   3.3| calculate NDVI values in years of extreme drought (2005)
for (i in station.name) {
  print(i)
  vls.dry05 <-
    dplyr::select(ndvi.monthly, i, date) %>%
    dplyr::filter(
      date >= "2005-09-01" & date <= "2006-08-31"
    ) %>%
    rename("ndvi" = i) %>%
    group_by(date = str_sub(date, 6, 7)) %>%
    summarise(ndvi.dry = mean(ndvi, na.rm = T))
  
  if (i == station.name[1]) {
    ndvi.dry05 <- tibble(vls.dry05$ndvi.dry)
  } else {
    ndvi.dry05 <- cbind(ndvi.dry05, vls.dry05$ndvi.dry)
  }
}

names(ndvi.dry05) <- station.name

ndvi.dry05 <-
  as_tibble(ndvi.dry05) %>%
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "1 month"))

#'   3.4| calculate NDVI values in years of extreme drought (2010)
for (i in station.name) {
  print(i)
  vls.dry10 <-
    dplyr::select(ndvi.monthly, i, date) %>%
    dplyr::filter(
      date >= "2010-09-01" & date <= "2011-08-31"
    ) %>%
    rename("ndvi" = i) %>%
    group_by(date = str_sub(date, 6, 7)) %>%
    summarise(ndvi.dry = mean(ndvi, na.rm = T))
  
  if (i == station.name[1]) {
    ndvi.dry10 <- tibble(vls.dry10$ndvi.dry)
  } else {
    ndvi.dry10 <- cbind(ndvi.dry10, vls.dry10$ndvi.dry)
  }
}

names(ndvi.dry10) <- station.name

ndvi.dry10 <-
  as_tibble(ndvi.dry10) %>%
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "1 month"))

#' 4| CORRELATION
for (i in station.name) {
  vls.ndvi.clim <- dplyr::select(ndvi.clim, i) %>% rename("ndvi" = i)
  dt.clim <- tibble(vls.ndvi.clim, index = index.clim$index.clim) %>% drop_na()
  vls.cor.clim <- cor(dt.clim$ndvi, dt.clim$index)
  
  vls.ndvi.dry05 <- dplyr::select(ndvi.dry05, i) %>% rename("ndvi" = i)
  dt.dry05 <- tibble(vls.ndvi.dry05, index = index.dry05$index.dry) %>% drop_na()
  vls.cor.dry05 <- cor(dt.dry05$ndvi, dt.dry05$index)
  
  vls.ndvi.dry10 <- dplyr::select(ndvi.dry10, i) %>% rename("ndvi" = i)
  dt.dry10 <- tibble(vls.ndvi.dry10, index = index.dry10$index.dry) %>% drop_na()
  vls.cor.dry10 <- cor(dt.dry10$ndvi, dt.dry10$index)
  
  if (i == station.name[1]) {
    cor.clim <- vls.cor.clim
    cor.dry05 <- vls.cor.dry05
    cor.dry10 <- vls.cor.dry10
  } else {
    cor.clim <- c(cor.clim, vls.cor.clim)
    cor.dry05 <- c(cor.dry05, vls.cor.dry05)
    cor.dry10 <- c(cor.dry10, vls.cor.dry10)
  }
}

df.cor <- tibble(
  cod = station.name,
  cor.sst.clim = cor.clim,
  cor.sst.dry05 = cor.dry05,
  cor.st.dry10 = cor.dry10
)

#' 5| LOAD VETOR DATA
sf.station <-
  st_read(
    dsn = "data/vector/senamhi_weather_stations.gpkg",
    layer = "weather_stations_HIST"
  ) %>%
  mutate(cod = as.character(cod)) %>%
  left_join(df.cor, "cod")

st_write(
  dsn = "data/vector/senamhi_weather_stations.gpkg",
  obj = sf.station, layer = "weather_stations_HIST_cor_NDVIvsSST"
)