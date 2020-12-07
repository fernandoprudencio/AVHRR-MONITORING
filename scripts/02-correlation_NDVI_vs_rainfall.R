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
      install.packages(x)
    }
  }
)

library(tidyverse)
library(sf)
library(zoo)

#' 1| LOAD DATA
load("data/rdata/ndvi_stations_MODIS.RData")

#' 2| SELECT NDVI VALUES FOR PERIOD 2002 - 2013
ndvi.octday <- dplyr::filter(df.modis, date <= "2013-12-31")
station.name <- names(dplyr::select(df.modis, -date))

#'   2.1| calculate climatology
for (i in station.name) {
  print(i)
  vls.clim <-
    dplyr::select(ndvi.octday, i, date) %>%
    dplyr::filter(
      date >= "2002-09-01" & date <= "2013-08-31",
      !(
        (date >= "2005-09-01" & date <= "2006-08-31") |
          (date >= "2010-09-01" & date <= "2011-08-31")
      )
    ) %>%
    rename("ndvi" = i) %>%
    group_by(date = str_sub(date, 6, 10)) %>%
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
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "8 day"))

#'   2.2| calculate NDVI values in years of extreme drought (2005)
for (i in station.name) {
  print(i)
  vls.dry05 <-
    dplyr::select(ndvi.octday, i, date) %>%
    dplyr::filter(
      date >= "2005-09-01" & date <= "2006-08-31"
    ) %>%
    rename("ndvi" = i) %>%
    group_by(date = str_sub(date, 6, 10)) %>%
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
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "8 day"))

#'   2.3| calculate NDVI values in years of extreme drought (2010)
for (i in station.name) {
  print(i)
  vls.dry10 <-
    dplyr::select(ndvi.octday, i, date) %>%
    dplyr::filter(
      date >= "2010-09-01" & date <= "2011-08-31"
    ) %>%
    rename("ndvi" = i) %>%
    group_by(date = str_sub(date, 6, 10)) %>%
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
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "8 day"))

#' 3| LOAD RAINFALL DATA BY STATIONS
#'   3.1| obtain daily rainfall
pp.daily <-
  read.csv("data/tables/BD_with_filterQA/BD_Pp.csv", sep = ";") %>%
  dplyr::select(station.name, date) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  dplyr::filter(date >= "2002-01-01" & date <= "2013-12-31")

pp.daily[pp.daily == -99.9] = NA

#'   3.2| obtain accumulated rainfall every eight days
for (i in station.name) {
  print(i)

  for (j in 2002:2013) {
    vls.df <-
      pp.daily %>%
      dplyr::select(date, i) %>%
      dplyr::filter(
        date >= sprintf("%1$s-01-01", j) &
          date <= sprintf("%1$s-12-31", j)
      ) %>%
      rename("pp" = i)

    vls.xts <- xts(vls.df$pp, order.by = vls.df$date)
    vls <- period.sum(vls.xts, endpoints(vls.xts, on = "days", k = 8)) %>%
      as.numeric()

    if (j == 2002) {
      vls.octday <- vls
    } else {
      vls.octday <- c(vls.octday, vls)
    }
  }

  if (i == "qc00000807") {
    pp.octday <- tibble(vls.octday)
  } else {
    pp.octday <- cbind(pp.octday, vls.octday)
  }
}

names(pp.octday) <- station.name

pp.octday <- mutate(pp.octday, date = ndvi.octday$date)

#'   3.3| calculate climatology
for (i in station.name) {
  print(i)
  vls.clim <-
    dplyr::select(pp.octday, i, date) %>%
    dplyr::filter(
      date >= "2002-09-01" & date <= "2013-08-31",
      !(
        (date >= "2005-09-01" & date <= "2006-08-31") |
          (date >= "2010-09-01" & date <= "2011-08-31")
      )
    ) %>%
    rename("pp" = i) %>%
    group_by(date = str_sub(date, 6, 10)) %>%
    summarise(pp.clim = mean(pp, na.rm = T))
  
  if (i == station.name[1]) {
    pp.clim <- tibble(vls.clim$pp.clim)
  } else {
    pp.clim <- cbind(pp.clim, vls.clim$pp.clim)
  }
}

names(pp.clim) <- station.name

pp.clim <-
  as_tibble(pp.clim) %>%
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "8 day"))

#'   3.4| calculate rainfall values in years of extreme drought (2005)
for (i in station.name) {
  print(i)
  vls.dry05 <-
    dplyr::select(pp.octday, i, date) %>%
    dplyr::filter(
      date >= "2005-09-01" & date <= "2006-08-31"
    ) %>%
    rename("pp" = i) %>%
    group_by(date = str_sub(date, 6, 10)) %>%
    summarise(pp.dry = mean(pp, na.rm = T))
  
  if (i == station.name[1]) {
    pp.dry05 <- tibble(vls.dry05$pp.dry)
  } else {
    pp.dry05 <- cbind(pp.dry05, vls.dry05$pp.dry)
  }
}

names(pp.dry05) <- station.name

pp.dry05 <-
  as_tibble(pp.dry05) %>%
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "8 day"))

#'   3.5| calculate rainfall values in years of extreme drought (2010)
for (i in station.name) {
  print(i)
  vls.dry10 <-
    dplyr::select(pp.octday, i, date) %>%
    dplyr::filter(
      date >= "2010-09-01" & date <= "2011-08-31"
    ) %>%
    rename("pp" = i) %>%
    group_by(date = str_sub(date, 6, 10)) %>%
    summarise(pp.dry = mean(pp, na.rm = T))
  
  if (i == station.name[1]) {
    pp.dry10 <- tibble(vls.dry10$pp.dry)
  } else {
    pp.dry10 <- cbind(pp.dry10, vls.dry10$pp.dry)
  }
}

names(pp.dry10) <- station.name

pp.dry10 <-
  as_tibble(pp.dry10) %>%
  mutate(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), "8 day"))

#' 4| CORRELATION
for (i in station.name) {
  vls.ndvi.clim <- dplyr::select(ndvi.clim, i) %>% rename("ndvi" = i)
  vls.pp.clim <- dplyr::select(pp.clim, i) %>% rename("pp" = i)
  dt.clim <- tibble(vls.ndvi.clim, vls.pp.clim) %>% drop_na()
  vls.cor.clim <- cor(dt.clim$ndvi, dt.clim$pp)

  vls.ndvi.dry05 <- dplyr::select(ndvi.dry05, i) %>% rename("ndvi" = i)
  vls.pp.dry05 <- dplyr::select(pp.dry05, i) %>% rename("pp" = i)
  dt.dry05 <- tibble(vls.ndvi.dry05, vls.pp.dry05) %>% drop_na()
  vls.cor.dry05 <- cor(dt.dry05$ndvi, dt.dry05$pp)

  vls.ndvi.dry10 <- dplyr::select(ndvi.dry10, i) %>% rename("ndvi" = i)
  vls.pp.dry10 <- dplyr::select(pp.dry10, i) %>% rename("pp" = i)
  dt.dry10 <- tibble(vls.ndvi.dry10, vls.pp.dry10) %>% drop_na()
  vls.cor.dry10 <- cor(dt.dry10$ndvi, dt.dry10$pp)
  
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
  cor.ndvi.clim = cor.clim,
  cor.ndvi.dry05 = cor.dry05,
  cor.ndvi.dry10 = cor.dry10
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
  obj = sf.station, layer = "weather_stations_HIST_cor_NDVI"
)