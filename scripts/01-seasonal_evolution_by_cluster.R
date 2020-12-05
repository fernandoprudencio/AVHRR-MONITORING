#' @title seasonal evolution of NDVI and rainfal by cluster
#' @author Fernando Prudencio
#'
rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "sf", "tidyverse", "Hmisc"
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
library(zoo)

#' 1| LOAD NDVI VALUES
load("data/rdata/ndvi_stations_MODIS.RData")

#' 2| CONSTANTS
k.cluster <- c(10, 12)
k.years.omit <- c(2005, 2010)

#' 3| SELECT STATION BY CLUSTER
sf.station <- st_read(
  dsn = "data/vector/senamhi_weather_stations.gpkg",
  layer = "weather_stations_HIST"
) %>%
  mutate(cod = as.character(cod)) %>%
  dplyr::filter(clus %in% k.cluster)

#' 4| OBTAIN NDVI VALUES BY STATIONS
ndvi.octday <-
  tibble(
    ndvi = dplyr::select(df.modis, sf.station$cod) %>%
      apply(1, FUN = function(x) mean(x, na.rm = T))
  ) %>%
  mutate(
    date = df.modis$date,
    ndvi = ndvi * .0001
  )
#'   4.1| calculate climatology
ndvi.clim <-
  dplyr::filter(
    ndvi.octday, date >= "2002-09-01" & date <= "2013-08-31"
  ) %>%
  dplyr::filter(
    !(
      (date >= "2005-09-01" & date <= "2006-08-31") |
        (date >= "2010-09-01" & date <= "2011-08-31")
    )
  ) %>%
  group_by(date = str_sub(date, 6, 10)) %>%
  summarise(ndvi.clim = mean(ndvi, na.rm = T)) %>%
  mutate(date = sprintf("2020-%1$s", date) %>% as.Date())

#'   4.2| calculate NDVI values in years of extreme drought
ndvi.dry <-
  dplyr::filter(
    ndvi.octday,
    (date >= "2005-09-01" & date <= "2006-08-31") |
      (date >= "2010-09-01" & date <= "2011-08-31")
  ) %>%
  group_by(date = str_sub(date, 6, 10)) %>%
  summarise(ndvi.dry = mean(ndvi, na.rm = T)) %>%
  mutate(date = sprintf("2020-%1$s", date) %>% as.Date())

#'   4.3| build table with normal and dry conditions
ndvi.table <-
  left_join(ndvi.clim, ndvi.dry, "date") %>%
  gather("ndvi.type", "ndvi.value", -date) %>%
  mutate(id = 1:n())

#'   4.4| calculate NDVI anomalies for full time series
# ndvi.anom <-
#   mutate(ndvi.octday, join = str_sub(date, 6, 10)) %>%
#   left_join(
#     mutate(ndvi.clim, join = str_sub(date, 6, 10)) %>%
#       dplyr::select(-date),
#     "join"
#   ) %>%
#   dplyr::select(-join) %>%
#   mutate(
#     anom = ndvi - ndvi.clim,
#     mavr = rollmean(anom, k = 5, fill = NA)
#   )

#' 5| LOAD RAINFALL DATA BY STATIONS
#'   5.1| obtain daily rainfall
pp.daily <-
  read.csv("data/tables/BD_with_filterQA/BD_Pp_Daily_new.csv", sep = ";") %>%
  dplyr::select(sf.station$cod) %>%
  apply(1, FUN = function(x) mean(x, na.rm = T)) %>%
  as_tibble() %>%
  mutate(
    date = seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by = "day")
  ) %>%
  rename("pp" = "value")

#'   5.2| obtain accumulated rainfall every eight days
for (i in 2002:2013) {
  vls.df <-
    pp.daily %>%
    dplyr::filter(
      date >= sprintf("%1$s-01-01", i) &
        date <= sprintf("%1$s-12-31", i)
    )

  vls.xts <- xts(vls.df$pp, order.by = vls.df$date)
  vls <- period.sum(vls.xts, endpoints(vls.xts, on = "days", k = 8)) %>%
    as.numeric()

  if (i == 2002) {
    pp.octday <- vls
  } else {
    pp.octday <- c(pp.octday, vls)
  }
}

pp.octday <- tibble(date = ndvi.octday$date[1:552], pp = pp.octday)

#'   5.3| calculate climatology
pp.clim <-
  dplyr::filter(
    pp.octday, date >= "2002-09-01" & date <= "2013-08-31"
  ) %>%
  dplyr::filter(
    !(
      (date >= "2005-09-01" & date <= "2006-08-31") |
        (date >= "2010-09-01" & date <= "2011-08-31")
    )
  ) %>%
  group_by(date = str_sub(date, 6, 10)) %>%
  summarise(pp.clim = mean(pp, na.rm = T)) %>%
  mutate(date = sprintf("2020-%1$s", date) %>% as.Date())

#'   5.4| calculate rainfall values in years of extreme drought
pp.dry <-
  dplyr::filter(
    pp.octday,
    (date >= "2005-09-01" & date <= "2006-08-31") |
      (date >= "2010-09-01" & date <= "2011-08-31")
  ) %>%
  group_by(date = str_sub(date, 6, 10)) %>%
  summarise(pp.dry = mean(pp, na.rm = T)) %>%
  mutate(date = sprintf("2020-%1$s", date) %>% as.Date())

#'   5.5| build table with normal and dry conditions
pp.table <-
  left_join(pp.clim, pp.dry, "date") %>%
  gather("pp.type", "pp.value", -date) %>%
  dplyr::select(-date) %>%
  mutate(id = 1:n())

#'   5.6| calculate NDVI anomalies for full time series
# pp.anom <-
#   mutate(pp.octday, join = str_sub(date, 6, 10)) %>%
#   left_join(
#     mutate(pp.clim, join = str_sub(date, 6, 10)) %>%
#       dplyr::select(-date),
#     "join"
#   ) %>%
#   dplyr::select(-join) %>%
#   mutate(
#     anom = pp - pp.clim,
#     mavr = rollmean(anom, k = 5, fill = NA)
#   )
#
# plot(pp.anom$date, pp.anom$anom, type = "l", col = "black")
# lines(pp.anom$date, pp.anom$mavr, type = "l", col = "blue")

#' 6| PLOT GRAPH
#'   6.1| build table to plot
df <-
  left_join(ndvi.table, pp.table, by = "id") %>%
  dplyr::select(-id) %>%
  mutate(
    time = ifelse(
      str_sub(date, 6, 7) %in% sprintf("%02d", 9:12),
      sprintf("2020-%1$s", str_sub(date, 6, 10)),
      sprintf("2021-%1$s", str_sub(date, 6, 10))
    ),
    time = as.Date(time)
  ) %>%
  dplyr::select(-date) %>%
  rename("date" = "time") %>%
  arrange(ndvi.type, date)

#'   6.2| plot graph
#'     6.2.1| homogenize the primary and secondary axis
ylim.prim <- c(min(df$pp.value, na.rm = T), max(df$pp.value, na.rm = T))
ylim.sec <- c(min(df$ndvi.value, na.rm = T), max(df$ndvi.value, na.rm = T))
#'     6.2.2| conversion coefficient"
n <- diff(ylim.prim) / diff(ylim.sec)
m <- ylim.prim[1] - n * (ylim.sec[1])

ggplot(df) +
  labs(y = "[mm]") +
  geom_bar(
    aes(x = date, y = pp.value, fill = pp.type),
    position = "identity", stat = "identity", alpha = 0.6,
    colour = rgb(207, 215, 247, maxColorValue = 255)
  ) +
  scale_fill_manual(
    values = c(
      rgb(39, 107, 255, maxColorValue = 255),
      rgb(207, 215, 247, maxColorValue = 255)
    ),
    labels = c("normal", "dry")
  ) +
  geom_line(
    aes(
      x = date,
      y = m + ndvi.value * n,
      linetype = ndvi.type,
      color = ndvi.type,
      size = ndvi.type
    )
  ) +
  # geom_point(aes(shape = type, color = type), size = 2) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = c("normal", "dry")
  ) +
  scale_color_manual(
    values = c(
      rgb(237, 28, 36, maxColorValue = 255),
      rgb(14, 149, 7, maxColorValue = 255)
    ),
    labels = c("normal", "dry")
  ) +
  scale_size_manual(
    values = c(1, 1), labels = c("normal", "dry")
  ) +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%b",
    expand = expansion(mult = c(.01, .01))
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ (. - m) / n, name = "index"),
    breaks = seq(0, 80, 10),
    limits = c(0, 80),
    expand = expansion(mult = c(.02, 0))
  ) +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin = margin(3, 7, 7, 7),
    # legend.key.size = unit(.8, "cm"),
    legend.key.width = unit(1.6, "cm"),
    legend.key.height = unit(1.1, "cm"),
    legend.position = c(0.77, 0.78),
    legend.title = element_blank(),
    legend.text = element_text(size = 15, family = "Source Sans Pro"),
    plot.title = element_text(size = 15, hjust = .5, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 12, colour = "black", family = "Source Sans Pro",
      face = "bold", angle = 0, vjust = .6
    ),
    axis.text.y = element_text(
      size = 13, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      face = "bold", family = "Source Sans Pro", color = "black", size = 20
    ),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm"),
    axis.line.y = element_line(
      size = .8, color = "black"
    )
  )