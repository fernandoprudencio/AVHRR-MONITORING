#' @title seasonal evolution of NDVI and rainfal by cluster
#' @author Fernando Prudencio
#'
rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("sf", "tidyverse", "Hmisc", "zoo", "magick")

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
library(magick)

#' 1| LOAD NDVI VALUES
load("data/rdata/ndvi_stations_MODIS.RData")

#' 2| CONSTANTS
k.cluster <- c(6)
# k.years.omit <- c(2005, 2010)

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
# ylim.prim <- c(min(df$pp.value, na.rm = T), max(df$pp.value, na.rm = T))
ylim.prim <- c(0, 80)
# ylim.sec <- c(min(df$ndvi.value, na.rm = T), max(df$ndvi.value, na.rm = T))
ylim.sec <- c(.1, .8)
#'     6.2.2| conversion coefficient"
n <- diff(ylim.prim) / diff(ylim.sec)
m <- ylim.prim[1] - n * (ylim.sec[1])
#'     6.2.3| plot
plt <- ggplot(df) +
  labs(y = "[mm]", subtitle = "Region: Altiplano") +
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
    labels = c("normal\nyears\nrainfall", "dry\nyears\nrainfall")
  ) +
  geom_line(
    aes(
      x = date,
      y = m + ndvi.value * n,
      linetype = ndvi.type,
      color = ndvi.type
    ), size = .9
  ) +
  geom_point(
    aes(
      x = date, y = m + ndvi.value * n, color = ndvi.type
    ),
    size = 1.3
  ) +
  scale_linetype_manual(
    values = c("solid", "solid"),
    labels = c("normal\nyears\nndvi", "dry\nyears\nndvi")
  ) +
  scale_color_manual(
    values = c(
      rgb(14, 149, 7, maxColorValue = 255),
      rgb(131, 201, 128, maxColorValue = 255)
    ),
    labels = c("normal\nyears\nndvi", "dry\nyears\nndvi")
  ) +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%b",
    expand = expansion(mult = c(.008, .002))
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ (. - m) / n, name = "index", breaks = seq(.1, .8, .1)
    ),
    breaks = seq(0, 80, 10),
    limits = c(0, 80),
    expand = expansion(mult = c(.02, 0))
  ) +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black", size = 0),
    legend.margin = margin(3, 7, 7, 7),
    legend.key.size = unit(.4, "cm"),
    # legend.key.width = unit(.5, "cm"),
    # legend.key.height = unit(.3, "cm"),
    legend.spacing.y = unit(.1, "cm"),
    legend.position = c(.08, .76),
    legend.title = element_blank(),
    legend.text = element_text(size = 6, family = "Source Sans Pro"),
    plot.subtitle = element_text(size = 12, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 12, colour = "black", family = "Source Sans Pro",
      face = "bold", angle = 0, vjust = .6, hjust = -.27
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
#'     6.2.4| plot
name <- "exports/rainfall_vs_NDVI_northern_Andes.png"
name <- "exports/rainfall_vs_NDVI_central_Andes.png"
name <- "exports/rainfall_vs_NDVI_southern_Andes.png"
name <- "exports/rainfall_vs_NDVI_Altiplano.png"
ggsave(
  plot = plt, name, width = 16, height = 12, units = "cm", dpi = 1000
)
#'     6.2.5| trim figure
img <- magick::image_read(name, strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#'     6.2.6| save figure
image_write(img, path = name, format = "png")