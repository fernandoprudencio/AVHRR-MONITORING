#' @title
#' monthly NDVI vs E index and C index
#'
#' @description
#' This script plots monthly NDVI vs. historic series of C index and
#'   E index from 1981 to 2018
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "sf", "griExtra", "tidyverse", "ggthemes", "grid", "gridExtra"
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
# library(ggthemes)
# library(grid)
# library(gridExtra)
library(scales)

#' LOAD DATA
load("data/rdata/ndvi_stations.RData")

#' CHANGE TO ENGLISH LANGUAGE
Sys.setlocale(category = "LC_ALL", locale = "english")

#' TYPE CLUSTER NUMBER
cluster <- 16

#' READ VECTOR DATA
station <-
  st_read(
    dsn = "data/vector/senamhi_weather_stations.gpkg",
    layer = "weather_stations_HIST"
  ) %>% dplyr::filter(clus == cluster)

#' NDVI
ndvi <-
  tibble(
    ndvi = dplyr::select(df, station$cod) %>%
      apply(1, FUN = function(x) mean(x, na.rm = T))
  ) %>%
  mutate(
    date = seq(as.Date("1981-06-01"), as.Date("2019-12-01"), by = "1 month"),
    ndvi = ndvi * .0001
  )

#' INDEX
ec.index <- read.csv(
  "data/tables/EC_Index/EC_index.csv",
  header = T, sep = ";"
) %>%
  as_tibble() %>%
  mutate(date = sprintf("%1$s-%2$s-01", yy, mm) %>% as.Date()) %>%
  dplyr::select(date, E, C) %>%
  dplyr::filter(date >= "1981-06-01")

#' JOIN DATA
table <- left_join(ec.index, ndvi, by = "date") %>%
  gather("type", "value", -date, -ndvi)

#' HOMOGENIZE THE PRIMARY AND SECONDARY AXIS
ylim.prim <- c(min(table$ndvi, na.rm = T), max(table$ndvi, na.rm = T))
ylim.sec <- c(min(table$value, na.rm = T), max(table$value, na.rm = T))

#' CONVERSION COEFFICIENT
n <- diff(ylim.prim) / diff(ylim.sec)
m <- ylim.prim[1] - n * (ylim.sec[1])

plt <- ggplot(table, aes(x = date, y = ndvi)) +
  geom_line(
    colour = rgb(0, 117, 5, maxColorValue = 255),
    alpha = 1, linetype = "solid", size = .2
  ) +
  labs(y = "NDVI", subtitle = sprintf("Cluster %1$s", cluster)) +#, tag = "a)"
  geom_line(
    aes(
      y = m + value * n,
      linetype = type,
      color = type,
      size = type
    )
  ) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = c("C index", "E index")
  ) +
  scale_color_manual(
    values = c("black", "deepskyblue3"),
    labels = c("C index", "E index")
  ) +
  scale_size_manual(
    values = c(.5, .5),
    labels = c("C index", "E index")
  ) +
  geom_hline(
    yintercept = m + 0 * n,
    linetype = "dashed", color = "gray", size = .8
  ) +
  scale_x_date(
    limits = c(as.Date("1981-01-01"), as.Date("2018-12-01")),
    labels = date_format("%Y"),
    breaks = "1 year", expand = c(0, 0)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ (. - m) / n, name = "index"),
    expand = c(0, 0)
  ) +
  annotate(
    "rect",
    xmin = as.Date("2016-06-01"),
    xmax = as.Date("2016-12-01"),
    ymin = ylim.prim[1], ymax = ylim.prim[2],
    alpha = .5, fill = "gray"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2010-07-01"),
    xmax = as.Date("2010-12-01"),
    ymin = ylim.prim[1], ymax = ylim.prim[2],
    alpha = .5, fill = "gray"
  ) +
  annotate(
    "rect",
    xmin = as.Date("2005-06-01"),
    xmax = as.Date("2005-12-01"),
    ymin = ylim.prim[1], ymax = ylim.prim[2],
    alpha = .5, fill = "gray"
  ) +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin = margin(3, 7, 7, 7),
    legend.key.width = unit(.9, "cm"),
    legend.key.height = unit(.4, "cm"),
    legend.position = c(.32, .87),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "Source Sans Pro"),
    plot.subtitle = element_text(
      family = "Source Sans Pro", face = "bold", size = 15
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 15, family = "Source Sans Pro"
    ),
    axis.text.x = element_text(
      size = 11, hjust = -.2, angle = 0, family = "Source Sans Pro"
    ),
    axis.text.y = element_text(
      size = 11, family = "Source Sans Pro"
    ),
    panel.grid = element_blank(),
    plot.tag = element_text(size = 25, family = "Source Sans Pro")
  )

# SAVE PLOT
ggsave(
  plt,
  filename = sprintf("exports/NDVI_vs_EC-index_cluster%s.png", cluster),
  width = 40, height = 10, units = "cm", dpi = 1000
)