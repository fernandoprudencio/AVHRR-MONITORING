#' @title
#' calculate and plot NDVI anomalies
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

#' LOAD FUNCTIONS
source("scripts/functions.R", echo = F)

#' 1| LOAD DATA
load("data/rdata/ndvi_stations_MODIS.RData")

#' 2| CONSTANTS
k.cluster <- list(c(10, 12), c(1, 2, 4), c(7, 8), c(6))

#' 3| CALCULATE ANOMALIES
# x <- fun.anom(k.cluster[[1]], df.modis)
df <- lapply(k.cluster, FUN = fun.anom, df.modis) %>%
  as.data.frame() %>%
  as.tibble() %>%
  mutate(
    date = seq(as.Date("2002-01-01"), as.Date("2018-12-01"), by = "month")
  )

names(df)[1:4] <-
  c("Andes_North", "Andes_Central", "Andes_South", "Altiplano")

#' 4| BUILD TABLE BEFORE PLOT ANOMALIES BY CLUSTER
table <- gather(df, key = "type", value = "value", -date)

#' 5| PLOT
labels <- c("Altiplano", "Central Andes", "Northern Andes", "Southern Andes")
plt <- ggplot(table) +
  labs(y = "anomalies", subtitle = "NDVI anomalies (2002-2018)") +
  geom_line(
    aes(
      x = date, y = value,
      linetype = type,
      color = type,
      size = type
    )
  ) +
  scale_linetype_manual(
    values = c("solid", "solid", "solid", "solid"), labels = labels
  ) +
  scale_color_manual(
    values = c("black", "gray", "blue", "red"), labels = labels
  ) +
  scale_size_manual(
    values = c(.5, .5, .5, .5), labels = labels
  ) +
  geom_hline(
    yintercept = 0, color = "black", size = .3
  ) +
  geom_hline(
    yintercept = .05, linetype = "dashed", color = "black", size = .3
  ) +
  geom_hline(
    yintercept = -.05, linetype = "dashed", color = "black", size = .3
  ) +
  scale_x_date(
    limits = c(as.Date("2002-01-01"), as.Date("2018-12-01")),
    date_labels = "%y",
    breaks =
      seq(as.Date("2002-01-01"), as.Date("2018-12-01"), by = "year"),#"1 year"
    expand = c(.03, .03)
  ) +
  scale_y_continuous(
    breaks = seq(-.1, .1, .05),
    limits = c(-.12, .12),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black", size = .3),
    legend.margin = margin(1, 7, 7, 7),
    legend.key.width = unit(.5, "cm"),
    legend.key.height = unit(.2, "cm"),
    legend.position = c(.145, .88),
    legend.title = element_blank(),
    legend.text = element_text(size = 9, family = "Source Sans Pro"),
    plot.subtitle = element_text(
      family = "Source Sans Pro", size = 10,
      hjust = .97, vjust = -9
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 15, family = "Source Sans Pro"
    ),
    axis.ticks.length.x = unit(-.15, "cm"),
    axis.text.x = element_text(
      size = 11, hjust = -.5, vjust = 5, angle = 0, family = "Source Sans Pro"
    ),
    axis.text.y = element_text(
      size = 11, family = "Source Sans Pro"
    ),
    panel.grid = element_blank(),
    plot.tag = element_text(size = 25, family = "Source Sans Pro")
  )

# SAVE PLOT
ggsave(
  plt, filename = "exports/NDVI_anomalies.png",
  width = 15, height = 10, units = "cm", dpi = 1000
)