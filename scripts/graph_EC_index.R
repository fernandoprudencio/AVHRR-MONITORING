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
  "sf", "gridExtra", "tidyverse", "ggthemes", "grid", "gridExtra"
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
library(scales)

#' INDEX
ec.index <- read.csv(
  "data/tables/EC_Index/EC_index.csv",
  header = T, sep = ";"
) %>%
  as_tibble() %>%
  mutate(date = sprintf("%1$s-%2$s-01", yy, mm) %>% as.Date()) %>%
  dplyr::select(date, E, C) %>%
  dplyr::filter(date >= "2002-01-01")

#' JOIN DATA
table <- ec.index %>% gather("type", "value", -date)

plt <- ggplot(table) +
  labs(y = "Index", subtitle = "C and E index (2002-2018)") +
  geom_line(
    aes(
      x = date, y = value,
      linetype = type,
      color = type,
      size = type
    )
  ) +
  scale_linetype_manual(
    values = c("solid", "solid"), labels = c("C index", "E index")
  ) +
  scale_color_manual(
    values = c("black", "gray"),
    labels = c("C index", "E index")
  ) +
  scale_size_manual(
    values = c(.8, .8),
    labels = c("C index", "E index")
  ) +
  geom_hline(
    yintercept = 0, color = "black", size = .3
  ) +
  scale_x_date(
    limits = c(as.Date("2002-01-01"), as.Date("2018-12-01")),
    date_labels = "%y",
    breaks =
      seq(as.Date("2002-01-01"), as.Date("2018-12-01"), by = "year"),#"1 year"
    expand = c(.03, .03)
  ) +
  scale_y_continuous(
    breaks = seq(-3, 3, 1),
    limits = c(-3, 3),
    expand = c(0, 0)
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
plt
# SAVE PLOT
ggsave(
  plt, filename = "exports/EC-index.png",
  width = 15, height = 10, units = "cm", dpi = 1000
)