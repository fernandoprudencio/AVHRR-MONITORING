#' @title availability of daily precipitation data
#' @author Fernando Prudencio

rm(list = ls())

#' LOAD PACKAGES
library(tidyverse)

#' READ DAILY RAINFALL DATA
data.pp <- read.csv("data/tables/BD_with_filterQA/BD_Pp.csv", sep = ";") %>%
  mutate(date = date %>% as.Date(format = "%d/%m/%Y")) %>%
  dplyr::filter(date >= "1980-01-01" & "2013-12-31" >= date) %>%
  dplyr::select(-date)

data.pp[data.pp == -99.9] <- NA

#' AVAILABLE DATA
df.pp <- tibble(
  miss = colSums(is.na(data.pp)) * 100 / nrow(data.pp),
  cod = names(data.pp)
) %>%
  mutate(id = 1:n()) %>%
  filter(miss <= 10)

#' WRITE AVAILABLE RAINFALL DATA LIST
save(df.pp, file = "data/rdata/available_rainfall_data.RData")