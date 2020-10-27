rm(list = ls())

library(dplyr)
library(reshape)
library(ggplot2)
library(sf)
library(cluster)
library(fpc)
library(factoextra)
library(ggpubr)
library(adegraphics)

data.pp <- read.csv('data/tables/BD_with_filterQA/BD_Pp.csv', sep = ';') %>% 
  mutate(date = date %>% as.Date(format = "%d/%m/%Y")) %>%
  filter(date >= '1980-01-01' & '2013-12-31' >= date)

data.pp[data.pp == -99.9] <- NA

df <- data.frame(
  miss = ((is.na(data.pp) %>% colSums()) * 100 / nrow(data.pp))[-1],
  COD = names(data.pp)[-1]
) %>%
  mutate(id = 1:n()) %>%
  filter(miss <= 10)
