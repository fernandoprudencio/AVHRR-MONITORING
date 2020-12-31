#' @title
#' calculate NDVI anomalies
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

#' LOAD PACKAGES
library(tidyverse)
library(raster)
library(doParallel)
library(foreach)

#' CREATE DATE TABLE
df <- tibble(
  date = seq(as.Date("2002-01-01"), as.Date("2018-12-01"), by = "1 month")
) %>%
  mutate(id = 1:n()) %>%
  dplyr::filter(date >= "2007-09-01" & date <= "2008-08-31")

#' LOAD NDVI DATA
ndvi <-
  list.files("data/raster/ndvi/anomalies", ".tif", full.names = T)[df$id] %>%
  stack()

#' LOAD DATA OF C INDEX
c.index <-
  read.csv("data/tables/EC_Index/EC_index.csv", header = T, sep = ";") %>%
  as_tibble() %>%
  mutate(date = sprintf("%1$s-%2$s-01", yy, mm) %>% as.Date()) %>%
  dplyr::select(date, C) %>%
  dplyr::filter(date >= "2007-09-01" & date <= "2008-08-31")

#' CALCULATE CORRELATION
anom.values <- getValues(ndvi) %>% as_tibble()

#'   DEFINE HOW MANY CLUSTER YOU WANT TO USE
use.cores <- detectCores() - 2

#'   MAKE AND REGISTER CLUSTER
cluster <- makeCluster(use.cores)
registerDoParallel(cluster)

#' USE foreach() LOOP AND %dopar% COMMAND TO RUN IN PARALLEL
end <- date()
vls <- foreach(i = 1:nrow(anom.values)) %dopar% { # nrow(anom.values)
  print(i)
  #' load packages
  library(tidyverse)
  library(raster)
  library(rgdal)
  # i <- 10208
  df <-
    tibble(
      cindex = c.index$C,
      anom = anom.values[i, ] %>% as.numeric()
    ) %>%
    drop_na()

  if (nrow(df) >= 10) {
    corr <- cor(df$cindex, df$anom, use = "pairwise.complete.obs")
  } else {
    corr <- NA
  }
  
  return(corr)
  # if (i == 1) vls <- corr else vls <- c(vls, corr)
}

#'   END CLUSTER
stopCluster(cluster)

end
date()

save(vls, file = "data/rdata/cool_07-08.RData")

# peru <- st_read("data/vector/limits.gpkg", layer = "world_countries") %>%
#   dplyr::filter(COUNTRY == "Peru")
# raster::extract(ndvi[[1]], peru)

#' BUILD THE RESULTS TO GRIDDED DATA
grid.df <- coordinates(ndvi) %>%
  as_tibble() %>%
  rename("lon" = "x", "lat" = "y") %>%
  mutate(z = values)

data.crs <- crs(ndvi)
data.res <- res(ndvi)
data.grid <- rasterFromXYZ(grid.df, data.res, data.crs, digits = 0)

writeRaster(
  data.grid, "data/raster/ndvi/ndvi_vs_sst/cool_07-08.tif",
  overwrite = T
)