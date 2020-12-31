#' FUNCTION N1
#'   Apply quality filter to AVHRR dataset
qaFilter <- function(img, bit) {
  # Extract the NDVI band
  ndvi <- img$select("NDVI")

  # Extract the QUALITY band
  qa <- img$select("QA")

  # Select pixels to mask
  qa_mask <- qa$bitwiseAnd(bit)$eq(0)

  # Mask pixels with value zero
  ndvi$updateMask(qa_mask) %>% return()
}

#' FUNCTION N2
#'   Extract values from point vectors GEE
ts.extract <- function(date, images, points) {
  print(date)
  year <- str_sub(date, 1, 4) %>% as.numeric()
  month <- str_sub(date, 6, 7) %>% as.numeric()
  ndvi <- images$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    filter(ee$Filter$calendarRange(month, month, "month"))$
    max()

  data <- ee_extract(ndvi, points, fun = ee$Reducer$mean(), scale = 5000)

  if (ncol(data) == 0) { data <- data.frame(NDVI = rep(NA, nrow(data))) }

  return(data)
}

#' FUNCTION N2
#'   Calculate anomalies
fun.anom <- function(cluster, data) {
  #' 1| Select stations
  sf.station <- read_sf(
    dsn = "data/vector/senamhi_weather_stations.gpkg",
    layer = "weather_stations_HIST"
  ) %>%
    mutate(cod = as.character(cod)) %>%
    dplyr::filter(clus %in% cluster)
  
  #' 2| Calculate monthly NDVI
  data.monthly <-
    tibble(
      ndvi = dplyr::select(data, sf.station$cod) %>%
        apply(1, FUN = function(x) mean(x, na.rm = T))
    ) %>%
    mutate(
      date = data$date,
      ndvi = ndvi * .0001
    ) %>%
    dplyr::filter(date <= "2018-12-31") %>%
    group_by(date = str_sub(date, 1, 7)) %>%
    summarise_all(.funs = function(x) mean(x, na.rm = T)) %>%
    mutate(
      date = as.Date(sprintf("%1$s-01", date)),
      month = str_sub(date, 6, 7)
    )
  
  #' 3| calculate climatology
  data.clim <-
    data.monthly %>%
    # dplyr::filter(
    #   !(
    #     (date >= "2005-09-01" & date <= "2006-08-31") |
    #       (date >= "2010-09-01" & date <= "2011-08-31")
    #   )
    # ) %>%
    group_by(date = str_sub(date, 6, 7)) %>%
    summarise(data.clim = mean(ndvi, na.rm = T)) %>%
    rename(month = date)
  
  #' 4| calculate anomalies
  data.anom <-
    left_join(data.monthly, data.clim, "month") %>%
    mutate(data.anom = ndvi - data.clim)
  
  #' RETURN
  return(data.anom$data.anom)
}