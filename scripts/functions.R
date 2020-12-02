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