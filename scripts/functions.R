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