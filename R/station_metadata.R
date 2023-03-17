#' Get Station Metadata
#'
#' @param country 
#'
#' @return
#' @export
#'
#' @examples
station_metadata <- function(country = c("mw", "zm")) {
  if (length(country) > 1) stop("'country' must be of length 1")
  country <- match.arg(country)
  
  f <- paste0(country, "/", "metadata", ".rds")
  if (file.exists(f)) {
    metadata <- readRDS(f)
  } else {
    f <- update_metadata(country)
    metadata <- readRDS(f)
  }
  return(metadata)
}