#' Get Station Metadata
#'
#' @param country A character vector specifying the country or countries from which to get the metadata. Options are `"mz"` and `"zm"`.
#'
#' @return This function gets the metadata for the specified station in the specified country.
#' @export
#'
#' @examples #todo
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