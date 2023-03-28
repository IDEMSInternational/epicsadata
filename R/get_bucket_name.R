#' Get Bucket Name
#'
#' @param country 
#'
#' @return
#'
#' @examples
get_bucket_name <- function(country = c("mw", "zm")) {
  if (length(country) > 1) stop("'country' must be length 1.")
  country <- match.arg(country)
  if (country == "mw") return("malawi_data")
  else if (country == "zm") return("zambia_data")
}