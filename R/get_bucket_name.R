#' Get Bucket Name
#'
#' @param country A character vector specifying the country or countries from which to update the data. Options are `"mz"` and `"zm"`.
#'
#' @return Returns the name of the bucket for the Malawi or Zambia data.
#'
#' @examples #get_bucket_name("mw")
get_bucket_name <- function(country = c("mw", "zm")) {
  if (length(country) > 1) stop("'country' must be length 1.")
  country <- match.arg(country)
  if (country == "mw") return("malawi_data")
  else if (country == "zm") return("zambia_data")
}
