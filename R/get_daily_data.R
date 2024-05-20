#' Get Daily Data
#'
#' @param country A character vector specifying the country or countries from which to get the data. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station for which to get the daily data.
#'
#' @return A data frame containing the daily data for the specified station and country.
#' @export
#'
#' @examples #
get_daily_data <- function(country = c("mw", "zm", "zm_test"), station_id) {
  if (length(country) > 1) stop("'country' must be of length 1")
  station_id <- as.character(station_id)
  dfs <- vector("list", length(station_id))
  names(dfs) <- station_id
  for (i in seq_along(station_id)) {
    f <- paste0(country, "/", "data", "/", station_id[i], ".rds")
    if (file.exists(f)) {
      dfs[[i]] <- readRDS(f)
    } else {
      f <- update_daily_data(country, station_id[i])
      dfs[[i]] <- f#saveRDS(o, file = f)
    }
  }
  if (length(station_id) > 1) {
    station_data <- dplyr::bind_rows(dfs)
  } else station_data <- dfs[[1]]
  return(station_data)
}
