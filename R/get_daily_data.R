#' Get Daily Data
#'
#' @param country 
#' @param station_id 
#'
#' @return
#' @export
#'
#' @examples
get_daily_data <- function(country = c("mw", "zm"), station_id) {
  if (length(country) > 1) stop("'country' must be of length 1")
  country <- match.arg(country)
  station_id <- as.character(station_id)
  dfs <- vector("list", length(station_id))
  names(dfs) <- station_id
  for (i in seq_along(station_id)) {
    f <- paste0(country, "/", "data", "/", station_id[i], ".rds")
    if (file.exists(f)) {
      dfs[[i]] <- readRDS(f)
    } else {
      f <- update_daily_data(country, station_id[i])
      dfs[[i]] <- readRDS(f)
    }
  }
  if (length(station_id) > 1) {
    station_data <- dplyr::bind_rows(dfs)
  } else station_data <- dfs[[1]]
  return(station_data)
}