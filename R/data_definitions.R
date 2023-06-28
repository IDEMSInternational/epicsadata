#' Get Data Column Names
#'
#' @param station_id The ID of the station.
#'
#' @return A list of data definitions based on the station ID.
#' @export
#'
#' @examples # todo
data_definitions <- function(station_id) {
  data_definitions <- list()
  
  if (station_id == "01122") {
    data_definitions$station <- "station"  
    data_definitions$date <- "date"  
    data_definitions$year <- "year"  
    data_definitions$month <- "month"  
    data_definitions$rain <- "rain"
  } else {
    data_definitions$station <- "station_name"  
    data_definitions$date <- "date"  
    data_definitions$year <- "year"  
    data_definitions$month <- "month"  
    data_definitions$day <- "day"  
    data_definitions$doy <- "doy"  
    data_definitions$rain <- "rain"  
    data_definitions$tmin <- "tmin"  
    data_definitions$tmax <- "tmax"  
  }
  
  return(data_definitions)
}
