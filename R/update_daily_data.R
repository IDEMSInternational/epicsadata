update_daily_data <- function(country = c("mz", "zm"), station_id) {
  filename <- paste0("data", "/", station_id, ".rds")
  invisible(get_data(country = country, filename = filename))
}