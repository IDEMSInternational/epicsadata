get_forecast_data <- function(country = c("mz", "zm"), station_id) {
  # and anything else we need here! I've just taken this from update_daily_data.R
  filename <- paste0("data", "/", station_id, ".rds")
  saveto <- paste0(country, "/", filename)
  invisible(get_data(country = country, filename = filename, saveto = saveto))
}