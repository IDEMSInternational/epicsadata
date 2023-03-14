update_daily_data <- function(country = c("mz", "zm"), station_id) {
  filename <- paste0("data", "/", station_id, ".rds")
  saveto <- paste0(country, "/", filename)
  invisible(update_data(country = country, 
                        filename = filename,
                        saveto = saveto))
}