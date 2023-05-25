update_metadata <- function(country = c("mz", "zm")) {
  filename <- paste0("metadata", ".rds")
  saveto <- paste0(country, "/", filename)
  invisible(get_data(country = country, 
                     filename = filename))
}