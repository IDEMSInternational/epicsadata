#' Get Daily Definitions Data
#'
#' This function retrieves definitions data for weather stations from a Google Cloud Storage (GCS) bucket. It includes timestamp handling to ensure that the most recent definitions file is imported.
#'
#' @param country A character vector specifying the country or countries from which to get the definitions data. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station for which to get the definitions data.
#'
#' @return A data frame containing daily data based on the station ID.
#'
#' @importFrom googleCloudStorageR gcs_list_objects gcs_get_object
#' @importFrom jsonlite fromJSON
#'
#' @seealso
#' \code{update_definitions_data} for updating definitions files.
#'
#' @export
#'
#' @examples # todo
get_definitions_data <- function(country = c("mw", "zm"), station_id) {
  if (length(country) > 1) stop("'country' must be of length 1")
  country <- match.arg(country)
  station_id <- as.character(station_id)
  dfs <- vector("list", length(station_id))
  names(dfs) <- station_id
  
  # for (i in seq_along(station_id)) {
  #   f <- paste0(country, "/", "definitions", "/", station_id[i], ".json")
  #   if (file.exists(f)) {
  #     dfs[[i]] <- jsonlite::read_json(f)
  #   } else {
  #     f <- update_definitions_data(country, station_id[i])
  #     dfs[[i]] <- f #jsonlite::write_json(f)
  #   }
  # }
  bucket_name <- get_bucket_name(country)
  for (i in seq_along(station_id)) {
    # List all files in the "definitions" directory for the station
    files <- googleCloudStorageR::gcs_list_objects(bucket = bucket_name,
                                                   prefix = paste0("definitions/", station_id[i], "."),
                                                   versions = TRUE)
    
    if (nrow(files) == 0) { stop("No files found. Check country and station_id")}
    # Filter files with the ".json" extension
    files <- files %>% dplyr::filter(grepl("\\.json$", name))
    json_files <- files$name
    
    # Check if multiple json files found. If so, take hte most recent one.
    if (length(json_files) > 1){
      # Extract timestamps from file names
      timestamps <- gsub(paste0(".*", station_id[i], "([0-9]+)\\.json$"), "\\1", json_files)
      timestamps <- suppressWarnings(as.numeric(timestamps))
      
      # Find the index of the most recent timestamp
      most_recent_index <- which.max(timestamps)
      
      # Get the most recent JSON file
      json_files <- json_files[most_recent_index]
    }
    f <- json_files
    if (file.exists(f)) {
      dfs[[i]] <- jsonlite::read_json(f)
    } else {
      f <- update_definitions_data(country, station_id[i])
      dfs[[i]] <- f #jsonlite::write_json(f)
    }
  }
  if (length(station_id) > 1) {
    station_data <- dplyr::bind_rows(dfs)
  } else station_data <- dfs[[1]]
  return(station_data)
}
