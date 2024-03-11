#' Get Summaries Data
#'
#' @param country A character vector specifying the country or countries from which to get the data. Options are `"mz"` and `"zm"`.
#' @param station_id A character string specifying the ID of the station for which to get the summary data.
#' @param summary A character string specifying the summary to retrieve.
#'
#' @return A data frame containing the summary data for the specified station and country.
#' @export
#'
#' @examples #
#' 
get_summaries_data <- function(country = c("mw", "zm"), station_id, summary) {
  if (length(country) > 1) stop("'country' must be of length 1")
  country <- match.arg(country)
  station_id <- as.character(station_id)
  dfs <- vector("list", length(station_id))
  names(dfs) <- station_id
  bucket_name <- epicsadata:::get_bucket_name(country)
  for (i in seq_along(station_id)) {
    f <- paste0(country, "/", "summaries", "/", summary, "_", station_id[i], ".rds")
    
    #   # TODO: fix up for rds_files > 1 (e.g., if several summary files)
    #   files <- googleCloudStorageR::gcs_list_objects(bucket = bucket_name, 
    #                                                  prefix = paste0("summaries/", summary, "_", station_id[i], "."), versions = TRUE)
    #   if (nrow(files) == 0) {
    #     stop("No files found. Check country and station_id")
    #   }
    #   files <- files %>% dplyr::filter(grepl("\\.rds$", name))
    #   rds_files <- files$name
    #   
    #   if (length(rds_files) > 1) {
    #     timestamps <- gsub(paste0(".*", station_id[i], 
    #                               "([0-9]+)\\.rds$"), "\\1", rds_files)
    #     timestamps <- suppressWarnings(as.numeric(timestamps))
    #     most_recent_index <- which.max(timestamps)
    #     rds_files <- rds_files[most_recent_index]
    #     station_id[i] <- stringr::str_remove(stringr::str_remove(rds_files, "summaries/"), ".rds")
    #   }
    
    if (file.exists(f)) {
      dfs[[i]] <- readRDS(f)
    }
    else {
      f <- update_summaries_data(country, station_id[i], summary)
      dfs[[i]] <- f
    }
  }
  station_data <- dfs[[1]]
  return(station_data)
}
