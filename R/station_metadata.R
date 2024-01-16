#' Get Station Metadata
#'
#' This function retrieves metadata for weather stations in specified countries.
#'
#' @param country A character vector specifying the country or countries from which to get the metadata. Options include "zm" (Zambia) and "mz" (Mozambique).
#' @param station_id A character vector specifying the station IDs to filter by. If provided, only metadata for the specified station IDs will be returned.
#' @param include_definitions A logical value indicating whether to include definitions data. If `TRUE`, additional information about station definitions will be included in the output.
#' @param format A character vector specifying the format of the output. Options are `"wide"` (default), `"long"`, or `"nested"`.
#' @return If `include_definitions` is FALSE, the function returns a data frame with metadata for the specified stations. If `include_definitions` is `TRUE`, it returns a data frame with both metadata and station definitions.
#' @export
#'
#' @examples
#' # TODO
#' 
#' @seealso
#' \code{update_metadata} for updating metadata files.
#'
#' @importFrom purrr map list_rbind
#' @importFrom dplyr full_join filter mutate
#' @importFrom tidyr pivot_longer nest
station_metadata <- function(country = NULL, station_id = NULL, include_definitions = FALSE, format = c("wide", "long", "nested")){
  format <- match.arg(format)
  
  # if no country is given, then no station_id can be given
  if (is.null(country) && !is.null(station_id)) {
    warning("No country given. Ignoring station_id")
    station_id <- NULL
  }
  
  # If no country is given, then set it to be both countries.
  #  if (is.null(country)) { country <- c("mw", "zm") }
  if (is.null(country)) { 
    warning("No country given. ZM will be returned.")
    country <- c("zm")
  }
  # temporarily disabled because we do not have metadata or definition files for mw
  
  # Check if the metadata file exists, if not, update it
  check_exists <- function(f, country){
    if (file.exists(f)) {
      metadata <- readRDS(f)
    } else {
      f <- update_metadata(country)
      metadata <- f#readRDS(f)
    }
  }
  
  # Create the file path for metadata. Then check the file exists, if not, update it.
  station_data <- purrr::map(.x = country,
                                 .f = ~ check_exists(f = paste0(.x, "/", "metadata", ".rds"),
                                                     country = .x) %>%
                                   dplyr::mutate(country_code = country))
  
  if (length(station_data) == 1) station_data <- station_data[[1]] 
  # Filter by 'station_id' if provided
  if (!is.null(station_id)){
    station_id_vars <- station_id
    station_data <- station_data %>%
      dplyr::filter(station_id %in% station_id_vars)
  }
  
  if (!include_definitions) return(station_data)
  
  # if include definitions then run the following -
  process_data <- function(station_data, format){
    definitions_data <- purrr::map(.x = station_data$station_id,
                                   .f = ~ data.frame(station_id = .x, t(unlist(get_definitions_data(country = "zm", .x)))))
    definitions_data <- purrr::list_rbind(definitions_data)
    
    #c("station_id", "station_name", "latitude", "longitude", "elevation", "district", "country_code") 
    wide_df <- dplyr::full_join(station_data, definitions_data)
    
    if (format == "wide"){
      return(wide_df)
    } else {
      long_df <- wide_df %>% tidyr::pivot_longer(cols = !colnames(station_data), names_to = "definition", values_to = "value")
      if(format == "long"){
        return(long_df)
      } else {
        nested_df <- long_df %>% tidyr::nest(.by = colnames(station_data), data = c(definition, value))
        return(nested_df)
      }
    }
  }
  
  if (is.data.frame(station_data)){
    station_data <- process_data(station_data, format = format)
  } else {
    station_data <- purrr::map(.x = station_data, .f = ~process_data(.x, format = format))
  }
  return(station_data) 
}
