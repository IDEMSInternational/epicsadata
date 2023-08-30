#' Get Processed Station Metadata
#'
#' This function retrieves and processes station metadata for the specified country and format. The station metadata includes information about station IDs and their associated definitions.
#'
#' @param country A character vector specifying the country code for which station metadata should be retrieved and processed.
#' @param format A character vector indicating the desired format of the processed data. It can be "wide", "long", or "nested".
#'
#' @return Depending on the specified format, the function returns the processed station metadata in either wide, long, or nested format.
#'
#' @export
#'
#' @examples
#' # Retrieve and process station metadata for country "zm" in wide format
#' #all_station_metadata(country = "zm", format = "wide")
#'
#' # Retrieve and process station metadata for countries "zm" and "mw" in long format
#' #all_station_metadata(country = c("zm", "mw"), format = "long")
#'
#' # Retrieve and process station metadata for country "zm" in nested format
#' #all_station_metadata(country = "zm", format = "nested")
all_station_metadata <- function(country = c("zm", "mw"), format = c("wide", "long", "nested")){
  country <- match.arg(country)
  format <- match.arg(format)
  station_data <- station_metadata(country = "zm")
  
  definitions_data <- purrr::map(.x = station_data$station_id,
                       .f = ~ data.frame(station_id = .x, t(unlist(get_definitions_data(country = "zm", .x)))))
  definitions_data <- dplyr::bind_rows(definitions_data)
  
  wide_df <- dplyr::full_join(station_data, definitions_data)
  
  if (format == "wide"){
    return(wide_df)
  } else {
    long_df <- wide_df %>% pivot_longer(cols = !colnames(b), names_to = "definition", values_to = "value")
    if(format == "long"){
      return(long_df)
    } else {
      nested_df <- long_df %>% nest(.by = colnames(b), data = c(definition, value))
      return(nested_df)
    }
  }
}
