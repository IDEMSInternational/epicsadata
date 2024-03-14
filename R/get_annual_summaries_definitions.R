#' Get annual summaries definitions
#'
#' Retrieves annual summaries definitions including start of rains, end of rains, end of season, seasonal length,
#' and annual rainfall summaries.
#'
#' @param data_name The name of the data.
#' @param by_definitions_list A list containing definitions for start of rains, end of rains, end of season, and seasonal length.
#' @param data_definitions_list A list containing data definitions.
#' @return A JSON representation of annual summaries definitions.
#' @export
#' @examples
#' # Example usage:
#' #get_annual_summaries_definitions("data_name", by_definitions_list, data_definitions_list)
get_annual_summaries_definitions <- function(data_name, by_definitions_list, data_definitions_list){
  
  start_of_rains <- get_start_rains_definitions(by_definitions_list$start_rain)
  end_rains <- get_end_rains_definitions(by_definitions_list$end_rains)
  end_season <- get_end_season_definitions(by_definitions_list$end_season)
  seasonal_length <- get_season_length_definitions(by_definitions_list$seasonal_length)
  
  # for annual rainfall / rainy days in year:
  # # 1. check what the rainfall column is called
  rain_name <- data_book$get_climatic_column_name(data_name = data_name, col_name = "rain")
  sum_rain <- by_defs[[paste0("sum_", rain_name)]]
  # 
  # # 2. check if we have either sum_Rainday or sum_count
  # # we can tell if there's a count of the number of rainy days by if "count" is a calculation:
  if (!is.null(ghana_defs$count)){
    n_rain_def <- c(by_defs$sum_count, by_defs$sum_Rainday)
  }
  annual_rain <- get_annual_rain_definitions(sum_rain, n_rain_def, data_definitions_list)
  
  # Get the list of summaries:
  summaries_list <- list(start_of_rains, end_rains, end_season, seasonal_length, annual_rain)
  
  # Convert the list to JSON format
  data_list <- jsonlite::toJSON(summaries_list, auto_unbox = TRUE, pretty = TRUE)
  return(data_list)
}