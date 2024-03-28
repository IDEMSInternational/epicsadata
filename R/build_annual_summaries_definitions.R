#' Get annual summaries definitions
#'
#' Retrieves annual summaries definitions including start of rains, end of rains, end of season, seasonal length,
#' and annual rainfall summaries.
#'
#' @param data_name The name of the data.
#' @param data_by_year A list containing definitions for start of rains, end of rains, end of season, and seasonal length.
#' @param data A list containing data definitions.
#' @param rain_name The name of the rainfall column in the data.
#' @return A list of annual summaries definitions.
#'
#' @examples
#' # Example usage:
#' #get_annual_summaries_definitions("data_name", data_by_year, data)
build_annual_summaries_definitions <- function(data_name, data_by_year, data,
                                               rain_name = data_book$get_climatic_column_name(data_name = data_name, col_name = "rain")){
  
  start_of_rains <- get_start_rains_definitions(data_by_year$start_rain)
  end_rains <- get_end_rains_definitions(data_by_year$end_rains)
  end_season <- get_end_season_definitions(data_by_year$end_season)
  seasonal_length <- get_season_length_definitions(data_by_year$seasonal_length)
  
  # for annual rainfall / rainy days in year:
  total_rain_counts <- get_total_rain_counts(data_name, data_by_year, rain_name)
    
  # Get the list of summaries:
  summaries_list <- c(start_of_rains, end_rains, end_season, seasonal_length, total_rain_counts)
  return(summaries_list)
}