#' Calculate total temperature summaries
#'
#' Calculates total temperature summaries based on provided parameters.
#'
#' @param tmin Character vector specifying the minimum temperature.
#' @param tmax Character vector specifying the maximum temperature.
#' @param year Character vector specifying the year.
#' @param month Character vector specifying the month.
#' @param by_definition_list A list of temperature summaries by definition.
#' @param by_definition_2_list An optional second list of temperature summaries by definition.
#' @return A JSON-formatted string containing total temperature summaries.
#' @export
#' @examples
#' # Example usage:
#' #total_temperature_summaries(tmin = "tmin", tmax = "tmax", year = "year", month = "month", 
#' #                             by_definition_list = my_definition_list, by_definition_2_list = my_definition_list_2)
build_total_temperature_summaries <- function(tmin = data_book$get_climatic_column_name(data_name, "temp_min"),
                                        tmax = data_book$get_climatic_column_name(data_name, "temp_max"),
                                        year = data_book$get_climatic_column_name(data_name, "year"),
                                        month = data_book$get_climatic_column_name(data_name, "month"),
                                        by_definition_list = data_defs,
                                        by_definition_2_list = by_definition_2_list){
  data_list <- get_temperature_summary_definitions(tmin = tmin, tmax = tmax, year = year, month = month,
                                      by_definition_list = by_definition_list,
                                      by_definition_2_list = by_definition_2_list)  
  
  # Convert the list to JSON format
  data_list <- jsonlite::toJSON(data_list, auto_unbox = TRUE, pretty = TRUE)
  return(data_list)
}
