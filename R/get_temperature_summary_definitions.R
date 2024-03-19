#' Get temperature summary definitions
#'
#' Retrieves temperature summary definitions based on provided parameters.
#'
#' @param tmin Character vector specifying the minimum temperature.
#' @param tmax Character vector specifying the maximum temperature.
#' @param year Character vector specifying the year.
#' @param month Character vector specifying the month.
#' @param data_by_year A list of temperature summaries by definition (e.g., year).
#' @param data_by_year_month An optional second list of temperature summaries by definition (e.g., year and month).
#' @return A list containing temperature summary definitions.
#' @export
#' 
#' @examples
#' # Example usage:
#' #get_temperature_summary_definitions(by_definition_list = my_definition_list, by_definition_2_list = my_definition_list_2)
get_temperature_summary_definitions <- function(tmin = data_book$get_climatic_column_name(data_name, "temp_min"),
                                                tmax = data_book$get_climatic_column_name(data_name, "temp_max"),
                                                year = data_book$get_climatic_column_name(data_name, "year"),
                                                month = data_book$get_climatic_column_name(data_name, "month"),
                                                data_by_year,
                                                data_by_year_month = NULL){
  tmin_summary_names <- paste0(c("min_", "max_", "mean_"), tmin)
  tmax_summary_names <- paste0(c("min_", "max_", "mean_"), tmax)
  temp_summary_names <- c(tmin_summary_names, tmax_summary_names)
  temp_summary_definitions <- purrr::map(.x = temp_summary_names,
                                         .f = ~get_temp_summaries(.x, year, month, data_by_year, data_by_year_month))
  names(temp_summary_definitions) <- temp_summary_names
  return(temp_summary_definitions)
}
