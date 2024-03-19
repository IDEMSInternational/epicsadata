#' Calculate total temperature summaries
#'
#' Calculates total temperature summaries based on provided parameters.
#'
#' @param tmin Character vector specifying the minimum temperature.
#' @param tmax Character vector specifying the maximum temperature.
#' @param year Character vector specifying the year.
#' @param month Character vector specifying the month.
#' @param data_by_year A list of temperature summaries by definition (e.g., year).
#' @param data_by_year_month An optional second list of temperature summaries by definition (e.g., year and month).
#' @return A list containing total temperature summaries.
#' @export
#' @examples
#' # Example usage:
#' #total_temperature_summaries(tmin = "tmin", tmax = "tmax", year = "year", month = "month", 
#' #                             data_by_year = my_definition_list, data_by_year_month = my_definition_list_2)
build_total_temperature_summaries <- function(tmin = data_book$get_climatic_column_name(data_name, "temp_min"),
                                              tmax = data_book$get_climatic_column_name(data_name, "temp_max"),
                                              year = data_book$get_climatic_column_name(data_name, "year"),
                                              month = data_book$get_climatic_column_name(data_name, "month"),
                                              data_by_year,
                                              data_by_year_month){
  data_list <- get_temperature_summary_definitions(tmin = tmin, tmax = tmax, year = year, month = month,
                                                   data_by_year = data_by_year,
                                                   data_by_year_month = data_by_year_month)  
  return(data_list)
}
