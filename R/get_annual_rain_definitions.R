#' Get annual rain definitions
#'
#' Retrieves annual rain definitions.
#'
#' @param annual_rain The annual rain data.
#' @param ghana_defs The Ghana definitions.
#' @return A JSON representation of annual rain definitions.
#' 
#' @examples
#' # Example usage:
#' #get_annual_rain_definitions(annual_rain, ghana_defs)
#' 
#' # In Progress.
get_annual_rain_definitions <- function(sum_rain = NULL, n_rain_def = NULL, data_definition){
  data_list <- list()
  data_list[["annual_rain"]] <- list()
  if (is.null(sum_rain) & is.null(n_rain_def)) {
    return(data_list)
  }
  
  if (!is.null(sum_rain)){
    annual_rain <- "TRUE"
  } else {
    annual_rain <- "FALSE"    
  }
  
  if (!is.null(n_rain_def)){
    n_rain <- "TRUE"
    rain_day <- extract_value(data_definition$count$rain_day[[2]], " >= ", FALSE)
  } else {
    n_rain <- "FALSE"    
  }
  
  sum_rain <- c(sum_rain, n_rain_def)
  na_rm <- extract_value(sum_rain$function_exp, "na.rm = ", FALSE)
  na_n <- extract_value(sum_rain$function_exp, "na_max_n = ", TRUE)
  na_n_non <- extract_value(sum_rain$function_exp, "na_min_n = ", TRUE)
  na_consec <- extract_value(sum_rain$function_exp, "na_consecutive_n = ", TRUE)
  na_prop <- extract_value(sum_rain$function_exp, "na_max_prop = ", TRUE)
  
  variables_list = c("annual_rain", "n_rain", "rain_day", "na_rm", "na_n",
                     "na_n_non", "na_consec", "na_prop")
  
  # Create an empty list
  data_list <- list()
  data_list[["annual_rain"]] <- list()
  
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      data_list[["annual_rain"]][[variable]] <- get(variable)
    }
  }
  return(data_list)
}
