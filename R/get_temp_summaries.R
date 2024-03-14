#' Get temperature summaries
#'
#' Retrieves temperature summaries based on provided parameters.
#'
#' @param temp_summary_name Character vector specifying the name of the temperature summary.
#' @param year Numeric vector specifying the year.
#' @param month Numeric vector specifying the month.
#' @param by_definition_list A list of temperature summaries by definition.
#' @param by_definition_2_list An optional second list of temperature summaries by definition.
#' @return A list containing temperature summary information.

#' @examples
#' # Example usage:
#' #get_temp_summaries("summary_name", year = 2023, month = 5, by_definition_list = my_definition_list)
get_temp_summaries <- function(temp_summary_name, year, month,
                               by_definition_list, by_definition_2_list = NULL){
  # Note, we take the na.rm bits from by_definition_list
  temp_summary <- by_definition_list[[temp_summary_name]]
  temp_summary_2 <- by_definition_2_list[[temp_summary_name]]
  to <- c()
  if (!is.null(temp_summary)){
    if (year %in% unlist(temp_summary$by_1) | year %in% unlist(temp_summary_2$by_1)){
      to <- c(to, "annual")
    }
    if (month %in% unlist(temp_summary$by_1) | month %in% unlist(temp_summary_2$by_1)){
      to <- c(to, "monthly")
    }
    
    na_rm <- extract_value(temp_summary$function_exp, "na.rm = ", FALSE)
    na_n <- extract_value(temp_summary$function_exp, "na_max_n = ", TRUE)
    na_n_non <- extract_value(temp_summary$function_exp, "na_min_n = ", TRUE)
    na_consec <- extract_value(temp_summary$function_exp, "na_consecutive_n = ", TRUE)
    na_prop <- extract_value(temp_summary$function_exp, "na_max_prop = ", TRUE)
  }
  
  variables_list = c("to", "na_rm", "na_n", "na_n_non", "na_consec", "na_prop")
  
  # Create an empty list
  temp_summary_name_list <- NULL
  
  # Loop through variables and add to the list if defined
  for (variable in variables_list) {
    if (exists(variable) && !is.na(get(variable))) {
      temp_summary_name_list[[variable]] <- get(variable)
    }
  }
  return(temp_summary_name_list)
}