#' Build Season Start Probabilities from File
#'
#' This function processes a file structure to extract information about the specified day for season start probabilities.
#' The information is split into lists
#'
#' @param definition_file A list containing file data and attributes generated in R-Instat.
#'
#' @return A list representing the season start probabilities with the specified days.
#'
#' @examples
#' #get_season_start_probabilities(definition_file)
build_season_start_probabilities <- function(definition_file){
  values <- definition_file$out.attrs$dimnames
  specified_day <- split_list(values$Var2)
  
  variables_list <- c("specified_day")
  
  # Create an empty list
  data_list <- list()
  data_list[["season_start_probabilities"]] <- list()
  
  # Loop through variables and add to the list if defined
  data_list[["season_start_probabilities"]][["specified_day"]] <- specified_day
  
  return(data_list)
}

