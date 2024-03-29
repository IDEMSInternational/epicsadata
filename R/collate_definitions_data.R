#' Collate Definitions Data for Climatic Analysis from R-Instat
#'
#' This function aggregates various climatic data definitions, including annual summaries, 
#' temperature summaries, crop data, and probabilities of season starts. It is designed to work
#' within a specific context that involves climatic data processing and analysis, particularly
#' focusing on data related to Ghana's climate. The function uses multiple sources of data
#' and calculations to generate a comprehensive list-formatted summary.
#'
#' @param data The name of the main data set, default is "ghana".
#' @param data_by_year The name of the data set that contains data aggregated by year, default is "ghana_by_station_year".
#' @param data_by_year_month The name of the data set that contains data aggregated by year and month, default is NULL.
#' @param crop_data The name of the crop data set, default is "crop_def".
#' @param rain The name of the column containing rainfall data.
#' @param tmin The name of the column containing minimum temperature data.
#' @param tmax The name of the column containing maximum temperature data.
#' @param year The name of the column containing year data.
#' @param month The name of the column containing month data.
#' @param summaries The name of the summaries to show. Options are `"annual_rainfall"`, `"annual_temperature"`, `"monthly_temperature"`, `"extremes"`, `"crop_success"`, `"start_season"`.
#' @export
#' @return A list that contains the aggregated data definitions.
#' @examples
#' #data_book <- list(get_climatic_column_name = function(data_name, col_name) { return(col_name) },
#' #                  get_calculations = function(data_name) { list() },
#' #                  get_data_frame_metadata = function(data_name) { list() })
#' #collate_definitions_data(data_book = data_book)
#' 
collate_definitions_data <- function(data = "ghana",
                                     data_by_year = "ghana_by_station_year",
                                     data_by_year_month = NULL,
                                     crop_data = "crop_def",
                                     rain = data_book$get_climatic_column_name(data_name = "ghana", "rain"),
                                     tmin = data_book$get_climatic_column_name(data_name = "ghana", "temp_min"),
                                     tmax = data_book$get_climatic_column_name("ghana", "temp_max"),
                                     year = data_book$get_climatic_column_name("ghana", "year"),
                                     month = data_book$get_climatic_column_name("ghana", "month"),
                                     summaries = c("annual_rainfall", "annual_temperature", "monthly_temperature", "extremes", "crop_success", "start_season")){
  
  definitions_data <- get_r_instat_definitions(data_book$get_calculations(data))
  definitions_year <- get_r_instat_definitions(data_book$get_calculations(data_by_year))
  
  # if yes to annual summaries - give the data frame "ghana_by_station_year"
  if ("annual_rainfall" %in% summaries){
    annual_summaries <- build_annual_summaries_definitions(data_name = data,
                                                           data_by_year = definitions_year,
                                                           data = definitions_data,
                                                           rain_name = rain)
  } else {
    annual_summaries <- NULL
  }

  if (any(grepl("_temperature", summaries))){
    # if yes to annual temperature summaries - give the data frame "ghana_by_station_year"
    # if yes to monthly temperature summaries - give the data frame "ghana_by_station_year_month"
    if ("annual_temperature" %in% summaries){
      annual_temp <- definitions_year
    } else {
      annual_temp <- NULL
    }
    if ("monthly_temperature" %in% summaries){
      if (!is.null(data_by_year_month)){
        definitions_year_month <- get_r_instat_definitions(data_book$get_calculations(data_by_year_month))
      } else {
        stop("monthly temperature requested but no data_by_year_month file given.")
      }
    } else {
      definitions_year_month <- NULL
    }
    temperature_summaries <- build_total_temperature_summaries(data_by_year = annual_temp,
                                                               data_by_year_month = definitions_year_month,
                                                               tmin = tmin, tmax = tmax, year = year,
                                                               month = month)
  } else {
    temperature_summaries <- NULL
  }

  # if yes to crop success then ...
  if ("crop_success" %in% summaries){
    if (!is.null(crop_data)){
      definitions_crop <- data_book$get_data_frame_metadata(crop_data)
    } else {
      stop("Crop summaries requested but no crop_data file given.")
    }
    crop_summaries <- build_crop_definitions(definitions_crop)
  } else {
    crop_summaries <- NULL
  }

  # if yes to probabilities
  if ("start_season" %in% summaries){
    if (!is.null(crop_data)){
      definitions_crop <- data_book$get_data_frame_metadata(crop_data)
    } else {
      stop("Season start summaries requested but no crop_data file given.")
    }
    season_start_summaries <- build_season_start_probabilities(definitions_crop)
  } else {
    season_start_summaries <- NULL
  }
  
  # extremes then ...
  
  # overall:
  data_list <- c(annual_summaries, temperature_summaries, crop_summaries, season_start_summaries)
  
  # remove anything of length 0 
  # Define a function to check the length of each element
  length_not_zero <- function(element) {
    length(element) > 0
  }
  
  # Filter the list x to remove elements with length 0
  data_list <- Filter(length_not_zero, data_list)
  
  return(data_list)
}
