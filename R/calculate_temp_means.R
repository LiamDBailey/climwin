#' Calculate Climate Variable Means for Different Date Ranges
#'
#' This function calculates the mean (or other summary statistic) of a climate variable for every possible 
#' combination of date ranges within the specified range, for each date in the biological data. The function
#' can operate in two modes:
#' 
#' 1. Relative mode (default): Calculates means relative to each date in bio_data
#' 2. Absolute mode: Uses a reference date to determine the date range, keeping the same day and month
#'    but using the year from each bio_data date
#'
#' @param range A numeric vector specifying the number of days to look back from each date in bio_data.
#'              For example, 0 represents the date itself, while 100 represents 100 days before that date.
#' @param climate_data A data frame containing climate data. If not provided, the function will attempt
#'                    to read from "MassClimate.csv".
#' @param bio_data A data frame containing biological data with a date column. The function will calculate
#'                climate summaries relative to each date in this data frame.
#' @param cdate Character string specifying the name of the date column in climate_data.
#'              Defaults to "Date".
#' @param bdate Character string specifying the name of the date column in bio_data.
#'              Defaults to "Date".
#' @param xvar Character string specifying the name of the climate variable column in climate_data.
#'             Defaults to "Temp".
#' @param fn A function to use for summarizing the climate data. Defaults to mean().
#'           Must be a function that can operate on a numeric vector.
#' @param type Character string specifying the type of date range calculation. Must be either "relative"
#'            (default) or "absolute".
#' @param refday Character string in format "DD/MM/YYYY" specifying the reference date to use when
#'              type is "absolute". Required when type is "absolute".
#'
#' @return A list of data frames, where each data frame corresponds to a specific range combination.
#'        Each data frame contains:
#'   \item{Bio_Date}{The date from bio_data (character string in DD/MM/YYYY format)}
#'   \item{Start_Date}{The beginning date of each range (character string in DD/MM/YYYY format)}
#'   \item{End_Date}{The end date of each range (character string in DD/MM/YYYY format)}
#'   \item{Start_Day}{The start day as an integer (number of days before Bio_Date)}
#'   \item{End_Day}{The end day as an integer (number of days before Bio_Date)}
#'   \item{Summary_Value}{The summarized climate variable for the specified range}
#'
#' @examples
#' # Calculate temperature means for all possible combinations in range 0 to 2
#' # for each date in the biological data (relative mode)
#' climate_data <- read.csv("MassClimate.csv")
#' bio_data <- read.csv("Mass.csv")
#' result <- calculate_temp_means(range = 0:2, climate_data = climate_data,
#'                                bio_data = bio_data)
#'
#' # Calculate temperature means using absolute mode with reference date
#' result <- calculate_temp_means(0:2,
#'                                climate_data = climate_data,
#'                                bio_data = bio_data, 
#'                                type = "absolute", 
#'                                refday = "15/01/1979")
#'
#' # Calculate rainfall means using custom column names
#' my_climate <- data.frame(
#'   my_date = c("01/01/1979", "02/01/1979"),
#'   rainfall = c(10, 12)
#' )
#' my_bio <- data.frame(
#'   bio_date = c("01/01/1979", "02/01/1979")
#' )
#' result <- calculate_temp_means(0:1, 
#'                              climate_data = my_climate,
#'                              bio_data = my_bio,
#'                              cdate = "my_date",
#'                              bdate = "bio_date",
#'                              xvar = "rainfall")
#'
#' # Use different summary function (e.g., median)
#' result <- calculate_temp_means(0:2,
#'                                climate_data = climate_data,
#'                                bio_data = bio_data, fn = median)
#'
#' @export
calculate_temp_means <- function(range, 
                                 climate_data,
                                 bio_data,
                                 cdate = "Date",
                                 bdate = "Date",
                                 xvar = "Temp",
                                 fn = mean,
                                 type = "relative",
                                 refday = NULL) {
  
  # Validate function first
  if (!is.function(fn)) {
    stop("fn must be a function")
  }
  
  # Validate type parameter
  if (!type %in% c("relative", "absolute")) {
    stop("type must be either 'relative' or 'absolute'")
  }
  
  # Validate refday parameter
  if (type == "absolute") {
    if (is.null(refday)) {
      stop("refday must be provided when type is 'absolute'")
    }
    refday_date <- as.Date(refday, format = "%d/%m/%Y")
    if (is.na(refday_date)) {
      stop("refday must be in format 'DD/MM/YYYY'")
    }
  }
  
  # Read the climate data if not provided
  if (missing(climate_data) || nrow(climate_data) == 0) {
    stop("climate_data must contain at least 1 row")
  }
  
  # Validate climate data structure
  if (!all(c(cdate, xvar) %in% names(climate_data))) {
    stop(sprintf("climate_data must contain columns '%s' and '%s'", cdate, xvar))
  }
  
  if (missing(bio_data) || nrow(bio_data) == 0) {
    stop("bio_data must contain at least 1 row")
  }
  
  # Validate bio data structure
  if (!bdate %in% names(bio_data)) {
    stop(sprintf("bio_data must contain column '%s'", bdate))
  }
  
  # Add integer dates to data frames
  climate_data$date_int <- convert_dates_to_int(climate_data[[cdate]])
  bio_data$date_int <- convert_dates_to_int(bio_data[[bdate]])
  
  # Calculate maximum possible range based on climate data
  max_climate_days <- max(climate_data$date_int)
  min_climate_days <- min(climate_data$date_int)
  max_possible_range <- max_climate_days - min_climate_days
  
  # Check if any requested range exceeds the available data
  max_requested_range <- max(range)
  if (max_requested_range > max_possible_range) {
    stop(sprintf(
      "Requested range (%d days) exceeds available climate data range (%d days).\nMaximum possible range is 0 to %d.",
      max_requested_range,
      max_possible_range,
      max_possible_range
    ))
  }
  
  # Generate all valid range combinations
  range_combinations <- expand.grid(start_days = range, end_days = range)
  range_combinations <- range_combinations[range_combinations$end_days >= range_combinations$start_days, ]
  
  # Initialize list to store results for each range combination
  results_list <- list()
  
  # Process each range combination
  for (i in seq_len(nrow(range_combinations))) {
    start_days <- range_combinations$start_days[i]
    end_days <- range_combinations$end_days[i]
    
    # Calculate start and end dates based on type
    if (type == "relative") {
      start_dates_int <- bio_data$date_int - start_days
      end_dates_int <- bio_data$date_int - end_days
    } else { # absolute mode
      # Format bio dates and refday as date objects
      bio_dates_as_date <- as.Date(bio_data[[bdate]], format = "%d/%m/%Y")
      refday_parts_as_date <- as.Date(refday, format = "%d/%m/%Y")
      
      ## Create new bio data dates using refday
      bio_data$date_int <- convert_dates_to_int(as.Date(paste(lubridate::day(refday_parts_as_date),
                                                                  lubridate::month(refday_parts_as_date),
                                                                  lubridate::year(bio_dates_as_date),
                                                                  sep = "/"), format = "%d/%m/%Y"))
      
      # Calculate start and end dates
      start_dates_int <- bio_data$date_int - start_days
      end_dates_int <- bio_data$date_int - end_days
    }
    
    # Initialize vectors for this combination
    bio_dates_int <- bio_data$date_int
    summary_values <- numeric(length(bio_dates_int))
    
    # Calculate summary for each bio date
    for (j in seq_along(bio_dates_int)) {
      # Filter data for the date range
      date_range_data <- climate_data[climate_data$date_int >= end_dates_int[j] & 
                                        climate_data$date_int <= start_dates_int[j], ]
      
      # Calculate summary statistic for this range
      summary_values[j] <- fn(date_range_data[[xvar]])
    }
    
    # Convert integer dates to character format
    # Character format is more robust than Date
    bio_dates <- format(as.Date(bio_dates_int), "%d/%m/%Y")
    start_dates <- format(as.Date(start_dates_int), "%d/%m/%Y")
    end_dates <- format(as.Date(end_dates_int), "%d/%m/%Y")
    
    # Create results dataframe for this combination
    results_list[[i]] <- data.frame(
      Date = bio_dates,
      Start_Date = start_dates,
      End_Date = end_dates,
      Start_Day = rep(start_days, length(bio_dates)),
      End_Day = rep(end_days, length(bio_dates)),
      Summary_Value = summary_values,
      stringsAsFactors = FALSE
    )
    
    # Name the list element with the range combination
    names(results_list)[i] <- sprintf("%d_%d", start_days, end_days)
  }
  
  return(results_list)
} 