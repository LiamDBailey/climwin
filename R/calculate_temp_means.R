#' Calculate Climate Variable Means for Different Date Ranges
#'
#' This function calculates the mean (or other summary statistic) of a climate variable for every possible 
#' combination of date ranges within the specified range, for each date in the biological data. For example, 
#' if range = c(0, 2), it will calculate means for ranges 0-0, 0-1, 0-2, 1-1, 1-2, and 2-2 for each date
#' in the biological data.
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
#' # for each date in the biological data
#' bio_data <- read.csv("Mass.csv")
#' result <- calculate_temp_means(0:2, bio_data = bio_data)
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
#' result <- calculate_temp_means(0:2, bio_data = bio_data, fn = median)
#'
#' @export
calculate_temp_means <- function(range, 
                               climate_data = NULL,
                               bio_data,
                               cdate = "Date",
                               bdate = "Date",
                               xvar = "Temp",
                               fn = mean) {
  
  # Validate function first
  if (!is.function(fn)) {
    stop("fn must be a function")
  }
  
  # Read the climate data if not provided
  if (is.null(climate_data)) {
    climate_data <- read.csv("MassClimate.csv")
  }
  
  # Validate climate data structure
  if (!all(c(cdate, xvar) %in% names(climate_data))) {
    stop(sprintf("climate_data must contain columns '%s' and '%s'", cdate, xvar))
  }
  
  # Validate bio data structure
  if (!bdate %in% names(bio_data)) {
    stop(sprintf("bio_data must contain column '%s'", bdate))
  }
  
  # Handle empty data frames
  if (nrow(climate_data) == 0 || nrow(bio_data) == 0) {
    return(list(data.frame(
      Bio_Date = character(0),
      Start_Date = character(0),
      End_Date = character(0),
      Start_Day = integer(0),
      End_Day = integer(0),
      Summary_Value = numeric(0),
      stringsAsFactors = FALSE
    )))
  }
  
  # Convert dates to integers
  climate_dates <- convert_dates_to_int(climate_data[[cdate]])
  bio_dates <- convert_dates_to_int(bio_data[[bdate]], min_date = climate_dates$min_date)
  
  # Add integer dates to data frames
  climate_data$date_int <- climate_dates$date_int
  bio_data$date_int <- bio_dates$date_int
  
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
    
    # Calculate start and end dates for all bio dates at once
    start_dates_int <- bio_data$date_int - start_days
    end_dates_int <- bio_data$date_int - end_days
    
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
    
    # Convert integer dates back to character format
    bio_dates <- climate_dates$lookup_table$date_char[match(bio_dates_int, climate_dates$lookup_table$date_int)]
    start_dates <- climate_dates$lookup_table$date_char[match(start_dates_int, climate_dates$lookup_table$date_int)]
    end_dates <- climate_dates$lookup_table$date_char[match(end_dates_int, climate_dates$lookup_table$date_int)]
    
    # Create results dataframe for this combination
    results_list[[i]] <- data.frame(
      Bio_Date = bio_dates,
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