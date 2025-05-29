#' Calculate Mean Temperatures for Different Date Ranges
#'
#' This function calculates the mean temperature (or other summary statistic) for every possible combination 
#' of date ranges within the specified range. For example, if range = c(0, 2), it will calculate means for
#' ranges 0-0, 0-1, 0-2, 1-1, 1-2, and 2-2.
#'
#' @param range A numeric vector specifying the number of days to look back from the reference date.
#'              For example, 0 represents the reference date itself, while 100 represents 100 days
#'              before the reference date.
#' @param climate_data A data frame containing climate data. If not provided, the function will attempt
#'                    to read from "MassClimate.csv". The data frame must contain columns 'Date' and 'Temp'.
#' @param reference_date A string representing the reference date in format "DD/MM/YYYY". 
#'                      Defaults to "01/01/1979".
#' @param fn A function to use for summarizing the temperature data. Defaults to mean().
#'           Must be a function that can operate on a numeric vector.
#'
#' @return A data frame containing:
#'   \item{Start_Date}{The beginning date of each range}
#'   \item{End_Date}{The end date of each range}
#'   \item{Summary_Temperature}{The summarized temperature for the specified range}
#'
#' @examples
#' # Calculate means for all possible combinations in range 0 to 2
#' result <- calculate_temp_means(0:2)
#'
#' # Use custom climate data
#' my_data <- data.frame(
#'   Date = c("01/01/1979", "02/01/1979"),
#'   Temp = c(10, 12)
#' )
#' result <- calculate_temp_means(0:1, climate_data = my_data)
#'
#' # Use different reference date
#' result <- calculate_temp_means(0:2, reference_date = "15/01/1979")
#'
#' # Use different summary function (e.g., median)
#' result <- calculate_temp_means(0:2, fn = median)
#'
#' @export
calculate_temp_means <- function(range, 
                               climate_data = NULL,
                               reference_date = "01/01/1979",
                               fn = mean) {
  
  # Read the climate data if not provided
  if (is.null(climate_data)) {
    climate_data <- read.csv("MassClimate.csv")
  }
  
  # Validate climate data structure
  if (!all(c("Date", "Temp") %in% names(climate_data))) {
    stop("climate_data must contain columns 'Date' and 'Temp'")
  }
  
  # Convert Date column to Date format
  climate_data$Date <- as.Date(climate_data$Date, format = "%d/%m/%Y")
  
  # Create reference date
  reference_date <- as.Date(reference_date, format = "%d/%m/%Y")
  
  # Validate reference date
  if (is.na(reference_date)) {
    stop("reference_date must be in format 'DD/MM/YYYY'")
  }
  
  # Validate function
  if (!is.function(fn)) {
    stop("fn must be a function")
  }
  
  # Initialize empty vectors to store results
  start_dates <- character()
  end_dates <- character()
  summary_temps <- numeric()
  
  # Generate all possible combinations of start and end dates
  for (start_days in range) {
    for (end_days in range) {
      # Only process if end_days >= start_days
      if (end_days >= start_days) {
        # Calculate start and end dates
        start_date <- reference_date - start_days
        end_date <- reference_date - end_days
        
        # Filter data for the date range
        date_range_data <- climate_data[climate_data$Date >= end_date & 
                                      climate_data$Date <= start_date, ]
        
        # Calculate summary statistic for this range
        summary_temp <- fn(date_range_data$Temp)
        
        # Store results
        start_dates <- c(start_dates, as.character(start_date))
        end_dates <- c(end_dates, as.character(end_date))
        summary_temps <- c(summary_temps, summary_temp)
      }
    }
  }
  
  # Create and return results dataframe
  results <- data.frame(
    Start_Date = start_dates,
    End_Date = end_dates,
    Summary_Temperature = summary_temps
  )
  
  return(results)
} 