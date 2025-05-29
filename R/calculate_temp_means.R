#' Calculate Mean Temperatures for Different Date Ranges
#'
#' This function calculates the mean temperature for every possible combination of date ranges
#' within the specified range. For example, if range = c(0, 2), it will calculate means for
#' ranges 0-0, 0-1, 0-2, 1-1, 1-2, and 2-2.
#'
#' @param range A numeric vector specifying the number of days to look back from the reference date.
#'              For example, 0 represents the reference date itself, while 100 represents 100 days
#'              before the reference date.
#'
#' @return A data frame containing:
#'   \item{Start_Date}{The beginning date of each range}
#'   \item{End_Date}{The end date of each range}
#'   \item{Mean_Temperature}{The mean temperature for the specified range}
#'
#' @examples
#' # Calculate means for all possible combinations in range 0 to 2
#' result <- calculate_temp_means(0:2)
#'
#' # Calculate means for specific ranges
#' result <- calculate_temp_means(c(0, 30, 60, 90))
#'
#' @export
calculate_temp_means <- function(range) {
  # Read the climate data
  climate_data <- read.csv("MassClimate.csv")
  
  # Convert Date column to Date format
  climate_data$Date <- as.Date(climate_data$Date, format = "%d/%m/%Y")
  
  # Create a reference date (01/01/1979)
  reference_date <- as.Date("1979-01-01")
  
  # Initialize empty vectors to store results
  start_dates <- character()
  end_dates <- character()
  mean_temps <- numeric()
  
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
        
        # Calculate mean temperature for this range
        mean_temp <- mean(date_range_data$Temp)
        
        # Store results
        start_dates <- c(start_dates, as.character(start_date))
        end_dates <- c(end_dates, as.character(end_date))
        mean_temps <- c(mean_temps, mean_temp)
      }
    }
  }
  
  # Create and return results dataframe
  results <- data.frame(
    Start_Date = start_dates,
    End_Date = end_dates,
    Mean_Temperature = mean_temps
  )
  
  return(results)
} 