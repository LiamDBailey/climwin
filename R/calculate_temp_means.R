#' Calculate Mean Temperatures for Different Date Ranges
#'
#' This function calculates the mean temperature for various date ranges relative to a reference date
#' (January 1st, 1979). It processes climate data from a CSV file and returns mean temperatures
#' for each specified range.
#'
#' @param range A numeric vector specifying the number of days to look back from the reference date.
#'              For example, 0 represents the reference date itself, while 100 represents 100 days
#'              before the reference date.
#'
#' @return A data frame containing:
#'   \item{Start_Date}{The beginning date of each range}
#'   \item{End_Date}{The reference date (1979-01-01)}
#'   \item{Mean_Temperature}{The mean temperature for the specified range}
#'
#' @examples
#' # Calculate means for ranges from 0 to 100 days
#' result <- calculate_temp_means(0:100)
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
  
  # Loop through each possible range
  for (i in range) {
    # Calculate start date (i days before reference date)
    start_date <- reference_date - i
    end_date <- reference_date
    
    # Filter data for the date range
    date_range_data <- climate_data[climate_data$Date >= start_date & 
                                   climate_data$Date <= end_date, ]
    
    # Calculate mean temperature for this range
    mean_temp <- mean(date_range_data$Temp)
    
    # Store results
    start_dates <- c(start_dates, as.character(start_date))
    end_dates <- c(end_dates, as.character(end_date))
    mean_temps <- c(mean_temps, mean_temp)
  }
  
  # Create and return results dataframe
  results <- data.frame(
    Start_Date = start_dates,
    End_Date = end_dates,
    Mean_Temperature = mean_temps
  )
  
  return(results)
} 