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
#' @return A data frame containing:
#'   \item{Bio_Date}{The date from bio_data (character string in DD/MM/YYYY format)}
#'   \item{Start_Date}{The beginning date of each range (character string in DD/MM/YYYY format)}
#'   \item{End_Date}{The end date of each range (character string in DD/MM/YYYY format)}
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
  
  # Convert date columns to Date format
  climate_data[[cdate]] <- as.Date(climate_data[[cdate]], format = "%d/%m/%Y")
  bio_data[[bdate]] <- as.Date(bio_data[[bdate]], format = "%d/%m/%Y")
  
  # Find the earliest date in either dataset
  min_date <- min(min(climate_data[[cdate]]), min(bio_data[[bdate]]))
  
  # Convert dates to integers (days since min_date)
  climate_data$date_int <- as.integer(climate_data[[cdate]] - min_date)
  bio_data$date_int <- as.integer(bio_data[[bdate]] - min_date)
  
  # Create a lookup table for converting back to dates
  date_lookup <- data.frame(
    date_int = seq(0, max(climate_data$date_int)),
    date_char = format(min_date + seq(0, max(climate_data$date_int)), "%d/%m/%Y")
  )
  
  # Validate function
  if (!is.function(fn)) {
    stop("fn must be a function")
  }
  
  # Initialize empty vectors to store results
  bio_dates_int <- integer()
  start_dates_int <- integer()
  end_dates_int <- integer()
  summary_values <- numeric()
  
  # Loop through each date in bio_data
  for (bio_date_int in bio_data$date_int) {
    # Generate all possible combinations of start and end dates
    for (start_days in range) {
      for (end_days in range) {
        # Only process if end_days >= start_days
        if (end_days >= start_days) {
          # Calculate start and end dates as integers
          start_date_int <- bio_date_int - start_days
          end_date_int <- bio_date_int - end_days
          
          # Filter data for the date range
          date_range_data <- climate_data[climate_data$date_int >= end_date_int & 
                                        climate_data$date_int <= start_date_int, ]
          
          # Calculate summary statistic for this range
          summary_value <- fn(date_range_data[[xvar]])
          
          # Store results as integers
          bio_dates_int <- c(bio_dates_int, bio_date_int)
          start_dates_int <- c(start_dates_int, start_date_int)
          end_dates_int <- c(end_dates_int, end_date_int)
          summary_values <- c(summary_values, summary_value)
        }
      }
    }
  }
  
  # Convert integer dates back to character format
  bio_dates <- date_lookup$date_char[match(bio_dates_int, date_lookup$date_int)]
  start_dates <- date_lookup$date_char[match(start_dates_int, date_lookup$date_int)]
  end_dates <- date_lookup$date_char[match(end_dates_int, date_lookup$date_int)]
  
  # Create and return results dataframe
  results <- data.frame(
    Bio_Date = bio_dates,
    Start_Date = start_dates,
    End_Date = end_dates,
    Summary_Value = summary_values,
    stringsAsFactors = FALSE
  )
  
  return(results)
} 