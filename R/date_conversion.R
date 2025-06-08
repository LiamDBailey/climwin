#' Convert Between Date Formats
#'
#' This function converts dates between character strings (DD/MM/YYYY) and integers (days since earliest date).
#' The min_date parameter is used as the origin for integer conversion.
#'
#' @param dates A vector of dates in character format (DD/MM/YYYY)
#' @param min_date Optional. The reference date to use as day 0. If NULL, uses the earliest date in the input.
#'
#' @return A list containing:
#'   \item{date_int}{Integer vector of days since min_date}
#'   \item{min_date}{The reference date used (as Date object)}
#'
#' @examples
#' dates <- c("01/01/1979", "02/01/1979", "03/01/1979")
#' result <- convert_dates_to_int(dates)
#' 
#' @export
convert_dates_to_int <- function(dates, min_date = NULL) {
  
  # Handle empty input
  if (length(dates) == 0) {
    stop("dates missing")
  }
  
  # Convert input dates to Date objects
  dates_as_date <- as.Date(dates, format = "%d/%m/%Y")
  
  # Validate dates
  if (any(is.na(dates_as_date))) {
    stop("All dates must be in format 'DD/MM/YYYY'")
  }
  
  # Use provided min_date or find earliest date
  if (is.null(min_date)) {
    date_int <- as.integer(dates_as_date)
  } else {
    min_date <- as.Date(min_date, format = "%d/%m/%Y")
    if (is.na(min_date)) {
      stop("min_date must be in format 'DD/MM/YYYY'")
    }
    # Convert to integers
    date_int <- as.integer(dates_as_date - min_date) 
  }
  
  return(date_int)
  
} 