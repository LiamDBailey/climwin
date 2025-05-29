#' Convert Between Date Formats
#'
#' This function converts dates between character strings (DD/MM/YYYY) and integers (days since earliest date).
#' It also provides a lookup table for converting back to character format.
#'
#' @param dates A vector of dates in character format (DD/MM/YYYY)
#' @param min_date Optional. The reference date to use as day 0. If NULL, uses the earliest date in the input.
#'
#' @return A list containing:
#'   \item{date_int}{Integer vector of days since min_date}
#'   \item{min_date}{The reference date used (as Date object)}
#'   \item{lookup_table}{Data frame mapping integers to character dates}
#'
#' @examples
#' dates <- c("01/01/1979", "02/01/1979", "03/01/1979")
#' result <- convert_dates_to_int(dates)
#' 
#' @export
convert_dates_to_int <- function(dates, min_date = NULL) {
  # Handle empty input
  if (length(dates) == 0) {
    if (is.null(min_date)) {
      stop("Cannot convert empty dates without a min_date")
    }
    min_date <- as.Date(min_date, format = "%d/%m/%Y")
    if (is.na(min_date)) {
      stop("min_date must be in format 'DD/MM/YYYY'")
    }
    return(list(
      date_int = integer(0),
      min_date = min_date,
      lookup_table = data.frame(
        date_int = integer(0),
        date_char = character(0)
      )
    ))
  }
  
  # Convert input dates to Date objects
  dates_as_date <- as.Date(dates, format = "%d/%m/%Y")
  
  # Validate dates
  if (any(is.na(dates_as_date))) {
    stop("All dates must be in format 'DD/MM/YYYY'")
  }
  
  # Use provided min_date or find earliest date
  if (is.null(min_date)) {
    min_date <- min(dates_as_date)
  } else {
    min_date <- as.Date(min_date, format = "%d/%m/%Y")
    if (is.na(min_date)) {
      stop("min_date must be in format 'DD/MM/YYYY'")
    }
  }
  
  # Convert to integers
  date_int <- as.integer(dates_as_date - min_date)
  
  # Create lookup table
  max_int <- max(date_int)
  lookup_table <- data.frame(
    date_int = seq(0, max_int),
    date_char = format(min_date + seq(0, max_int), "%d/%m/%Y")
  )
  
  return(list(
    date_int = date_int,
    min_date = min_date,
    lookup_table = lookup_table
  ))
} 