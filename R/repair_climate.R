#' Repair Climate Data by Filling Missing Dates and Interpolating Missing Values
#'
#' This function repairs climate data by ensuring continuous date sequences and interpolating
#' missing climate variable values. It adds rows for missing dates and replaces NA/Inf values
#' with interpolated values based on surrounding data.
#'
#' @param climate_data A data frame containing climate data
#' @param cdate Character string specifying the name of the date column. Defaults to "Date".
#' @param xvar Character vector specifying the names of the climate variable columns to repair. Defaults to "Temp".
#'
#' @return A data frame with repaired climate data containing:
#'         - All dates from min to max in continuous sequence
#'         - Interpolated climate variable values for any previously missing data
#'         - Original data preserved where possible
#'
#' @examples
#' # Example with missing dates and NA values
#' climate_data <- data.frame(
#'   Date = c("01/01/1979", "02/01/1979", "04/01/1979", "05/01/1979"),
#'   Temp = c(10, 15, 13, 12),
#'   Rain = c(1, 2, 3, 4)
#' )
#' 
#' repaired_data <- repair_climate(climate_data, cdate = "Date", xvar = c("Temp", "Rain"))
#' 
#' @export
#' @importFrom dplyr bind_rows
repair_climate <- function(climate_data, cdate, xvar, method = imputeTS::na_interpolation, ...) {
  
  # Validate column names exist
  if (!cdate %in% names(climate_data)) {
    stop(sprintf("Column '%s' not found in climate_data", cdate))
  }
  
  # Ensure xvar is a character vector
  if (!is.character(xvar)) {
    stop("'xvar' must be a character vector")
  }
  
  # Check that all xvar columns exist in climate_data
  missing_cols <- setdiff(xvar, names(climate_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Columns not found in climate_data: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate cdate column is character in DD/MM/YYYY format
  climate_dates <- climate_data[[cdate]]
  if (!is.character(climate_dates)) {
    stop(sprintf("Column '%s' in climate_data must be a character in format 'DD/MM/YYYY'", cdate))
  }
  
  # Convert to Date objects for processing
  converted_dates <- as.Date(climate_dates, format = "%d/%m/%Y")
  if (all(is.na(converted_dates))) {
    stop(sprintf("Column '%s' in climate_data must be in format 'DD/MM/YYYY'", cdate))
  }
  
  ### REPAIR MISSING DATES ####
  # Sort dates and find range
  sorted_dates <- sort(converted_dates)
  min_date <- min(sorted_dates)
  max_date <- max(sorted_dates)
  
  # Create complete date sequence
  complete_dates <- seq.Date(from = min_date, to = max_date, by = "day")
  
  # Find missing dates
  missing_dates <- setdiff(complete_dates, sorted_dates)
  
  # Create new data frame with complete dates
  if (length(missing_dates) > 0) {
    # Create rows for missing dates with NA values
    missing_rows <- data.frame(
      Date = format(missing_dates, format = "%d/%m/%Y"),
      stringsAsFactors = FALSE
    )
    
    # Add NA values for all climate variables
    for (col in xvar) {
      missing_rows[[col]] <- NA
    }
    
    # Combine original data with missing rows
    all_data <- dplyr::bind_rows(climate_data, missing_rows)
    
    # Convert dates back to Date objects for proper sorting
    all_data[[cdate]] <- as.Date(all_data[[cdate]], format = "%d/%m/%Y")
    
    # Sort by date
    all_data <- all_data[order(all_data[[cdate]]), ]
    
    # Convert dates back to DD/MM/YYYY format
    all_data[[cdate]] <- format(all_data[[cdate]], format = "%d/%m/%Y")
    
  } else {
    # No missing dates, just ensure proper format
    all_data <- climate_data
    all_data[[cdate]] <- format(converted_dates, format = "%d/%m/%Y")
  }
  
  ### REPAIR MISSING VALUES ####
  # Repair each climate variable column
  for (col in xvar){
    
    all_data[[col]] <- replace(all_data[[col]], is.infinite(all_data[[col]]), NA_real_)
    all_data[[col]] <- method(all_data[[col]], ...)
    
  }
  
  return(all_data)
  
}
