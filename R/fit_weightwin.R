#' Fit Weighted Window Analysis Using Weibull Function
#'
#' This function creates weighted means of climate data for each row in bio_data across a specified range.
#' The weights are drawn from a Weibull function parameterized by shape, scale, and location parameters.
#'
#' @param range A numeric vector specifying the number of days to look back from each date in bio_data.
#'              For example, 0 represents the date itself, while 100 represents 100 days before that date.
#' @param bio_data A data frame containing biological data with a date column. Required.
#' @param climate_data A data frame containing climate data. Required.
#' @param cdate Character string specifying the name of the date column in climate_data. Defaults to "Date".
#' @param bdate Character string specifying the name of the date column in bio_data. Defaults to "Date".
#' @param xvar Character string specifying the name of the climate variable column in climate_data. Defaults to "Temp".
#' @param par A numeric vector of length 3 containing the Weibull function parameters:
#'            par[1] = shape, par[2] = scale, par[3] = location. Required.
#'
#' @return A data frame containing the original bio_data with an additional column 'climate'
#'         containing the weighted mean climate values for each row
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' 
#' # Weibull parameters: shape = 2, scale = 50, location = 0
#' results <- fit_weightwin(range = 0:100, 
#'                         bio_data = Mass,
#'                         climate_data = Climate,
#'                         par = c(2, 50, 0))
#'                         
#' # Access the weighted climate data
#' head(results)
#'
#' @export
fit_weightwin <- function(range,
                         bio_data,
                         climate_data,
                         cdate = "Date",
                         bdate = "Date",
                         xvar = "Temp",
                         par) {
  
  ### ARGUMENT CHECKS ####
  # Validate par parameter
  validate_arg("par", par, required = TRUE, type = "numeric",
               additional_checks = function(x) {
                 if(length(x) != 3) stop("par must be a numeric vector of length 3")
                 if(any(x <= 0)) stop("all par values must be positive")
               })
  
  # Extract Weibull parameters
  shape <- par[1]
  scale <- par[2]
  location <- par[3]
  
  ### PROCESS DATA ####
  processed_data <- process_data(
    climate_data = climate_data,
    bio_data = bio_data,
    range = range,
    cdate = cdate,
    bdate = bdate,
    xvar = xvar,
    spatial = NULL,
    type = "relative",
    refday = NULL,
    cohort = NULL
  )
  
  # Extract processed data
  bio_data <- processed_data$bio_data
  bio_int_ranges <- processed_data$bio_int_ranges
  bio_data_row <- processed_data$bio_data_row
  bio_xvar_ranges <- processed_data$bio_xvar_ranges
  
  # Calculate weights for each day in range using manual Weibull function
  range_days <- seq_along(range)
  weights <- sapply(range_days, function(x) {
    # Weibull function: shape/scale * ((x-location)/scale)^(shape-1) * exp(-((x-location)/scale)^shape)
    # For x > location, otherwise 0
    if (x < location) return(0)
    x_norm <- (x - location) / scale
    return((shape / scale) * (x_norm^(shape - 1)) * exp(-(x_norm^shape)))
  })
  
  # Normalize weights to sum to 1
  weights <- weights / sum(weights)
  
  # Apply weighted mean to each bio_data row
  climate <- apply(bio_xvar_ranges, MARGIN = 2, FUN = function(x) {
    # Apply weights to the climate values within the range
    weighted_sum <- sum(x * weights, na.rm = TRUE)
    return(weighted_sum)
  })
  
  # Reorder the weighted climate data to match bio_data rows
  bio_data$climate <- climate[order(bio_data_row)]
  
  # Return bio_data with the new climate column
  return(bio_data)
}
