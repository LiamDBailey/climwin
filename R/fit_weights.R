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
#'            par[1] = shape, par[2] = scale. Required.
#'
#' @return A data frame containing the original bio_data with an additional column 'climate'
#'         containing the weighted mean climate values for each row
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' 
#' results <- fit_weights(range = 0:100, 
#'                         bio_data = Mass,
#'                         climate_data = Climate,
#'                         cdate = "Date", bdate = "Date",
#'                         xvar = "Temp",
#'                         par = c(2, 50, 0))
#'                         
#' # Access the weighted climate data
#' head(results)
#'
#' @export
fit_weights <- function(range,
                          bio_data,
                          climate_data,
                          cdate,
                          bdate,
                          xvar,
                          par) {
  
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
  bio_xvar_ranges <- processed_data$bio_xvar_ranges
  
  # Calculate weights for each day in range using manual Weibull function
  weights <- dweibull(seq(0, 1, length.out = length(range)), par[1], par[2])
    # weibull3(seq(0, 1, length.out = length(range)), par[1], par[2], par[3])
  
  # Normalize weights to sum to 1
  # Replace NA and Inf with 0
  weights[is.na(weights) | is.infinite(weights)] <- 0
  if (sum(weights) == 0){
    weights <- weights + 1
  }
  weights <- weights / sum(weights)
  
  # Apply weighted mean to each bio_data row
  climate <- apply(bio_xvar_ranges, MARGIN = 2, FUN = function(x) {
    # Apply weights to the climate values within the range
    weighted_sum <- sum(x * weights, na.rm = TRUE)
    return(weighted_sum)
  })
  
  bio_data$climate <- climate
  
  output <- list(bio_data = bio_data,
                 weights = weights)
  
  # Return bio_data with the new climate column
  return(output)
}

#' Fit Weighted Window Analysis Using a Uniform Distribution
#'
#' Creates weighted means of climate data using equal weights between two
#' time-step boundaries. All time steps within [par[1], par[2]] receive
#' equal weight; all others receive zero weight.
#'
#' @param range A numeric vector specifying the time steps to consider.
#' @param bio_data A data frame containing biological data with a date column.
#' @param climate_data A data frame containing climate data.
#' @param cdate Character string specifying the date column in climate_data.
#' @param bdate Character string specifying the date column in bio_data.
#' @param xvar Character string specifying the climate variable column.
#' @param par A numeric vector of length 2: par[1] = window start,
#'   par[2] = window end, both on the same scale as \code{range}.
#'   par[1] must be <= par[2].
#'
#' @return A list with \code{bio_data} (with added \code{climate} column)
#'   and \code{weights}.
#'
#' @export
fit_weights_uniform <- function(range,
                                bio_data,
                                climate_data,
                                cdate,
                                bdate,
                                xvar,
                                par) {

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

  bio_data        <- processed_data$bio_data
  bio_xvar_ranges <- processed_data$bio_xvar_ranges

  # Uniform weights: 1 inside [par[1], par[2]], 0 outside
  weights <- as.numeric(range >= par[1] & range <= par[2])
  if (sum(weights) == 0) weights <- weights + 1
  weights <- weights / sum(weights)

  climate <- apply(bio_xvar_ranges, MARGIN = 2, FUN = function(x) {
    sum(x * weights, na.rm = TRUE)
  })

  bio_data$climate <- climate

  list(bio_data = bio_data, weights = weights)
}
