#' Fit a Weighted Climate Window Using a Density Function
#'
#' Computes a weighted mean of climate data for each row in \code{bio_data}
#' by evaluating \code{dfun} on a normalised [0, 1] domain that spans
#' \code{range}, then applying the resulting weights.
#'
#' @param range A numeric vector specifying the time steps to consider.
#' @param bio_data A data frame containing biological data with a date column.
#' @param climate_data A data frame containing climate data.
#' @param cdate Character string specifying the date column in climate_data.
#' @param bdate Character string specifying the date column in bio_data.
#' @param xvar Character string specifying the climate variable column.
#' @param dfun A density function with signature \code{function(x, ...)} where
#'   \code{x} is a numeric vector on [0, 1].  Additional arguments are the
#'   distribution parameters.
#' @param par A numeric vector of parameters passed to \code{dfun} after
#'   \code{x} (positionally).
#' @param cinterval Character string specifying the temporal resolution: \code{"day"}
#'   (default), \code{"week"}, or \code{"month"}. When \code{"month"} or \code{"week"},
#'   \code{climate_data} must be pre-aggregated with \code{\link{trans_clim_interval}}.
#'
#' @return A list with \code{bio_data} (with added \code{climate} column)
#'   and \code{weights}.
#'
#' @examples
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))
#'
#' out <- fit_weights(range = 0:100, bio_data = Mass, climate_data = Climate,
#'                    cdate = "Date", bdate = "Date", xvar = "Temp",
#'                    dfun = dweibull, par = c(2, 0.5))
#'
#' @export
fit_weights <- function(range,
                        bio_data,
                        climate_data,
                        cdate,
                        bdate,
                        xvar,
                        dfun,
                        par,
                        cinterval = "day") {

  processed_data <- process_data(
    climate_data = climate_data,
    bio_data     = bio_data,
    range        = range,
    cdate        = cdate,
    bdate        = bdate,
    xvar         = xvar,
    spatial      = NULL,
    type         = "relative",
    refday       = NULL,
    cohort       = NULL,
    cinterval    = cinterval
  )

  bio_data        <- processed_data$bio_data
  bio_xvar_ranges <- processed_data$bio_xvar_ranges

  x       <- seq(0, 1, length.out = length(range))
  weights <- do.call(dfun, c(list(x), as.list(par)))
  weights[is.na(weights) | is.infinite(weights)] <- 0
  if (sum(weights) == 0) weights <- weights + 1
  weights <- weights / sum(weights)

  climate <- apply(bio_xvar_ranges, MARGIN = 2, FUN = function(col) {
    sum(col * weights, na.rm = TRUE)
  })

  bio_data$climate <- climate
  list(bio_data = bio_data, weights = weights)
}
