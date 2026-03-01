#' Append a Climate Threshold Column to Climate Data
#'
#' Adds a new column \code{threshold} to \code{climate_data} by applying an
#' upper and/or lower threshold to a climate variable. Values that do not meet
#' the threshold criteria are set to 0; values that do are either kept as-is
#' or set to 1 when \code{binary = TRUE}.
#'
#' @param climate_data A data frame containing climate data, as used in
#'   \code{\link{run_slidingwin}}.
#' @param xvar Character string specifying the climate variable column to
#'   threshold. Defaults to \code{"Temp"}.
#' @param upper Numeric. Upper threshold. Values at or above \code{upper} are
#'   retained; values strictly below are set to 0. At least one of
#'   \code{upper} or \code{lower} must be provided.
#' @param lower Numeric. Lower threshold. Values strictly below \code{lower}
#'   are retained; values at or above are set to 0. At least one of
#'   \code{upper} or \code{lower} must be provided.
#' @param binary Logical. If \code{TRUE}, retained values are set to 1.
#'   If \code{FALSE} (default), retained values keep their original
#'   \code{xvar} value.
#'
#' @details
#' The threshold rules are applied as follows:
#' \describe{
#'   \item{upper only}{Values \eqn{\ge} \code{upper} are retained; values
#'     \eqn{<} \code{upper} become 0.}
#'   \item{lower only}{Values \eqn{<} \code{lower} are retained; values
#'     \eqn{\ge} \code{lower} become 0.}
#'   \item{both}{Values strictly between \code{lower} and \code{upper}
#'     (i.e. \eqn{lower < x < upper}) are retained; all others become 0.}
#' }
#'
#' @return The original \code{climate_data} with an additional column
#'   \code{threshold} containing the thresholded values.
#'
#' @examples
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#'
#' # Continuous: keep temperatures >= 10, zero out the rest
#' Climate_upper <- append_clim_threshold(Climate, upper = 10, binary = FALSE)
#'
#' # Binary indicator: 1 where temperature >= 10, else 0
#' Climate_bin <- append_clim_threshold(Climate, upper = 10, binary = TRUE)
#'
#' # Keep only temperatures strictly below 5
#' Climate_lower <- append_clim_threshold(Climate, lower = 5, binary = FALSE)
#'
#' # Keep temperatures strictly between 5 and 15
#' Climate_range <- append_clim_threshold(Climate, lower = 5, upper = 15,
#'                                        binary = FALSE)
#'
#' @export
append_clim_threshold <- function(climate_data,
                                   xvar = "Temp",
                                   upper = NULL,
                                   lower = NULL,
                                   binary = FALSE) {

  ### ARGUMENT CHECKS ####
  validate_arg("climate_data", climate_data, required = TRUE, type = "data.frame",
               additional_checks = function(x) {
                 if (nrow(x) == 0) stop("must contain at least 1 row")
                 if (!xvar %in% names(x))
                   stop(sprintf("must contain column '%s'", xvar))
               })

  if (is.null(upper) && is.null(lower)) {
    stop("At least one of 'upper' or 'lower' must be provided.")
  }

  if (!is.null(upper)) {
    validate_arg("upper", upper, required = TRUE, type = "numeric",
                 additional_checks = function(x) {
                   if (length(x) != 1 || is.na(x))
                     stop("must be a single numeric value")
                 })
  }

  if (!is.null(lower)) {
    validate_arg("lower", lower, required = TRUE, type = "numeric",
                 additional_checks = function(x) {
                   if (length(x) != 1 || is.na(x))
                     stop("must be a single numeric value")
                 })
  }

  validate_arg("binary", binary, required = FALSE, type = "logical",
               additional_checks = function(x) {
                 if (length(x) != 1 || is.na(x))
                   stop("must be a single logical value")
               })

  ### APPLY THRESHOLD ####
  xvar_vals <- climate_data[[xvar]]

  if (!is.null(upper) && is.null(lower)) {
    # Values >= upper are retained; values < upper become 0
    if (binary) {
      threshold <- ifelse(xvar_vals >= upper, 1L, 0L)
    } else {
      threshold <- ifelse(xvar_vals >= upper, xvar_vals, 0)
    }
  } else if (is.null(upper) && !is.null(lower)) {
    # Values < lower are retained; values >= lower become 0
    if (binary) {
      threshold <- ifelse(xvar_vals < lower, 1L, 0L)
    } else {
      threshold <- ifelse(xvar_vals < lower, xvar_vals, 0)
    }
  } else {
    # Values strictly between lower and upper are retained; all others become 0
    if (binary) {
      threshold <- ifelse(xvar_vals > lower & xvar_vals < upper, 1L, 0L)
    } else {
      threshold <- ifelse(xvar_vals > lower & xvar_vals < upper, xvar_vals, 0)
    }
  }

  climate_data$threshold <- threshold
  return(climate_data)
}
