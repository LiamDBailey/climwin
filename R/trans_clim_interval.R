#' Transform Climate Data by Temporal Interval
#'
#' Aggregates one or more climate variable columns in \code{climate_data}
#' over a specified temporal interval ("day", "week", or "month"). When
#' \code{cinterval = "day"} no aggregation is performed and the data are
#' returned unchanged. For "week" and "month" each row in the output
#' represents one aggregated period.
#'
#' @param climate_data A data frame containing at least a date column and
#'   the climate variable columns listed in \code{xvar}.
#' @param xvar Character vector of column names in \code{climate_data} to
#'   aggregate. Must be length >= 1. Defaults to \code{"Temp"}.
#' @param cdate Character string. Name of the date column in
#'   \code{climate_data}. The column may be a character vector in
#'   \code{"DD/MM/YYYY"} format or a \code{Date} object. Defaults to
#'   \code{"Date"}.
#' @param cinterval Character string. Temporal interval over which to
#'   aggregate. Must be \code{"day"} (no aggregation), \code{"week"}
#'   (7-day blocks from the first date in the dataset), or \code{"month"}
#'   (calendar month). Defaults to \code{"day"}.
#' @param aggfunc A function or a list of functions to apply to each
#'   \code{xvar} column. If a single function is provided it is applied to
#'   all \code{xvar} columns. If a list, its length must equal the length of
#'   \code{xvar} and each function is applied to the corresponding column.
#'   Any function that accepts a numeric vector and returns a single value is
#'   valid (e.g. \code{mean}, \code{sum}, \code{max}, \code{min}). Defaults
#'   to \code{mean}.
#'
#' @details
#' Only the \code{cdate} and \code{xvar} columns are retained in the output
#' when \code{cinterval} is \code{"week"} or \code{"month"}; all other
#' columns are dropped. When \code{cinterval = "day"} all columns are
#' preserved.
#'
#' Week boundaries are determined from the earliest date in
#' \code{climate_data}: week 1 starts on that date, week 2 starts 7 days
#' later, and so on.
#'
#' @return A data frame with one row per aggregated period containing the
#'   \code{cdate} column (as \code{Date}) and the specified \code{xvar}
#'   columns with aggregated values.
#'
#' @examples
#' data("MassClimate")
#'
#' # Aggregate to monthly means
#' Climate_monthly <- trans_clim_interval(MassClimate, cinterval = "month")
#'
#' # Aggregate Temp by weekly mean and apply a threshold afterwards
#' Climate_weekly <- trans_clim_interval(MassClimate, cinterval = "week")
#' Climate_thresh <- append_clim_threshold(Climate_weekly, upper = 10)
#'
#' @export
trans_clim_interval <- function(climate_data,
                                xvar = "Temp",
                                cdate = "Date",
                                cinterval = "day",
                                aggfunc = mean) {

  ### ARGUMENT CHECKS ####
  validate_arg("climate_data", climate_data,
               required = TRUE, type = "data.frame",
               additional_checks = function(x) {
                 if (nrow(x) == 0) stop("must contain atleast 1 row")
               })

  validate_arg("cdate", cdate,
               required = FALSE, type = "character",
               additional_checks = function(x) {
                 if (length(x) != 1)
                   stop("must be a single character string")
                 if (!x %in% names(climate_data))
                   stop(sprintf("column '%s' not found in climate_data", x))
               })

  validate_arg("xvar", xvar,
               required = TRUE, type = "character",
               additional_checks = function(x) {
                 if (length(x) == 0)
                   stop("must contain at least 1 element")
                 missing_cols <- setdiff(x, names(climate_data))
                 if (length(missing_cols) > 0)
                   stop(sprintf("column(s) not found in climate_data: %s",
                                paste(missing_cols, collapse = ", ")))
               })

  validate_arg("cinterval", cinterval,
               required = FALSE, type = "character",
               additional_checks = function(x) {
                 if (length(x) != 1 || !x %in% c("day", "week", "month"))
                   stop("must be 'day', 'week', or 'month'")
               })

  # Normalise aggfunc to a list so it is consistent for the rest of the function
  if (is.function(aggfunc)) {
    aggfunc <- list(aggfunc)
  }

  validate_arg("aggfunc", aggfunc,
               required = FALSE, type = "list",
               additional_checks = function(x) {
                 if (!all(sapply(x, is.function)))
                   stop("all elements must be functions")
                 if (length(x) != 1 && length(x) != length(xvar))
                   stop(sprintf(
                     "length must be 1 or %d (length of xvar); got %d",
                     length(xvar), length(x)
                   ))
               })

  # Expand singleton aggfunc to match length of xvar
  if (length(aggfunc) == 1) {
    aggfunc <- rep(aggfunc, length(xvar))
  }

  ### DATE CONVERSION ####
  raw_dates <- climate_data[[cdate]]
  if (is.character(raw_dates)) {
    parsed_dates <- as.Date(raw_dates, format = "%d/%m/%Y")
    if (any(is.na(parsed_dates))) {
      stop(sprintf("Column '%s' must be in 'DD/MM/YYYY' format.", cdate))
    }
    climate_data[[cdate]] <- parsed_dates
  } else if (!inherits(raw_dates, "Date")) {
    stop(sprintf(
      "Column '%s' must be character ('DD/MM/YYYY') or Date class.", cdate
    ))
  }

  ### INTERVAL AGGREGATION ####
  if (cinterval == "day") {
    return(climate_data)
  }

  # Replace each date with the label for its period
  if (cinterval == "month") {
    climate_data[[cdate]] <- as.Date(format(climate_data[[cdate]], "%Y-%m-01"))
  } else {
    min_date   <- min(climate_data[[cdate]])
    days_since <- as.integer(climate_data[[cdate]] - min_date)
    climate_data[[cdate]] <- min_date + floor(days_since / 7) * 7
  }

  # Aggregate each xvar column within each period
  date_groups  <- climate_data[[cdate]]
  unique_dates <- sort(unique(date_groups))

  result <- data.frame(setNames(list(unique_dates), cdate),
                       stringsAsFactors = FALSE)

  for (i in seq_along(xvar)) {
    fn  <- aggfunc[[i]]
    col <- xvar[i]
    # tapply returns a named array; as.vector strips dim/names giving a plain
    # numeric vector in factor-level (chronological) order, matching unique_dates
    result[[col]] <- as.vector(tapply(climate_data[[col]], date_groups, fn))
  }

  return(result)
}
