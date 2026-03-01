#' Process Climate and Biological Data for Sliding Window Analysis
#'
#' This internal function processes climate and biological data to prepare them for sliding window analysis.
#' It handles spatial grouping, date conversion, and range validation.
#'
#' @param climate_data A data frame containing climate data
#' @param bio_data A data frame containing biological data
#' @param range A numeric vector specifying the number of days to look back
#' @param cdate Character string specifying the name of the date column in climate_data. Defaults to "Date".
#' @param bdate Character string specifying the name of the date column in bio_data. Defaults to "Date".
#' @param xvar Character string specifying the name of the climate variable column. Defaults to "Temp".
#' @param spatial Character string specifying the name of the spatial grouping column. Defaults to NULL.
#' @param type Character string specifying the type of date range calculation. Must be either "relative" (default) or "absolute".
#' @param refday Character string specifying the reference date for absolute type. Defaults to NULL.
#' @param cohort Character string specifying the name of the cohort column. Defaults to NULL.
#' @param cinterval Character string specifying the temporal resolution. Must be "day" (default),
#'        "week", or "month". If "month", climate data are aggregated to monthly means and
#'        date indices are in months. If "week", data are aggregated to 7-day blocks and indices
#'        are in weeks.
#' @param aggfunc A function used to aggregate climate values within each period when
#'        \code{cinterval} is \code{"month"} or \code{"week"}. Defaults to \code{mean}.
#'        Any function that accepts a numeric vector and returns a single value is valid
#'        (e.g. \code{sum}, \code{max}, \code{min}, \code{median}).
#'
#' @return A list containing:
#'         - bio_data: The processed biological data with date_int column
#'         - bio_int_ranges: List of integer date ranges for each spatial group
#'         - bio_data_row: Vector of row indices for rejoining data
#'         - bio_xvar_ranges: Matrix of climate variable values for each range
#'         - max_possible_range: Maximum possible range based on climate data
#'
#' @keywords internal
process_data <- function(climate_data,
                        bio_data,
                        range,
                        cdate = "Date",
                        bdate = "Date",
                        xvar = "Temp",
                        spatial = NULL,
                        type = "relative",
                        refday = NULL,
                        cohort = NULL,
                        cinterval = "day",
                        aggfunc = mean) {
  
  ### ARGUMENT CHECKS ####
  # Validate required data frames
  validate_args(
    args = list(
      climate_data = climate_data,
      bio_data = bio_data
    ),
    required = TRUE,
    type = "data.frame",
    additional_checks = function(x) if(nrow(x) == 0) stop("must contain at least 1 row")
  )
  
  # Validate type parameter
  validate_arg("type", type, required = FALSE, type = "character",
              additional_checks = function(x) if(!x %in% c("relative", "absolute"))
                stop("must be either 'relative' or 'absolute'"))

  # Validate cinterval parameter
  validate_arg("cinterval", cinterval, required = FALSE, type = "character",
              additional_checks = function(x) if(!x %in% c("day", "week", "month"))
                stop("must be 'day', 'week', or 'month'"))

  # Validate aggfunc parameter
  validate_arg("aggfunc", aggfunc, required = FALSE, type = "function")
  
  # Validate refday parameter if type is absolute
  if (type == "absolute") {
    validate_arg("refday", refday, required = TRUE, type = "character",
                additional_checks = function(x) {
                  refday_date <- as.Date(x, format = "%d/%m/%Y")
                  if(is.na(refday_date)) stop("must be in format 'DD/MM/YYYY'")
                })
  }
  
  # Validate column names in climate_data
  validate_arg("climate_data", climate_data, required = FALSE,
              additional_checks = function(x) {
                if(!all(c(cdate, xvar) %in% names(x))) 
                  stop(sprintf("must contain columns '%s' and '%s'", cdate, xvar))
              })
  
  # Validate column names in bio_data
  validate_arg("bio_data", bio_data, required = FALSE,
              additional_checks = function(x) {
                if(!bdate %in% names(x)) 
                  stop(sprintf("must contain column '%s'", bdate))
              })
  
  # If spatial is not given, we create a dummy col
  if (is.null(spatial)){
    spatial <- "spatial"
    climate_data$spatial <- "A"
    bio_data$spatial <- "A"
  } else {
    # Validate spatial column exists in both datasets
    validate_args(
      args = list(
        climate_data = climate_data,
        bio_data = bio_data
      ),
      required = FALSE,
      additional_checks = function(x) {
        if(!spatial %in% names(x)) 
          stop(sprintf("must contain column '%s'", spatial))
      }
    )
  }
  
  # Validate cohort column if provided
  if (!is.null(cohort)) {
    validate_arg("bio_data", bio_data, required = FALSE,
                additional_checks = function(x) {
                  if(!cohort %in% names(x)) 
                    stop(sprintf("must contain column '%s'", cohort))
                })
  }
  
  ### CLIMATE DATA COMPLETENESS CHECKS ####
  # Check 1: Validate that cdate column is a date column
  climate_dates <- climate_data[[cdate]]
  if (!is.character(climate_dates)) {
    stop(sprintf("Column '%s' in climate_data must be a character in format 'DD/MM/YYYY'", cdate))
  }

  # Convert to Date object for validation and processing
  converted_dates <- as.Date(climate_dates, format = "%d/%m/%Y")
  if (all(is.na(converted_dates))) {
    stop(sprintf("Column '%s' in climate_data must be in format 'DD/MM/YYYY'", cdate))
  }
  climate_data[[cdate]] <- converted_dates

  # Aggregate climate data based on cinterval
  if (cinterval == "month") {
    climate_data[[cdate]] <- as.Date(format(climate_data[[cdate]], "%Y-%m-01"))
    climate_data <- dplyr::group_by(climate_data, .data[[spatial]], .data[[cdate]]) |>
      dplyr::summarise(!!rlang::sym(xvar) := aggfunc(.data[[xvar]]),
                       .groups = "drop") |>
      dplyr::arrange(.data[[spatial]], .data[[cdate]]) |>
      as.data.frame()
  } else if (cinterval == "week") {
    min_climate_date <- min(climate_data[[cdate]])
    days_since_start <- as.integer(climate_data[[cdate]] - min_climate_date)
    climate_data[[cdate]] <- min_climate_date + floor(days_since_start / 7) * 7
    climate_data <- dplyr::group_by(climate_data, .data[[spatial]], .data[[cdate]]) |>
      dplyr::summarise(!!rlang::sym(xvar) := aggfunc(.data[[xvar]]),
                       .groups = "drop") |>
      dplyr::arrange(.data[[spatial]], .data[[cdate]]) |>
      as.data.frame()
  }

  # Check 2: Verify continuous series with no missing periods
  climate_dates_sorted <- sort(unique(climate_data[[cdate]]))
  if (cinterval == "day") {
    expected_dates <- seq.Date(from = min(climate_dates_sorted),
                               to = max(climate_dates_sorted),
                               by = "day")
    missing_dates <- setdiff(as.character(expected_dates), as.character(climate_dates_sorted))
    if (length(missing_dates) > 0) {
      stop(sprintf("Climate data has missing dates: %s. The date series must be continuous from %s to %s.",
                  paste(missing_dates, collapse = ", "),
                  min(climate_dates_sorted),
                  max(climate_dates_sorted)))
    }
  } else if (cinterval == "month") {
    expected_months <- seq.Date(from = min(climate_dates_sorted),
                                to = max(climate_dates_sorted),
                                by = "month")
    missing_months <- setdiff(as.character(expected_months), as.character(climate_dates_sorted))
    if (length(missing_months) > 0) {
      stop(sprintf("Climate data has missing months: %s. The monthly series must be continuous from %s to %s.",
                  paste(missing_months, collapse = ", "),
                  min(climate_dates_sorted),
                  max(climate_dates_sorted)))
    }
  } else if (cinterval == "week") {
    expected_weeks <- seq.Date(from = min(climate_dates_sorted),
                               to = max(climate_dates_sorted),
                               by = "week")
    missing_weeks <- setdiff(as.character(expected_weeks), as.character(climate_dates_sorted))
    if (length(missing_weeks) > 0) {
      stop(sprintf("Climate data has missing weeks: %s. The weekly series must be continuous from %s to %s.",
                  paste(missing_weeks, collapse = ", "),
                  min(climate_dates_sorted),
                  max(climate_dates_sorted)))
    }
  }

  # Check 3: Verify no missing data in xvar column
  xvar_data <- climate_data[[xvar]]
  if (any(is.na(xvar_data) | is.infinite(xvar_data))) {
    missing_count <- sum(is.na(xvar_data) | is.infinite(xvar_data))
    total_count <- length(xvar_data)
    stop(sprintf("Column '%s' in climate_data contains %d missing or infinite values out of %d total values. All climate data must be complete.",
                xvar, missing_count, total_count))
  }

  ### PROCESS DATA ####
  # Add integer dates to data frames (1 = earliest climate data)
  climate_data$date_int <- 1:nrow(climate_data)
  # Convert dates to Date objects
  bio_dates <- as.Date(bio_data[[bdate]], format = "%d/%m/%Y")
  # Get year from cohort
  if (!is.null(cohort)){
    # Get earliest year for each cohort
    cohort_years <- tapply(bio_dates, bio_data[[cohort]], function(x) {
      min(lubridate::year(x))
    })
    years <- cohort_years[as.character(bio_data[[cohort]])]
  } else {
    years <- lubridate::year(bio_dates)
  }

  # Compute effective bio dates (adjusted for relative/absolute type and cohort year)
  if (type == "relative") {
    effective_bio_dates <- as.Date(paste(
      lubridate::day(bio_dates),
      lubridate::month(bio_dates),
      years,
      sep = "/"
    ), format = "%d/%m/%Y")
  } else {
    refday_parts_as_date <- as.Date(refday, format = "%d/%m/%Y")
    effective_bio_dates <- as.Date(paste(
      lubridate::day(refday_parts_as_date),
      lubridate::month(refday_parts_as_date),
      years,
      sep = "/"
    ), format = "%d/%m/%Y")
  }

  # Convert effective bio dates to integer indices matching the climate interval
  first_climate_date <- climate_data[[cdate]][1]
  if (cinterval == "day") {
    bio_data$date_int <- convert_dates_to_int(effective_bio_dates,
                                              min_date = first_climate_date) + 1
  } else if (cinterval == "month") {
    first_year  <- as.integer(format(first_climate_date, "%Y"))
    first_month <- as.integer(format(first_climate_date, "%m"))
    bio_year    <- as.integer(format(effective_bio_dates, "%Y"))
    bio_month   <- as.integer(format(effective_bio_dates, "%m"))
    bio_data$date_int <- (bio_year - first_year) * 12 + (bio_month - first_month) + 1
  } else if (cinterval == "week") {
    bio_data$date_int <- floor(as.integer(effective_bio_dates - first_climate_date) / 7) + 1
  }
  
  ## Each col is all the possible (integer) dates that are relevant across range
  bio_int_split <- split(bio_data$date_int, bio_data[[spatial]])
  ## Need to keep track of how the data are split so we can rejoin
  bio_data_row <- unlist(split(1:nrow(bio_data), bio_data[[spatial]]))
  bio_int_ranges <- lapply(bio_int_split, FUN = \(site) {
    sapply(site, FUN = \(x) {
      x - range
    })
  })
  
  if (any(unlist(bio_int_ranges) < 1)) {
    stop("'range' covers time periods not included in climate data. Consider adding more climate data or reducing range.")
  }
  
  ## Make climate data as a named list so that we can access the different spatial locations
  climate_data_vec <- climate_data[[xvar]]
  climate_data_list <- split(climate_data_vec, climate_data[[spatial]])
  
  ## Each col is all the possible xvar values that are relevant across range
  bio_xvar_ranges_list <- lapply(names(bio_int_ranges), \(site) {
    climate_data_site <- climate_data_list[[site]]
    bio_data_site <- bio_int_ranges[[site]]
    
    apply(bio_data_site, MARGIN = 2, FUN = \(x) {
      climate_data_site[x]
    })
  })
  ## Convert back into a single matrix for later code
  bio_xvar_ranges <- do.call(cbind, bio_xvar_ranges_list)
  
  # Calculate maximum possible range based on climate data
  max_climate_days <- max(climate_data$date_int)
  min_climate_days <- min(climate_data$date_int)
  max_possible_range <- max_climate_days - min_climate_days
  
  # Check if any requested range exceeds the available data
  max_requested_range <- max(range)
  if (max_requested_range > max_possible_range) {
    stop(sprintf(
      "Requested range (%d days) exceeds available climate data range (%d days).\nMaximum possible range is 0 to %d.",
      max_requested_range,
      max_possible_range,
      max_possible_range
    ))
  }
  
  return(list(
    bio_data = bio_data,
    bio_int_ranges = bio_int_ranges,
    bio_data_row = bio_data_row,
    bio_xvar_ranges = bio_xvar_ranges,
    max_possible_range = max_possible_range
  ))
} 