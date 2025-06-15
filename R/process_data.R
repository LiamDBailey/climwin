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
                        cohort = NULL) {
  
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
  
  ### PROCESS DATA ####
  # Add integer dates to data frames (1 = earliest climate data)
  climate_data$date_int <- 1:nrow(climate_data)
  
  if (type == "relative") {
    if (!is.null(cohort)) {
      # Convert dates to Date objects
      bio_dates <- as.Date(bio_data[[bdate]], format = "%d/%m/%Y")
      
      # Get earliest year for each cohort
      cohort_years <- tapply(bio_dates, bio_data[[cohort]], function(x) {
        min(lubridate::year(x))
      })
      
      # Create new dates using earliest year for each cohort
      bio_data$date_int <- convert_dates_to_int(
        as.Date(paste(
          lubridate::day(bio_dates),
          lubridate::month(bio_dates),
          cohort_years[bio_data[[cohort]]],
          sep = "/"
        ), format = "%d/%m/%Y"),
        min_date = climate_data[[cdate]][1]
      ) + 1
    } else {
      bio_data$date_int <- convert_dates_to_int(bio_data[[bdate]], min_date = climate_data[[cdate]][1]) + 1
    }
  } else {
    # Format bio dates and refday as date objects
    bio_dates_as_date <- as.Date(bio_data[[bdate]], format = "%d/%m/%Y")
    refday_parts_as_date <- as.Date(refday, format = "%d/%m/%Y")
    
    ## Create new bio data dates using refday
    bio_data$date_int <- convert_dates_to_int(
      as.Date(paste(
        lubridate::day(refday_parts_as_date),
        lubridate::month(refday_parts_as_date),
        lubridate::year(bio_dates_as_date),
        sep = "/"
      ), format = "%d/%m/%Y"),
      min_date = climate_data[[cdate]][1]
    ) + 1
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