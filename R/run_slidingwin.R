#' Run Sliding Window Analysis Between Mass and Climate Data
#'
#' This function fits linear models between mass and climate data for each combination of days,
#' returning AIC values and other statistics. The function requires a base model structure
#' that will be updated for each climate window. It internally calls calc_windows
#' to compute climate summaries for each window.
#'
#' @param range A numeric vector specifying the number of days to look back from each date in bio_data.
#'              For example, 0 represents the date itself, while 100 represents 100 days before that date.
#' @param climate_data A data frame containing climate data. Required.
#' @param bio_data A data frame containing biological data with a date column. Required.
#' @param basemodel An lm model object that will be updated for each climate window (e.g., lm(Mass ~ climate, data = bio_data)). Required.
#' @param cdate Character string specifying the name of the date column in climate_data. Defaults to "Date".
#' @param bdate Character string specifying the name of the date column in bio_data. Defaults to "Date".
#' @param xvar Character string specifying the name of the climate variable column in climate_data. Defaults to "Temp".
#' @param fn A function to use for summarizing the climate data. Defaults to mean().
#' @param type Character string specifying the type of date range calculation. Must be either "relative" (default) or "absolute".
#' @param refday Character string in format "DD/MM/YYYY" specifying the reference date to use when type is "absolute".
#' @param parallel Logical. If TRUE, parallel processing is used. Default is FALSE.
#'
#' @return A data frame containing:
#'         - Start_Date: Start date of the climate window
#'         - End_Date: End date of the climate window
#'         - Start_Day: Start day as integer (number of days before Bio_Date)
#'         - End_Day: End day as integer (number of days before Bio_Date)
#'         - AIC: AIC value for the linear model
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv("MassClimate.csv")
#' Mass <- read.csv("Mass.csv")
#' results <- run_slidingwin(range = 0:2, 
#'                         climate_data = Climate, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data))
#'
#' @importFrom furrr future_map
#' @importFrom future plan
#' @importFrom future multisession
#' @export
run_slidingwin <- function(range,
                         climate_data,
                         bio_data,
                         basemodel,
                         cdate = "Date",
                         bdate = "Date",
                         xvar = "Temp",
                         fn = mean,
                         type = "relative",
                         refday = NULL,
                         parallel = FALSE) {
  # Ensure future and furrr are loaded if parallel is TRUE
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE)) stop("Package 'future' is required.")
    if (!requireNamespace("furrr", quietly = TRUE)) stop("Package 'furrr' is required.")
    future::plan(future::multisession)
  }

  ## Substitute basemodel at the start so it doesn't try and run
  ## and fail
  basemodel <- substitute(basemodel)
  
  # Input validation
  if (missing(climate_data) || nrow(climate_data) == 0) {
    stop("climate_data must contain at least 1 row")
  }
  
  if (missing(bio_data) || nrow(bio_data) == 0) {
    stop("bio_data must contain at least 1 row")
  }
  
  if (missing(basemodel)) {
    stop("'basemodel' is required.")
  }
  
  if (!is.data.frame(bio_data)) {
    stop("bio_data must be a data frame")
  }
  
  # Calculate climate means using calc_windows
  climate_means <- calc_windows(
    range = range,
    climate_data = climate_data,
    bio_data = bio_data,
    cdate = cdate,
    bdate = bdate,
    xvar = xvar,
    fn = fn,
    type = type,
    refday = refday
  )
  
  # Define a function to process each window
  process_window <- function(window_data) {
    
    # Update climate variable with Summary_Value
    bio_data$climate <- window_data$Summary_Value
    
    # Try to fit the model and extract statistics
    fit_result <- tryCatch({
      model <- eval(basemodel)
      list(
        AIC = AIC(model)
      )
    }, error = function(e) {
      list(
        AIC = NA_real_
      )
    })
    
    return(data.frame(
      # Start_Date = as.character(window_data$Start_Date[1]),
      # End_Date = as.character(window_data$End_Date[1]),
      Start_Day = as.integer(window_data$Start_Day[1]),
      End_Day = as.integer(window_data$End_Day[1]),
      AIC = fit_result$AIC,
      stringsAsFactors = FALSE
    ))
  }
  
  # Process all windows in parallel or sequentially
  if (parallel) {
    results <- furrr::future_map(climate_means, process_window, .options = furrr::furrr_options(seed = TRUE))
  } else {
    results <- lapply(climate_means, process_window)
  }
  
  # Combine results
  results <- do.call(rbind, results)
  
  # Sort by AIC (NAs last)
  results <- results[order(is.na(results$AIC), results$AIC), ]
  
  return(results)
} 