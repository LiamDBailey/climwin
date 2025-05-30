#' Fit Linear Models Between Mass and Climate Data
#'
#' This function takes a list of data frames (output from splitting calculate_temp_means)
#' and fits linear models between mass and climate data for each combination of days,
#' returning AIC values and other statistics. The function requires a base model structure
#' that will be updated for each climate window.
#'
#' @param climate_means A LIST of data frames, each containing Bio_Date, Start_Date, End_Date, and Summary_Value columns
#' @param basemodel An lm model object that will be updated for each climate window (e.g., lm(Mass ~ climate, data = bio_data)). This argument is required and cannot be NULL.
#' @param bio_data A data frame containing all variables used in the basemodel. This argument is required and cannot be NULL.
#' @param parallel Logical. If TRUE, parallel processing is used. Default is TRUE.
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
#' climate_means <- calculate_temp_means(0:2, bio_data = Mass)
#' Mass$climate <- 0
#' basemodel <- lm(Mass ~ climate, data = Mass)
#' results <- fit_climate_models(climate_means, basemodel = basemodel, bio_data = Mass)
#'
#' @importFrom furrr future_map
#' @importFrom future plan
#' @importFrom future multisession
#' @export
fit_climate_models <- function(climate_means, basemodel, bio_data, parallel = FALSE) {
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
  if (!is.list(climate_means)) {
    stop("climate_means must be a list")
  }
  
  if (missing(basemodel)) {
    stop("'basemodel' is required.")
  }
  
  if (missing(bio_data) || is.null(bio_data)) {
    stop("'bio_data' is required and cannot be NULL.")
  }
  if (!is.data.frame(bio_data)) {
    stop("bio_data must be a data frame")
  }
  
  # Define a function to process each window
  process_window <- function(window_data) {
    # Check required columns in each list element
    required_cols <- c("Bio_Date", "Start_Date", "End_Date", "Start_Day", "End_Day", "Summary_Value")
    if (!all(required_cols %in% names(window_data))) {
      return(data.frame(
        Start_Date = NA_character_,
        End_Date = NA_character_,
        Start_Day = NA_integer_,
        End_Day = NA_integer_,
        AIC = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    
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
      Start_Date = as.character(window_data$Start_Date[1]),
      End_Date = as.character(window_data$End_Date[1]),
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