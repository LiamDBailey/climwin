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
#' @param spatial Character string specifying the name of the spatial grouping column in both climate_data and bio_data. Defaults to NULL.
#' @param cohort Character string specifying the name of the cohort column in bio_data. When type is "relative", each row will use the earliest year of all records in the same cohort. Defaults to NULL.
#' @param parallel Logical. If TRUE, parallel processing is used. Default is FALSE.
#' @param progress Logical. If TRUE, shows a progress bar. Default is TRUE.
#' @param .basemodelIsCall Logical. Internal parameter used to handle basemodel substitution. Default is FALSE.
#'
#' @return A list containing:
#'         - dataset: A data frame containing:
#'           - Start_Day: Start day as integer (number of days before Bio_Date)
#'           - End_Day: End day as integer (number of days before Bio_Date)
#'           - AIC: AIC value for the linear model
#'           - ModWeight: Model weight calculated as (exp(-0.5 * AIC)) / sum(exp(-0.5 * AIC))
#'         - bestModel: The fitted model object with the lowest AIC value
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' results <- run_slidingwin(range = 0:2, 
#'                         climate_data = Climate, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data))
#'                         
#' # Access the dataset and best model
#' dataset_results <- results$dataset
#' best_model <- results$bestModel
#'                         
#' Mass$site <- sample(c("A", "B"), size = nrow(Mass), replace = TRUE)
#' Climate1 <- Climate
#' Climate1$site <- "A"
#' Climate2 <- Climate
#' Climate2$site <- "B"
#' Climate_site <- rbind(Climate1, Climate2)
#' results_spatial <- run_slidingwin(range = 0:2, 
#'                         climate_data = Climate_site, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data),
#'                         spatial = "site")
#'                         
#'\dontrun{
#'
#'# Full working examples
#'
#'##EXAMPLE 1## 
#'  
#'# Test both a linear and quadratic variable climate window using datasets "Offspring"
#'# and "OffspringClimate".
#'
#'# Load data.
#'
#' OffspringClimate <- read.csv(system.file("OffspringClimate.csv", package = "climwin"))
#' Offspring <- read.csv(system.file("Offspring.csv", package = "climwin"))
#'
#'# Test both linear and quadratic functions with climate variable temperature
#'
#'OffspringWin <- run_slidingwin(
#'                           range = 0:150,
#'                           climate_data = OffspringClimate,
#'                           bio_data = Offspring,
#'                           basemodel = glm(Offspring ~ climate, data = bio_data, family = "poisson"),
#'                           xvar = "Temperature", 
#'                           cdate = "Date", 
#'                           bdate = "Date", 
#'                           type = "relative",
#'                           parallel = TRUE, progress = TRUE
#'                           )
#'  
#'##EXAMPLE 2##
#'  
#'# Test for an absolute climate window with both 'mean' and 'max' aggregate statistics
#'# using datasets 'Mass' and 'MassClimate'.
#'  
#'# Load data.
#'  
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#'  
#'# Test an absolute window, starting 20 May (refday = c(20, 5))
#'# Test for climate windows between 100 and 0 days ago (range = c(100, 0))
#'# Test both mean and max aggregate statistics (stat = c("mean", "max"))
#'# Fit a linear term (func = "lin")
#'# Test at the resolution of days (cinterval = "day")
#'  
#'MassWin <- run_slidingwin(
#'                      range = 0:100,
#'                      climate_data = Climate, bio_data = Mass,
#'                      basemodel = lm(Mass ~ climate, data = bio_data),
#'                      xvar = "Temp",
#'                      cdate = "Date", bdate = "Date", 
#'                      type = "absolute", refday = "20/05/2025"
#'                      )
#'  
#'}
#'
#' @importFrom furrr future_map
#' @importFrom future plan
#' @importFrom future multisession
#' @importFrom progress progress_bar
#' @importFrom progressr progressor
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
                           spatial = NULL,
                           cohort = NULL,
                           parallel = FALSE,
                           progress = TRUE,
                           .basemodelIsCall = FALSE) {
  
  ### ARGUMENT CHECKS ####
  # Ensure future and furrr are loaded if parallel is TRUE
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE)) stop("Package 'future' is required.")
    if (!requireNamespace("furrr", quietly = TRUE)) stop("Package 'furrr' is required.")
    future::plan(future::multisession)
  }
  
  ## It's possible that basemodel is already a substitute
  ## with model lm()
  if (!.basemodelIsCall){
    basemodel <- substitute(basemodel) 
  }
  
  validate_arg("basemodel", basemodel, required = TRUE)
  validate_arg("fn", fn, required = FALSE, type = "function")
  
  ### PROCESS DATA ####
  processed_data <- process_data(
    climate_data = climate_data,
    bio_data = bio_data,
    range = range,
    cdate = cdate,
    bdate = bdate,
    xvar = xvar,
    spatial = spatial,
    type = type,
    refday = refday,
    cohort = cohort
  )
  
  # Extract processed data
  bio_data <- processed_data$bio_data
  bio_int_ranges <- processed_data$bio_int_ranges
  bio_data_row <- processed_data$bio_data_row
  bio_xvar_ranges <- processed_data$bio_xvar_ranges
  
  # Generate all valid range combinations
  range_combinations <- expand.grid(start_days = range, end_days = range)
  range_combinations <- range_combinations[range_combinations$end_days >= range_combinations$start_days, ]
  
  process_window <- function(i){
    start_days <- range_combinations$start_days[i] + 1
    end_days <- range_combinations$end_days[i] + 1
    
    # Initialize vectors for this combination
    summary_data_unordered <- apply(bio_xvar_ranges, MARGIN = 2, FUN = \(x){
      fn(x[start_days:end_days])
    })
    ## Need to reorder the data incase they were split during spatial joins
    bio_data$climate <- summary_data_unordered[order(bio_data_row)]
    
    # Create results dataframe for this combination
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
    
    data.frame(
      Start_Day = start_days - 1,
      End_Day = end_days - 1,
      AIC = fit_result$AIC,
      stringsAsFactors = FALSE
    )
  }
  
  # Process each range combination
  total_combinations <- nrow(range_combinations)
  
  if (parallel){
    p <- progressr::progressor(steps = total_combinations)
    results <- furrr::future_map(seq_len(nrow(range_combinations)),
                                 function(i) {
                                   result <- process_window(i)
                                   p()
                                   return(result)
                                 }, .options = furrr::furrr_options(seed = TRUE))
  } else {
    pb <- progress::progress_bar$new(
      format = "Processing windows [:bar] :percent :elapsed",
      total = total_combinations,
      clear = FALSE,
      width = 60
    )
    results <- purrr::map(seq_len(nrow(range_combinations)), function(i) {
      result <- process_window(i)
      if (interactive() & progress){
        pb$tick() 
      }
      return(result)
    })
  }
  
  # Combine results
  results <- dplyr::bind_rows(results)
  
  # Calculate model weights (ModWeight)
  # Formula: (exp(-0.5 * AIC)) / sum(exp(-0.5 * AIC))
  # Handle NA values by excluding them from the calculation
  valid_aic <- !is.na(results$AIC)
  if (any(valid_aic)) {
    deltaAIC <- results$AIC - min(results$AIC)
    aic_weights <- exp(-0.5 * deltaAIC[valid_aic])
    total_weight <- sum(aic_weights)
    results$ModWeight <- NA_real_
    results$ModWeight[valid_aic] <- aic_weights / total_weight
  } else {
    results$ModWeight <- NA_real_
  }
  
  # Sort by AIC (NAs last)
  results <- results[order(is.na(results$AIC), results$AIC), ]
  
  # Fit the best model (lowest AIC)
  best_model <- NULL
  if (nrow(results) > 0 && !is.na(results$AIC[1])) {
    best_start <- results$Start_Day[1] + 1
    best_end <- results$End_Day[1] + 1
    
    # Get the climate data for the best window
    best_climate_summary <- apply(bio_xvar_ranges, MARGIN = 2, FUN = \(x){
      fn(x[best_start:best_end])
    })
    
    # Reorder and assign climate data
    bio_data$climate <- best_climate_summary[order(bio_data_row)]
    
    # Fit the best model
    best_model <- tryCatch({
      eval(basemodel)
    }, error = function(e) {
      NULL
    })
  }
  
  # Return list with dataset and best model
  return(list(
    dataset = results,
    bestModel = best_model
  ))
}
