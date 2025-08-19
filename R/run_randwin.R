#' Run Randomized Sliding Window Analysis
#'
#' This function performs randomization tests by running sliding window analysis 
#' on randomized climate data. For each iteration, the climate variable is 
#' randomized and the best climate window (lowest AIC) is extracted. This creates
#' a null distribution for comparison with observed results.
#'
#' @param repeats Integer. Number of randomization iterations to perform.
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
#' @param progress Logical. If TRUE, progress bars are shown. Default is TRUE.
#'
#' @return A data frame containing the best climate window (lowest AIC) from each randomization iteration:
#'         - Iteration: The randomization iteration number
#'         - Start_Day: Start day as integer (number of days before Bio_Date)
#'         - End_Day: End day as integer (number of days before Bio_Date)
#'         - AIC: AIC value for the linear model
#'         - ModWeight: Model weight calculated as (exp(-0.5 * AIC)) / sum(exp(-0.5 * AIC))
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' null_results <- run_randwin(repeats = 100,
#'                            range = 0:50, 
#'                            climate_data = Climate, 
#'                            bio_data = Mass,
#'                            basemodel = lm(Mass ~ climate, data = bio_data))
#'
#' @importFrom progress progress_bar
#' @export
run_randwin <- function(repeats,
                        range,
                        climate_data,
                        bio_data,
                        basemodel,
                        cdate = "Date",
                        bdate = "Date",
                        xvar = "Temp",
                        fn = mean,
                        type = "relative",
                        refday = NULL,
                        parallel = FALSE,
                        progress = TRUE) {
  
  ### ARGUMENT CHECKS ####
  validate_arg("repeats", repeats, required = TRUE, type = "numeric",
               additional_checks = list(
                 function(x) if(length(x) != 1 || is.na(x)) stop("must be a single number"),
                 function(x) if(x < 1) stop("must be a positive integer")
               ))
  
  validate_arg("range", range, required = TRUE)
  
  validate_arg("climate_data", climate_data, required = TRUE, type = "data.frame",
               additional_checks = function(x) if(nrow(x) == 0) stop("must contain at least 1 row"))
  
  validate_arg("bio_data", bio_data, required = TRUE, type = "data.frame",
               additional_checks = function(x) if(nrow(x) == 0) stop("must contain at least 1 row"))
  
  validate_arg("basemodel", basemodel, required = TRUE)
  
  validate_arg("fn", fn, required = FALSE, type = "function")
  
  validate_arg("type", type, required = FALSE, type = "character",
               additional_checks = function(x) if(!x %in% c("relative", "absolute")) 
                 stop("must be either 'relative' or 'absolute'"))
  
  if (type == "absolute") {
    validate_arg("refday", refday, required = TRUE, type = "character",
                 additional_checks = function(x) {
                   refday_date <- as.Date(x, format = "%d/%m/%Y")
                   if(is.na(refday_date)) stop("must be in format 'DD/MM/YYYY'")
                 })
  }
  
  validate_arg("climate_data", climate_data, required = FALSE,
               additional_checks = function(x) {
                 if(!all(c(cdate, xvar) %in% names(x))) 
                   stop(sprintf("must contain columns '%s' and '%s'", cdate, xvar))
               })
  
  # Initialize progress bar for randomizations
  if (progress && interactive()) {
    pb_rand <- progress::progress_bar$new(
      format = "Randomization [:bar] :current/:total (:percent) :elapsed",
      total = repeats,
      clear = FALSE,
      width = 60
    )
  }
  
  basemodel <- substitute(basemodel)
  
  # Run randomization iterations
  rand_results <- purrr::map(1:repeats, .f = function(i){
    
    # Create a copy of climate_data with randomized xvar column
    climate_rand <- climate_data
    climate_rand[[xvar]] <- sample(climate_data[[xvar]])
    
    sw_result <- run_slidingwin(
      range = range,
      climate_data = climate_rand,
      bio_data = bio_data,
      basemodel = basemodel,
      cdate = cdate,
      bdate = bdate,
      xvar = xvar,
      fn = fn,
      type = type,
      refday = refday,
      parallel = parallel,
      progress = FALSE,  # Disable progress for individual runs
      .basemodelIsCall = TRUE
      )
    
    # Extract the top row (lowest AIC) and add iteration number
    # Handle new list structure from run_slidingwin
    if (is.list(sw_result) && "dataset" %in% names(sw_result)) {
      sw_result <- sw_result$dataset
    }
    
    if (nrow(sw_result) > 0) {
      best_window <- sw_result[1, , drop = FALSE]
      best_window$Iteration <- i
    } else {
      stop("Missing data")
    }
    
    # Update progress bar
    if (progress && interactive()) {
      pb_rand$tick()
    }
    
    return(best_window)
    
  })
  
  # Combine all results
  final_results <- dplyr::bind_rows(rand_results)
  
  # Reorder columns to put Iteration first
  col_order <- c("Iteration", setdiff(names(final_results), "Iteration"))
  final_results <- final_results[, col_order, drop = FALSE]
  
  return(final_results)
} 