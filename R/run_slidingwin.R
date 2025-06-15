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
  
  validate_arg("basemodel", basemodel, required = TRUE)
  
  # Validate optional arguments
  validate_arg("fn", fn, required = FALSE, type = "function")
  
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
  
  ### FORMAT CLIMATE DATA ####
  
  # Add integer dates to data frames (1 = earliest climate data)
  ## FIXME: We assume climate data is ordered and first date = min date
  ## Need to check it is actually ordered!!
  climate_data$date_int <- 1:nrow(climate_data)
  if (type == "relative"){
    bio_data$date_int <- convert_dates_to_int(bio_data[[bdate]], min_date = climate_data[[cdate]][1]) + 1 
  } else {
    # Format bio dates and refday as date objects
    bio_dates_as_date <- as.Date(bio_data[[bdate]], format = "%d/%m/%Y")
    refday_parts_as_date <- as.Date(refday, format = "%d/%m/%Y")
    
    ## Create new bio data dates using refday
    bio_data$date_int <- convert_dates_to_int(as.Date(paste(lubridate::day(refday_parts_as_date),
                                                            lubridate::month(refday_parts_as_date),
                                                            lubridate::year(bio_dates_as_date),
                                                            sep = "/"), format = "%d/%m/%Y"),
                                              min_date = climate_data[[cdate]][1]) + 1
  }
  
  ## Each col is all the possible (integer) dates that are relevant across range
  bio_int_split <- split(bio_data$date_int, bio_data[[spatial]])
  ## Need to keep track of how the data are split so we can rejoin
  bio_data_row  <- unlist(split(1:nrow(bio_data), bio_data[[spatial]]))
  bio_int_ranges <- lapply(bio_int_split, FUN = \(site){
    sapply(site, FUN = \(x){
      x - range
    })
  })
  
  if (any(unlist(bio_int_ranges) < 1)){
    stop("'range' covers time periods not included in climate data. Consider adding more climate data or reducing range.")
  }
  
  ## Make climate data as a named list so that we can access the different spatial locations
  climate_data_vec <- climate_data[[xvar]]
  climate_data_list <- split(climate_data_vec, climate_data[[spatial]])
  
  ## Each col is all the possible xvar values that are relevant across range
  bio_xvar_ranges_list <- lapply(names(bio_int_ranges), \(site){
    
    climate_data_site <- climate_data_list[[site]]
    bio_data_site <- bio_int_ranges[[site]]
    
    apply(bio_data_site, MARGIN = 2, FUN = \(x){
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
  
  # Sort by AIC (NAs last)
  results <- results[order(is.na(results$AIC), results$AIC), ]
  
  return(results)
} 
