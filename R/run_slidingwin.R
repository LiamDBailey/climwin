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
#' @param cinterval Character string specifying the temporal resolution: \code{"day"} (default),
#'   \code{"week"}, or \code{"month"}. When \code{"month"} or \code{"week"},
#'   \code{climate_data} must be pre-aggregated to one row per period — use
#'   \code{\link{trans_clim_interval}} first.
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
#' dataset_results <- getDataset(results)
#' best_model <- getBestModel(results)
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
#' @importFrom future.apply future_vapply
#' @importFrom future plan
#' @importFrom future multisession
#' @importFrom progress progress_bar
#' @importFrom progressr progressor with_progress
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
                           cinterval = "day",
                           parallel = FALSE,
                           progress = TRUE,
                           .basemodelIsCall = FALSE,
                           .processed_data = NULL) {
  
  ### ARGUMENT CHECKS ####
  # Ensure future and furrr are loaded if parallel is TRUE
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE)) stop("Package 'future' is required.")
    if (!requireNamespace("future.apply", quietly = TRUE)) stop("Package 'future.apply' is required.")
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
  if (!is.null(.processed_data)) {
    # Reuse bio-side data from a pre-computed process_data call (e.g. run_randwin).
    # Only bio_xvar_ranges needs rebuilding from the new (shuffled) climate values.
    bio_data       <- .processed_data$bio_data
    bio_int_ranges <- .processed_data$bio_int_ranges
    bio_data_row   <- .processed_data$bio_data_row

    spatial_col_internal <- .processed_data$spatial_col
    if (!spatial_col_internal %in% names(climate_data)) {
      climate_data[[spatial_col_internal]] <- names(bio_int_ranges)[1L]
    }

    climate_data_list <- split(climate_data[[xvar]], climate_data[[spatial_col_internal]])
    bio_xvar_ranges <- do.call(cbind, lapply(names(bio_int_ranges), function(site) {
      idx <- bio_int_ranges[[site]]
      matrix(climate_data_list[[site]][idx], nrow = nrow(idx), ncol = ncol(idx))
    }))
  } else {
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
      cohort = cohort,
      cinterval = cinterval
    )
    bio_data        <- processed_data$bio_data
    bio_int_ranges  <- processed_data$bio_int_ranges
    bio_data_row    <- processed_data$bio_data_row
    bio_xvar_ranges <- processed_data$bio_xvar_ranges
  }

  # Pre-compute row ordering once (spatial joins may reorder columns)
  row_order <- order(bio_data_row)

  # Pre-compute column-wise cumulative sums when fn is mean or sum —
  # avoids matrix slicing inside the window loop
  use_cumsum <- identical(fn, mean) || identical(fn, sum)
  if (use_cumsum) {
    xvar_cumsum <- rbind(0, apply(bio_xvar_ranges, 2L, cumsum))
  }

  # Generate all valid range combinations
  range_combinations <- expand.grid(start_days = range, end_days = range)
  range_combinations <- range_combinations[range_combinations$end_days >= range_combinations$start_days, ]

  # Returns c(Start_Day, End_Day, AIC) for window combination i
  process_window <- function(i) {
    start_days <- range_combinations$start_days[i] + 1L
    end_days   <- range_combinations$end_days[i]   + 1L

    # Aggregate climate across the window for each bio record
    if (use_cumsum) {
      col_sums <- xvar_cumsum[end_days + 1L, ] - xvar_cumsum[start_days, ]
      summary_data_unordered <- if (identical(fn, mean)) {
        col_sums / (end_days - start_days + 1L)
      } else {
        col_sums
      }
    } else {
      summary_data_unordered <- apply(bio_xvar_ranges, MARGIN = 2L,
                                      FUN = \(x) fn(x[start_days:end_days]))
    }

    bio_data$climate <- summary_data_unordered[row_order]

    aic_val <- tryCatch(AIC(eval(basemodel)), error = function(e) NA_real_)

    c(start_days - 1L, end_days - 1L, aic_val)
  }

  total_combinations <- nrow(range_combinations)

  if (parallel) {
    if (progress && interactive()) {
      message("Initiating parallel processing...")
      result_mat <- progressr::with_progress({
        p <- progressr::progressor(steps = total_combinations)
        future.apply::future_vapply(
          seq_len(total_combinations),
          function(i) { result <- process_window(i); p(); result },
          numeric(3L),
          future.seed = TRUE
        )
      })
    } else {
      result_mat <- future.apply::future_vapply(
        seq_len(total_combinations),
        process_window,
        numeric(3L),
        future.seed = TRUE
      )
    }
  } else {
    if (progress && interactive()) {
      pb <- progress::progress_bar$new(
        format = "Processing windows [:bar] :percent :elapsed",
        total  = total_combinations,
        clear  = FALSE,
        width  = 60
      )
    }
    result_mat <- matrix(NA_real_, nrow = 3L, ncol = total_combinations)
    for (i in seq_len(total_combinations)) {
      result_mat[, i] <- process_window(i)
      if (progress && interactive()) pb$tick()
    }
  }

  results <- data.frame(
    Start_Day = as.integer(result_mat[1L, ]),
    End_Day   = as.integer(result_mat[2L, ]),
    AIC       = result_mat[3L, ]
  )

  # Calculate model weights
  # Formula: (exp(-0.5 * deltaAIC)) / sum(exp(-0.5 * deltaAIC))
  valid_aic <- !is.na(results$AIC)
  if (any(valid_aic)) {
    deltaAIC    <- results$AIC - min(results$AIC[valid_aic])
    aic_weights <- exp(-0.5 * deltaAIC[valid_aic])
    results$ModWeight <- NA_real_
    results$ModWeight[valid_aic] <- aic_weights / sum(aic_weights)
  } else {
    results$ModWeight <- NA_real_
  }

  # Sort by AIC (NAs last)
  results <- results[order(is.na(results$AIC), results$AIC), ]

  # Fit the best model (lowest AIC)
  best_model <- NULL
  if (nrow(results) > 0 && !is.na(results$AIC[1])) {
    best_start <- results$Start_Day[1] + 1L
    best_end   <- results$End_Day[1]   + 1L

    if (use_cumsum) {
      col_sums <- xvar_cumsum[best_end + 1L, ] - xvar_cumsum[best_start, ]
      best_climate_summary <- if (identical(fn, mean)) {
        col_sums / (best_end - best_start + 1L)
      } else {
        col_sums
      }
    } else {
      best_climate_summary <- apply(bio_xvar_ranges, MARGIN = 2L,
                                    FUN = \(x) fn(x[best_start:best_end]))
    }

    bio_data$climate <- best_climate_summary[row_order]

    best_model <- tryCatch(eval(basemodel), error = function(e) NULL)
  }

  # Return climwin S7 object
  output <- climwin(
    dataset   = results,
    bestModel = list(model = best_model, data = bio_data),
    range     = range(range)
  )
  return(output)
}
