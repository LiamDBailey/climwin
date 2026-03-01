#' Run Randomized Window Analysis
#'
#' Performs randomization tests by running sliding or weighted window analysis
#' on randomized climate data. For each iteration the climate variable is
#' shuffled and the best-fitting window is extracted, building a null
#' distribution for comparison with observed results.
#'
#' @param repeats Integer. Number of randomization iterations to perform.
#' @param range A numeric vector specifying the number of days to look back
#'   from each date in bio_data.
#' @param climate_data A data frame containing climate data. Required.
#' @param bio_data A data frame containing biological data with a date column.
#'   Required.
#' @param basemodel An lm model object used as the model template. Required.
#' @param cdate Character string — date column in climate_data.
#'   Defaults to \code{"Date"}.
#' @param bdate Character string — date column in bio_data.
#'   Defaults to \code{"Date"}.
#' @param xvar Character string — climate variable column in climate_data.
#'   Defaults to \code{"Temp"}.
#' @param fn A function for summarising climate data (slidingwin only).
#'   Defaults to \code{mean}.
#' @param type \code{"relative"} (default) or \code{"absolute"}.
#' @param refday Reference date (\code{"DD/MM/YYYY"}) when
#'   \code{type = "absolute"}.
#' @param parallel Logical. Use parallel processing (slidingwin only).
#'   Default \code{FALSE}.
#' @param progress Logical. Show a progress bar. Default \code{TRUE}.
#' @param window_type Character string. Either \code{"slidingwin"} (default)
#'   or \code{"weightwin"}.
#' @param cinterval Character string specifying the temporal resolution: \code{"day"} (default),
#'   \code{"week"}, or \code{"month"}. Passed through to the underlying window function.
#'
#' @section weightwin arguments:
#' The following arguments are passed to \code{\link{run_weightwin}} when
#' \code{window_type = "weightwin"}.  Plots are always suppressed
#' (\code{plot_every = NULL}).
#'
#' @param weightfunc Weighting function — see \code{\link{run_weightwin}}.
#'   Defaults to \code{"W"}.
#' @param par Initial distribution parameters — see \code{\link{run_weightwin}}.
#' @param method Optimisation method. Defaults to \code{"L-BFGS-B"}.
#' @param lower Lower bounds. Set automatically when \code{NULL}.
#' @param upper Upper bounds. Set automatically when \code{NULL}.
#' @param control \code{optim} control list. Defaults to
#'   \code{list(maxit = 100)}.
#' @param par_min Lower bounds for random starting parameters.
#' @param par_max Upper bounds for random starting parameters.
#'
#' @return A data frame with one row per randomization iteration. For
#'   \code{"slidingwin"}, columns include \code{Iteration}, \code{Start_Day},
#'   \code{End_Day}, \code{AIC}, and \code{ModWeight}.  For
#'   \code{"weightwin"}, columns include \code{Iteration}, the optimal
#'   parameter columns from \code{weightwin_summary}, and \code{AIC}.
#'
#' @examples
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))
#' null_results <- run_randwin(repeats = 100,
#'                             range = 0:50,
#'                             climate_data = Climate,
#'                             bio_data = Mass,
#'                             basemodel = lm(Mass ~ climate, data = bio_data))
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
                        progress = TRUE,
                        window_type = "slidingwin",
                        cinterval = "day",
                        # weightwin-specific arguments
                        weightfunc = "W",
                        par = c(3, 0.2),
                        method = "L-BFGS-B",
                        lower = NULL,
                        upper = NULL,
                        control = list(maxit = 100),
                        par_min = NULL,
                        par_max = NULL) {

  ### ARGUMENT CHECKS ####
  validate_arg("repeats", repeats, required = TRUE, type = "numeric",
               additional_checks = list(
                 function(x) if (length(x) != 1 || is.na(x)) stop("must be a single number"),
                 function(x) if (x < 1) stop("must be a positive integer")
               ))

  validate_arg("range", range, required = TRUE)

  validate_arg("climate_data", climate_data, required = TRUE, type = "data.frame",
               additional_checks = function(x) if (nrow(x) == 0) stop("must contain at least 1 row"))

  validate_arg("bio_data", bio_data, required = TRUE, type = "data.frame",
               additional_checks = function(x) if (nrow(x) == 0) stop("must contain at least 1 row"))

  validate_arg("basemodel", basemodel, required = TRUE)

  validate_arg("fn", fn, required = FALSE, type = "function")

  validate_arg("type", type, required = FALSE, type = "character",
               additional_checks = function(x) if (!x %in% c("relative", "absolute"))
                 stop("must be either 'relative' or 'absolute'"))

  if (type == "absolute") {
    validate_arg("refday", refday, required = TRUE, type = "character",
                 additional_checks = function(x) {
                   refday_date <- as.Date(x, format = "%d/%m/%Y")
                   if (is.na(refday_date)) stop("must be in format 'DD/MM/YYYY'")
                 })
  }

  validate_arg("climate_data", climate_data, required = FALSE,
               additional_checks = function(x) {
                 if (!all(c(cdate, xvar) %in% names(x)))
                   stop(sprintf("must contain columns '%s' and '%s'", cdate, xvar))
               })

  window_type <- match.arg(window_type, choices = c("slidingwin", "weightwin"))

  # Initialize progress bar
  if (progress && interactive()) {
    pb_rand <- progress::progress_bar$new(
      format = "Randomization [:bar] :current/:total (:percent) :elapsed",
      total  = repeats,
      clear  = FALSE,
      width  = 60
    )
  }

  basemodel <- substitute(basemodel)

  # Run randomization iterations
  rand_results <- purrr::map(1:repeats, .f = function(i) {

    # Shuffle the climate variable
    climate_rand         <- climate_data
    climate_rand[[xvar]] <- sample(climate_data[[xvar]])

    if (window_type == "slidingwin") {

      sw_result <- run_slidingwin(
        range            = range,
        climate_data     = climate_rand,
        bio_data         = bio_data,
        basemodel        = basemodel,
        cdate            = cdate,
        bdate            = bdate,
        xvar             = xvar,
        fn               = fn,
        type             = type,
        refday           = refday,
        cinterval        = cinterval,
        parallel         = parallel,
        progress         = FALSE,
        .basemodelIsCall = TRUE
      )

      sw_result <- getDataset(sw_result)

      if (nrow(sw_result) > 0) {
        best_window           <- sw_result[1, , drop = FALSE]
        best_window$Iteration <- i
      } else {
        stop("Missing data")
      }

      result_row <- best_window

    } else {

      ww_result <- run_weightwin(
        n                = 1,
        range            = range,
        bio_data         = bio_data,
        climate_data     = climate_rand,
        basemodel        = basemodel,
        cdate            = cdate,
        bdate            = bdate,
        xvar             = xvar,
        par              = par,
        type             = type,
        refday           = refday,
        weightfunc       = weightfunc,
        method           = method,
        lower            = lower,
        upper            = upper,
        control          = control,
        plot_every       = NULL,
        par_min          = par_min,
        par_max          = par_max,
        cinterval        = cinterval,
        .basemodelIsCall = TRUE
      )

      best_row            <- ww_result@weightwin_summary[1, , drop = FALSE]
      best_row$Iteration  <- i

      result_row <- best_row

    }

    if (progress && interactive()) pb_rand$tick()

    return(result_row)

  })

  # Combine all results
  final_results <- dplyr::bind_rows(rand_results)

  # Reorder columns to put Iteration first
  col_order     <- c("Iteration", setdiff(names(final_results), "Iteration"))
  final_results <- final_results[, col_order, drop = FALSE]

  return(final_results)
}
