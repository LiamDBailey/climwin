#' Run Randomized Window Analysis
#'
#' Performs randomization tests by running sliding or weighted window analysis
#' on randomized climate data. For each iteration the climate variable is
#' shuffled and the best-fitting window is extracted, building a null
#' distribution for comparison with observed results.
#'
#' @param repeats Integer. Number of randomization iterations to perform.
#' @param range A two-element numeric vector \code{c(lower, upper)} specifying
#'   the day range to search (e.g. \code{c(0, 50)}).
#' @param climate_data A data frame containing climate data. Required.
#' @param bio_data A data frame containing biological data with a date column.
#'   Required.
#' @param baseline An lm model object used as the model template. Required.
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
#'   \code{"week"}, or \code{"month"}. When \code{"month"} or \code{"week"},
#'   \code{climate_data} must be pre-aggregated with \code{\link{trans_clim_interval}}.
#'
#' @section weightwin arguments:
#' The following arguments are passed to \code{\link{run_weightwin}} when
#' \code{window_type = "weightwin"}.  Plots are always suppressed
#' (\code{plot_every = NULL}).
#'
#' @param weightfunc Weighting function — see \code{\link{run_weightwin}}.
#'   Defaults to \code{"W"}.
#' @param par Initial distribution parameters — see \code{\link{run_weightwin}}.
#' @param method Optimisation method: \code{"L-BFGS-B"} (default) or
#'   \code{"Nelder-Mead"} — see \code{\link{run_weightwin}}.
#' @param lower Lower bounds. Set automatically when \code{NULL}.
#' @param upper Upper bounds. Set automatically when \code{NULL}.
#' @param control \code{optim} control list. Defaults to
#'   \code{list(maxit = 100)}.
#' @param par_min Lower bounds for random starting parameters.
#' @param par_max Upper bounds for random starting parameters.
#' @param AIC_fn A function used to compute AIC from a fitted model.
#' Must return a single numeric value. Defaults to \code{AIC}.
#' Pass a custom function when using model classes that do not support the
#' standard \code{AIC} generic behaviour (e.g. spaMM).
#'
#' @return A data frame with one row per randomization iteration. For
#'   \code{"slidingwin"}, columns include \code{Iteration}, \code{Start_Day},
#'   \code{End_Day}, \code{AIC}, and \code{ModWeight}.  For
#'   \code{"weightwin"}, columns include \code{Iteration}, the optimal
#'   parameter columns from \code{weightwin_summary}, and \code{AIC}.
#'
#' @examples
#' data("MassClimate")
#' data("Mass")
#' null_results <- run_randwin(repeats = 100,
#'                             range = c(0, 50),
#'                             climate_data = MassClimate,
#'                             bio_data = Mass,
#'                             baseline = lm(Mass ~ climate, data = bio_data))
#'
#' @importFrom progress progress_bar
#' @importFrom future.apply future_lapply
#' @importFrom progressr progressor with_progress
#' @export
run_randwin <- function(repeats,
                        range,
                        climate_data,
                        bio_data,
                        baseline,
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
                        par_max = NULL,
                        AIC_fn  = AIC) {

  ### ARGUMENT CHECKS ####
  validate_arg("repeats", repeats, required = TRUE, type = "numeric",
               additional_checks = list(
                 function(x) if (length(x) != 1 || is.na(x)) stop("must be a single number"),
                 function(x) if (x < 1) stop("must be a positive integer")
               ))

  validate_range(range)
  range_seq <- seq.int(range[1], range[2])

  validate_arg("climate_data", climate_data, required = TRUE, type = "data.frame",
               additional_checks = function(x) if (nrow(x) == 0) stop("must contain at least 1 row"))

  validate_arg("bio_data", bio_data, required = TRUE, type = "data.frame",
               additional_checks = function(x) if (nrow(x) == 0) stop("must contain at least 1 row"))

  validate_arg("baseline", baseline, required = TRUE)

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

  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE))
      stop("Package 'future' is required.")
    if (!requireNamespace("future.apply", quietly = TRUE))
      stop("Package 'future.apply' is required.")
    future::plan(future::multisession)
  }

  window_type <- match.arg(window_type, choices = c("slidingwin", "weightwin"))

  baseline <- substitute(baseline)

  # Pre-compute the bio-side of process_data once for the slidingwin path.
  # Only bio_xvar_ranges changes between iterations (shuffled climate values);
  # all date indices and bio data are constant.
  base_processed <- if (window_type == "slidingwin") {
    process_data(
      climate_data = climate_data,
      bio_data     = bio_data,
      range        = range_seq,
      cdate        = cdate,
      bdate        = bdate,
      xvar         = xvar,
      type         = type,
      refday       = refday,
      cinterval    = cinterval
    )
  } else {
    NULL
  }

  # For the slidingwin parallel path, pre-compute objects that are constant
  # across iterations so workers only receive pre-computed base R objects.
  range_combinations <- if (window_type == "slidingwin") {
    rc <- expand.grid(start_days = range_seq, end_days = range_seq)
    rc[rc$end_days >= rc$start_days, ]
  } else {
    NULL
  }

  # Capture package functions so parallel workers can find them when the
  # weightwin path is used (run_weightwin calls package internals).
  .run_weightwin <- run_weightwin

  # Core per-iteration function — returns one best-window data.frame row.
  # The slidingwin path uses only base R to stay portable across worker
  # processes regardless of how the package was loaded.
  run_one <- function(i) {
    clim_shuffled <- sample(climate_data[[xvar]])

    if (window_type == "slidingwin") {
      # Rebuild bio_xvar_ranges from shuffled climate values only.
      # Use 'bio_data' as the local name so eval(baseline) resolves it
      # correctly (the model call contains data = bio_data).
      bio_data       <- base_processed$bio_data
      bio_int_ranges <- base_processed$bio_int_ranges
      bio_data_row   <- base_processed$bio_data_row
      spatial_col    <- base_processed$spatial_col

      # Mirror run_slidingwin fast path: split shuffled values by spatial group
      clim_spatial <- if (spatial_col %in% names(climate_data)) {
        climate_data[[spatial_col]]
      } else {
        rep(names(bio_int_ranges)[1L], length(clim_shuffled))
      }
      clim_rand_list  <- split(clim_shuffled, clim_spatial)
      bio_xvar_ranges <- do.call(cbind, lapply(names(bio_int_ranges), function(site) {
        idx <- bio_int_ranges[[site]]
        matrix(clim_rand_list[[site]][idx], nrow = nrow(idx), ncol = ncol(idx))
      }))

      row_order   <- order(bio_data_row)
      use_cumsum  <- identical(fn, mean) || identical(fn, sum)
      xvar_cumsum <- if (use_cumsum) rbind(0, apply(bio_xvar_ranges, 2L, cumsum))

      n_windows  <- nrow(range_combinations)
      result_mat <- matrix(NA_real_, nrow = 3L, ncol = n_windows)
      for (j in seq_len(n_windows)) {
        s <- range_combinations$start_days[j] + 1L
        e <- range_combinations$end_days[j]   + 1L
        if (use_cumsum) {
          col_sums <- xvar_cumsum[e + 1L, ] - xvar_cumsum[s, ]
          bio_data$climate <- if (identical(fn, mean)) {
            (col_sums / (e - s + 1L))[row_order]
          } else {
            col_sums[row_order]
          }
        } else {
          bio_data$climate <- apply(
            bio_xvar_ranges, 2L, function(x) fn(x[s:e])
          )[row_order]
        }
        result_mat[, j] <- c(
          s - 1L, e - 1L,
          tryCatch(AIC_fn(eval(baseline)), error = function(e) NA_real_)
        )
      }

      valid       <- !is.na(result_mat[3L, ])
      best_j      <- if (any(valid)) which.min(result_mat[3L, ]) else NA_integer_
      mod_weights <- rep(NA_real_, n_windows)
      if (any(valid)) {
        delta   <- result_mat[3L, ] - min(result_mat[3L, valid])
        aw      <- exp(-0.5 * delta[valid])
        mod_weights[valid] <- aw / sum(aw)
      }
      data.frame(
        Iteration = i,
        Start_Day = as.integer(result_mat[1L, best_j]),
        End_Day   = as.integer(result_mat[2L, best_j]),
        AIC       = result_mat[3L, best_j],
        ModWeight = mod_weights[best_j]
      )
    } else {
      climate_rand         <- climate_data
      climate_rand[[xvar]] <- clim_shuffled
      ww_result <- .run_weightwin(
        n                = 1L,
        range            = range,
        bio_data         = bio_data,
        climate_data     = climate_rand,
        baseline         = baseline,
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
        AIC_fn           = AIC_fn,
        .baselineIsCall  = TRUE
      )
      best_row           <- ww_result@weightwin_summary[1L, , drop = FALSE]
      best_row$Iteration <- i
      best_row
    }
  }

  # Run iterations — parallel across repeats, or sequential with a progress bar
  if (parallel) {
    if (progress && interactive()) {
      rand_results <- progressr::with_progress({
        p <- progressr::progressor(steps = repeats)
        future.apply::future_lapply(seq_len(repeats), function(i) {
          result <- run_one(i)
          p()
          result
        }, future.seed = TRUE)
      })
    } else {
      rand_results <- future.apply::future_lapply(
        seq_len(repeats), run_one, future.seed = TRUE
      )
    }
  } else {
    if (progress && interactive()) {
      pb_rand <- progress::progress_bar$new(
        format = "Randomization [:bar] :current/:total (:percent) :elapsed",
        total  = repeats,
        clear  = FALSE,
        width  = 60
      )
    }
    rand_results <- vector("list", repeats)
    for (i in seq_len(repeats)) {
      rand_results[[i]] <- run_one(i)
      if (progress && interactive()) pb_rand$tick()
    }
  }

  # Combine all results
  final_results <- dplyr::bind_rows(rand_results)

  # Reorder columns to put Iteration first
  col_order     <- c("Iteration", setdiff(names(final_results), "Iteration"))
  final_results <- final_results[, col_order, drop = FALSE]

  return(final_results)
}
