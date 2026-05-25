#' Optimise Parameters for Weighted Climate Windows
#'
#' Uses \code{optim} to find the optimal distribution parameters that minimise
#' AIC when creating weighted means of climate data.
#'
#' Built-in weighting functions are selected by passing a character string to
#' \code{weightfunc}: \code{"W"} (Weibull), \code{"G"} (Gumbel), or \code{"F"}
#' (Frechet with location fixed at 0).  Alternatively, supply any density
#' function directly — it must accept \code{(x, par1, par2, ...)} where
#' \code{x} is a numeric vector on [0, 1].
#'
#' @param n Integer. Number of independent optimisation runs. The first run
#'   uses \code{par}; subsequent runs draw starting parameters uniformly from
#'   [\code{par_min}, \code{par_max}].
#' @param range A two-element numeric vector \code{c(lower, upper)} specifying
#'   the day range to search (e.g. \code{c(0, 100)}).
#' @param bio_data A data frame containing biological data with a date column.
#' @param climate_data A data frame containing climate data.
#' @param baseline An \code{lm} call used as the model template
#'   (e.g. \code{lm(Mass ~ climate, data = bio_data)}).
#' @param cdate Character string — date column in \code{climate_data}.
#'   Defaults to \code{"Date"}.
#' @param bdate Character string — date column in \code{bio_data}.
#'   Defaults to \code{"Date"}.
#' @param xvar Character string — climate variable column in
#'   \code{climate_data}.
#' @param par Numeric vector of initial distribution parameters. Length depends
#'   on \code{weightfunc}: 2 for \code{"W"} and \code{"G"}, 2 for \code{"F"}
#'   (scale, shape), or however many your custom function expects.
#' @param type \code{"relative"} (default) or \code{"absolute"}.
#' @param refday Reference date (\code{"DD/MM/YYYY"}) used when
#'   \code{type = "absolute"}.
#' @param weightfunc Either a character string (\code{"W"}, \code{"G"},
#'   \code{"F"}) or a density function with signature
#'   \code{function(x, par1, par2, ...)}. When a function is supplied,
#'   \code{lower} and \code{upper} must be provided explicitly and
#'   \code{par_labels} defaults to \code{"par1"}, \code{"par2"}, …
#' @param method Optimisation method: \code{"L-BFGS-B"} (default) or
#'   \code{"Nelder-Mead"}.  \code{"L-BFGS-B"} applies bounded quasi-Newton
#'   optimisation directly.  \code{"Nelder-Mead"} logit-transforms each
#'   parameter to an unconstrained real line (so the bounds are always
#'   respected) and then applies gradient-free simplex optimisation — this can
#'   converge more reliably on noisy or flat AIC landscapes.
#' @param lower Lower bounds for the parameters. Set automatically for
#'   built-in \code{weightfunc} values when \code{NULL} (default).
#' @param upper Upper bounds for the parameters. Set automatically for
#'   built-in \code{weightfunc} values when \code{NULL} (default).
#'   For \code{"W"} the default scale upper bound is 10: values above this
#'   give near-zero Weibull density across the \code{[0, 1]} domain and only
#'   slow the optimiser down.
#' @param control A named list of control parameters passed to \code{optim}.
#'   Defaults to \code{list(maxit = 100)}.  Additional settings are filled in
#'   automatically if not supplied by the user:
#'   \describe{
#'     \item{L-BFGS-B only — \code{ndeps}}{Finite-difference step for gradient
#'       estimation, set to 0.1 \% of each parameter's range so gradient
#'       estimates are well-conditioned across parameters with different
#'       scales.}
#'     \item{L-BFGS-B only — \code{factr}}{Convergence threshold
#'       (\code{1e9}, ≈ 0.0001 AIC-unit improvement).  The R built-in default
#'       (\code{1e7}) is too strict for AIC landscapes and causes oscillation
#'       near the minimum.}
#'     \item{L-BFGS-B only — \code{pgtol}}{Projected-gradient convergence
#'       tolerance (\code{1e-4}); complements \code{factr}.}
#'     \item{Nelder-Mead only — \code{reltol}}{Relative convergence tolerance
#'       on function values (\code{1e-5}).  The R default
#'       (\code{sqrt(.Machine$double.eps)} ≈ \code{1.5e-8}) is unnecessarily
#'       tight for AIC.}
#'   }
#' @param plot_every Integer or \code{NULL}. Plot diagnostics every
#'   \code{plot_every} objective evaluations, and always after convergence.
#'   Pass \code{NULL} to suppress all plots.
#' @param par_min Numeric vector — lower bounds for random starting parameters
#'   when \code{n > 1}. Defaults to \code{lower}.
#' @param par_max Numeric vector — upper bounds for random starting parameters
#'   when \code{n > 1}. Defaults to \code{upper}.
#' @param cinterval Character string specifying the temporal resolution: \code{"day"} (default),
#'   \code{"week"}, or \code{"month"}. When \code{"month"} or \code{"week"},
#'   \code{climate_data} must be pre-aggregated with \code{\link{trans_clim_interval}}.
#' @param show_best Logical. When \code{TRUE} (default) an additional panel is
#'   included in each diagnostic plot showing observed data (points) and the
#'   predicted climate effect (line) from the current best model.  Set to
#'   \code{FALSE} if rendering the scatter plot slows optimisation too much.
#' @param AIC_fn Function used to calculate AIC of windows.
#' Function must return a single numeric value that can be minimsied to find the best window.
#' Default AIC should work for most model structures, but some models (e.g. `spaMM` package) will require
#' custom functions.
#' @param .predict_args A named list of extra arguments forwarded to
#'   \code{\link[stats]{predict}} when drawing the predicted line in the
#'   scatter panel.  Useful for mixed models where you may want to pass
#'   e.g. \code{list(re.form = NA)} to obtain population-level predictions.
#'
#' @return A \code{climwin_weightwin} S7 object.
#'
#' @examples
#' data("MassClimate")
#' data("Mass")
#'
#' results <- run_weightwin(range = c(0, 100),
#'                          bio_data = Mass,
#'                          climate_data = MassClimate,
#'                          cdate = "Date", bdate = "Date",
#'                          xvar = "Temp",
#'                          baseline = lm(Mass ~ climate, data = bio_data),
#'                          par = c(1.25, 0.5))
#'
#' @export
run_weightwin <- function(n = 1,
                          range,
                          bio_data,
                          climate_data,
                          baseline,
                          cdate = "Date",
                          bdate = "Date",
                          xvar,
                          par = c(3, 0.2),
                          type = "relative",
                          refday = NULL,
                          weightfunc = "W",
                          method = "L-BFGS-B",
                          lower = NULL,
                          upper = NULL,
                          control = list(maxit = 100),
                          plot_every = 10,
                          par_min = NULL,
                          par_max = NULL,
                          cinterval = "day",
                          show_best = TRUE,
                          AIC_fn = AIC,
                          .predict_args = list(),
                          .baselineIsCall = FALSE) {

  validate_range(range)
  range_seq <- seq.int(range[1], range[2])

  validate_arg("baseline", baseline, required = TRUE)
  if (!isTRUE(.baselineIsCall)) baseline <- substitute(baseline)

  method <- match.arg(method, choices = c("L-BFGS-B", "Nelder-Mead"))

  # Resolve weightfunc to a density function + metadata
  if (is.function(weightfunc)) {
    dfun       <- weightfunc
    par_labels <- paste0("par", seq_along(par))
    weightfunc_name <- "custom"
    if (is.null(lower) || is.null(upper))
      stop("When weightfunc is a function, 'lower' and 'upper' must be supplied")
  } else {
    weightfunc <- match.arg(weightfunc, choices = c("W", "G", "F"))
    weightfunc_name <- weightfunc

    if (weightfunc == "W") {
      dfun <- dweibull
      if (is.null(lower)) lower <- c(0.1, 0.1)
      if (is.null(upper)) upper <- c(10, 10)   # scale > 10 gives near-zero density on [0,1]
      par_labels <- c("shape", "scale")
    } else if (weightfunc == "G") {
      dfun <- function(x, loc, scale) evd::dgumbel(x, loc = loc, scale = scale)
      if (is.null(lower)) lower <- c(-0.5, 0.01)
      if (is.null(upper)) upper <- c(1.5, 5)
      par_labels <- c("loc", "scale")
    } else if (weightfunc == "F") {
      ## Fix location at 0.
      ## This is default and allows for easier optimisation
      ## Can allow location to also vary using custom fn
      dfun <- function(x, scale, shape) evd::dfrechet(x, loc = 0,
                                                       scale = scale,
                                                       shape = shape)
      if (is.null(lower)) lower <- c(0.01, 0.1)
      if (is.null(upper)) upper <- c(2, 10)
      par_labels <- c("scale", "shape")
    }
  }

  n_par        <- length(par_labels)
  n_diag_plots <- 2L + n_par + if (isTRUE(show_best)) 1L else 0L
  mfrow_dims   <- if (n_diag_plots <= 4L) c(2L, 2L) else c(2L, 3L)

  # Local helper: scatter of observed response vs weighted climate + predicted line
  draw_scatter <- function(model, bio_data_inner) {
    mf        <- model.frame(model)
    y_obs     <- mf[[1L]]
    x_obs     <- bio_data_inner$climate
    clim_grid <- seq(min(x_obs, na.rm = TRUE), max(x_obs, na.rm = TRUE),
                     length.out = 100L)
    nd        <- as.data.frame(lapply(mf[-1L], function(col) {
      if (is.numeric(col)) rep(mean(col, na.rm = TRUE), 100L)
      else                 rep(levels(factor(col))[[1L]],  100L)
    }))
    nd[["climate"]] <- clim_grid
    pred_line <- tryCatch(
      do.call(predict, c(list(model, newdata = nd), .predict_args)),
      error = function(e) NULL
    )
    plot(x_obs, y_obs, pch = 16L, col = "grey50",
         xlab = "climate", ylab = names(mf)[1L])
    if (!is.null(pred_line)) lines(clim_grid, pred_line)
  }

  # Ensure lower < upper for each parameter
  if (any(lower >= upper)) stop("lower bounds must be less than upper bounds")
  if (any(par < lower) || any(par > upper))
    stop("initial parameters must be within bounds")

  # Default random-start bounds to optimisation bounds when not specified
  if (is.null(par_min)) par_min <- lower
  if (is.null(par_max)) par_max <- upper

  if (any(par_min >= par_max))
    stop("par_min must be less than par_max for each parameter")

  # Logit-space helpers for Nelder-Mead.
  #
  # Nelder-Mead cannot enforce box constraints, so each bounded parameter is
  # mapped to an unconstrained real via the logit of its rescaled value.
  # Clipping to (lower + ε, upper − ε) prevents ±Inf at the boundaries.
  #
  #   to_logit  : p ∈ (lower, upper) → q ∈ (−∞, +∞)
  #   from_logit: q ∈ (−∞, +∞)      → p ∈ (lower, upper)
  eps        <- (upper - lower) * 1e-8
  to_logit   <- function(p) {
    p <- pmax(lower + eps, pmin(upper - eps, p))
    log((p - lower) / (upper - p))
  }
  from_logit <- function(q) lower + (upper - lower) * stats::plogis(q)

  # Method-specific control defaults
  if (method == "L-BFGS-B") {
    # ndeps  — finite-difference step, 0.1 % of each parameter's range
    # factr  — stop when AIC improvement < ~0.0001 units (R default is 100x tighter)
    # pgtol  — projected-gradient complementary stopping rule
    if (is.null(control$ndeps)) control$ndeps <- pmax((upper - lower) * 1e-3, 1e-6)
    if (is.null(control$factr)) control$factr <- 1e9
    if (is.null(control$pgtol)) control$pgtol <- 1e-4
  } else {
    # reltol — relative tolerance on function values; R default (~1.5e-8) is
    #          far tighter than needed for AIC comparisons
    if (is.null(control$reltol)) control$reltol <- 1e-5
  }

  # Objective function to minimize (AIC).
  # Always receives parameters in the original bounded space.
  objective_function <- function(params, fn_env) {
    tryCatch({

      # Create weighted climate data using current parameters
      fitted_output <- fit_weights(
        range        = range,
        bio_data     = bio_data,
        climate_data = climate_data,
        cdate        = cdate,
        bdate        = bdate,
        xvar         = xvar,
        dfun         = dfun,
        par          = params,
        cinterval    = cinterval
      )

      bio_data <- fitted_output$bio_data

      # Fit the baseline model with the weighted climate data
      model <- eval(baseline)

      outputAIC <- AIC_fn(model)

      save_list <- c(setNames(as.list(params), par_labels), list(AIC = outputAIC))

      for (i in seq_along(save_list)) {
        fn_env$plot_save[[i]] <- append(fn_env$plot_save[[i]], save_list[[i]])
      }

      fn_env$iter         <- fn_env$iter + 1L
      fn_env$last_weights <- fitted_output$weights

      if (!is.null(plot_every) && fn_env$iter %% plot_every == 0L) {
        graphics::par(mfrow = mfrow_dims)
        plot(fn_env$last_weights, type = "l",
             ylab = "weight", xlab = "time step (e.g days)")
        plot(fn_env$plot_save[[n_par + 1]], type = "l",
             ylab = "AIC", xlab = "convergence step")
        for (j in seq_len(n_par)) {
          plot(fn_env$plot_save[[j]], type = "l",
               ylab = par_labels[j], xlab = "convergence step")
        }
        if (isTRUE(show_best)) draw_scatter(model, bio_data)
      }

      outputAIC

    }, error = function(e) {
      return(1e6)
    })
  }

  # Nelder-Mead wrapper: receives logit-space parameters, back-transforms to
  # bounded space, then delegates to objective_function.
  objective_logit <- function(q, fn_env) {
    objective_function(from_logit(q), fn_env)
  }

  summary_output <- data.frame()
  output <- list()

  for (i in 1:n) {

    if (i > 1) {
      for (j in 1:length(par)) {
        par[j] <- runif(n = 1, min = par_min[j], max = par_max[j])
      }
    }

    plot_save <- c(setNames(lapply(par, identity), par_labels), list(AIC = NA))
    iter <- 0L

    # Run optimisation in the appropriate space
    if (method == "L-BFGS-B") {
      optim_result <- optim(
        par     = par,
        fn      = objective_function,
        method  = "L-BFGS-B",
        control = control,
        fn_env  = environment(),
        lower   = lower,
        upper   = upper
      )
      optimal_par <- optim_result$par
    } else {
      # Nelder-Mead: optimise in logit space; bounds enforced by the transform
      optim_result <- optim(
        par     = to_logit(par),
        fn      = objective_logit,
        method  = "Nelder-Mead",
        control = control,
        fn_env  = environment()
      )
      optimal_par <- from_logit(optim_result$par)
    }

    # Get the optimal weighted climate data
    optimal_data <- fit_weights(
      range        = range,
      bio_data     = bio_data,
      climate_data = climate_data,
      cdate        = cdate,
      bdate        = bdate,
      xvar         = xvar,
      dfun         = dfun,
      par          = optimal_par,
      cinterval    = cinterval
    )

    bio_data   <- optimal_data$bio_data
    best_model <- eval(baseline)

    if (!is.null(plot_every)) {
      graphics::par(mfrow = mfrow_dims)
      plot(optimal_data$weights, type = "l",
           ylab = "weight", xlab = "time step (e.g days)")
      plot(plot_save[[n_par + 1]], type = "l",
           ylab = "AIC", xlab = "convergence step")
      for (j in seq_len(n_par)) {
        plot(plot_save[[j]], type = "l",
             ylab = par_labels[j], xlab = "convergence step")
      }
      if (isTRUE(show_best)) draw_scatter(best_model, bio_data)
    }

    output <- append(output,
                     list(list(
                       dataset    = as.data.frame(plot_save)[-1, ] |>
                                      ## Track optim step for plotting
                                      mutate(step = 1:n()) |>
                                      arrange(desc(AIC)),
                       bestModel  = list(model = best_model,
                                         data  = optimal_data$bio_data),
                       weights    = list(par     = optimal_par,
                                         weights = optimal_data$weights),
                       weightfunc = weightfunc_name
                     )))

    row_data <- c(
      setNames(as.list(par),         paste0("start_", par_labels)),
      setNames(as.list(optimal_par), paste0("end_",   par_labels)),
      list(AIC = AIC_fn(best_model))
    )
    summary_output <- bind_rows(summary_output, as.data.frame(row_data))

  }

  summary_output <- summary_output |> arrange(AIC)

  return(climwin_weightwin(weightwin_summary = summary_output,
                           weightwin_output  = output,
                           range             = range))
}
