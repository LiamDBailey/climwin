#' Optimise Parameters for Weighted Climate Windows
#'
#' Uses \code{optim} to find the optimal distribution parameters that minimise
#' AIC when creating weighted means of climate data.  Supports Weibull
#' (\code{"W"}), Gumbel (\code{"G"}), and Frechet (\code{"F"}) weighting
#' functions.
#'
#' @param n Integer. Number of independent optimisation runs. The first run
#'   uses \code{par}; subsequent runs draw starting parameters uniformly from
#'   [\code{par_min}, \code{par_max}].
#' @param range A numeric vector specifying the time steps to look back from
#'   each date in \code{bio_data} (e.g. \code{0:100}).
#' @param bio_data A data frame containing biological data with a date column.
#' @param climate_data A data frame containing climate data.
#' @param basemodel An \code{lm} call used as the model template
#'   (e.g. \code{lm(Mass ~ climate, data = bio_data)}).
#' @param cdate Character string — date column in \code{climate_data}.
#'   Defaults to \code{"Date"}.
#' @param bdate Character string — date column in \code{bio_data}.
#'   Defaults to \code{"Date"}.
#' @param xvar Character string — climate variable column in
#'   \code{climate_data}.
#' @param par Numeric vector of initial distribution parameters. Length depends
#'   on \code{weightfunc}: 2 for \code{"W"} and \code{"G"};
#'   3 for \code{"F"} (loc, scale, shape).
#' @param type \code{"relative"} (default) or \code{"absolute"}.
#' @param refday Reference date (\code{"DD/MM/YYYY"}) used when
#'   \code{type = "absolute"}.
#' @param weightfunc Character string specifying the weighting function:
#'   \code{"W"} (Weibull, default), \code{"G"} (Gumbel), or \code{"F"}
#'   (Frechet). For \code{"F"}, \code{par} must have 3 elements and plots use
#'   a 3x3 grid.
#' @param method Optimisation method passed to \code{optim}.
#'   Defaults to \code{"L-BFGS-B"}.
#' @param lower Lower bounds for the parameters. Set automatically per
#'   \code{weightfunc} when \code{NULL} (default).
#' @param upper Upper bounds for the parameters. Set automatically per
#'   \code{weightfunc} when \code{NULL} (default).
#' @param control List of control parameters passed to \code{optim}.
#'   Defaults to \code{list(maxit = 100)}.
#' @param plot_every Integer or \code{NULL}. Plot diagnostics every
#'   \code{plot_every} objective evaluations, and always after convergence.
#'   Pass \code{NULL} to suppress all plots.
#' @param par_min Numeric vector — lower bounds for random starting parameters
#'   when \code{n > 1}. Defaults to \code{lower}.
#' @param par_max Numeric vector — upper bounds for random starting parameters
#'   when \code{n > 1}. Defaults to \code{upper}.
#'
#' @return A \code{climwin_weightwin} S7 object.
#'
#' @examples
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#'
#' results <- run_weightwin(range = 0:100,
#'                          bio_data = Mass,
#'                          climate_data = Climate,
#'                          cdate = "Date", bdate = "Date",
#'                          xvar = "Temp",
#'                          basemodel = lm(Mass ~ climate, data = bio_data),
#'                          par = c(1.25, 0.5))
#'
#' @export
run_weightwin <- function(n = 1,
                          range,
                          bio_data,
                          climate_data,
                          basemodel,
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
                          par_max = NULL) {

  # Validate basemodel
  validate_arg("basemodel", basemodel, required = TRUE)
  basemodel <- substitute(basemodel)

  weightfunc <- match.arg(weightfunc, choices = c("W", "G", "F"))

  # Set bounds, labels, and validate par based on weightfunc
  if (weightfunc == "W") {
    if (is.null(lower)) lower <- c(0.1, 0.1)
    if (is.null(upper)) upper <- c(10, 1000)
    par_labels <- c("shape", "scale")
  } else if (weightfunc == "G") {
    if (is.null(lower)) lower <- c(-0.5, 0.01)
    if (is.null(upper)) upper <- c(1.5, 5)
    par_labels <- c("loc", "scale")
  } else if (weightfunc == "F") {
    if (length(par) != 3)
      stop("For weightfunc = 'F', par must have 3 elements: c(loc, scale, shape)")
    if (is.null(lower)) lower <- c(0, 0.01, 0.1)
    if (is.null(upper)) upper <- c(1, 2, 10)
    par_labels <- c("loc", "scale", "shape")
  }

  n_par      <- length(par_labels)
  mfrow_dims <- if (n_par >= 3) c(3, 3) else c(2, 2)

  # Ensure lower < upper for each parameter
  if (any(lower >= upper)) stop("lower bounds must be less than upper bounds")
  if (any(par < lower) || any(par > upper))
    stop("initial parameters must be within bounds")

  # Default random-start bounds to optimisation bounds when not specified
  if (is.null(par_min)) par_min <- lower
  if (is.null(par_max)) par_max <- upper

  if (any(par_min >= par_max))
    stop("par_min must be less than par_max for each parameter")

  fit_fn <- switch(weightfunc,
    "W" = fit_weights,
    "G" = fit_weights_gumbel,
    "F" = fit_weights_frechet
  )

  # Objective function to minimize (AIC)
  objective_function <- function(params, fn_env) {
    tryCatch({

      # Create weighted climate data using current parameters
      fitted_output <- fit_fn(
        range = range,
        bio_data = bio_data,
        climate_data = climate_data,
        cdate = cdate,
        bdate = bdate,
        xvar = xvar,
        par = params
      )

      bio_data <- fitted_output$bio_data

      # Fit the basemodel with the weighted climate data
      model <- eval(basemodel)

      outputAIC <- AIC(model)

      save_list <- c(setNames(as.list(params), par_labels), list(AIC = outputAIC))

      for (i in seq_along(save_list)) {
        fn_env$plot_save[[i]] <- append(fn_env$plot_save[[i]], save_list[[i]])
      }

      fn_env$iter        <- fn_env$iter + 1L
      fn_env$last_weights <- fitted_output$weights

      if (!is.null(plot_every) && fn_env$iter %% plot_every == 0L) {
        par(mfrow = mfrow_dims)
        plot(fn_env$last_weights, type = "l",
             ylab = "weight", xlab = "time step (e.g days)")
        plot(fn_env$plot_save[[n_par + 1]], type = "l",
             ylab = "AIC", xlab = "convergence step")
        for (j in seq_len(n_par)) {
          plot(fn_env$plot_save[[j]], type = "l",
               ylab = par_labels[j], xlab = "convergence step")
        }
      }

      outputAIC

    }, error = function(e) {
      return(1e6)
    })
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

    # Run optimization
    optim_result <- optim(
      par     = par,
      fn      = objective_function,
      method  = method,
      control = control,
      fn_env  = environment(),
      lower   = lower,
      upper   = upper
    )

    # Get the optimal weighted climate data
    optimal_par  <- optim_result$par
    optimal_data <- fit_fn(
      range        = range,
      bio_data     = bio_data,
      climate_data = climate_data,
      cdate        = cdate,
      bdate        = bdate,
      xvar         = xvar,
      par          = optimal_par
    )

    bio_data   <- optimal_data$bio_data
    best_model <- eval(basemodel)

    if (!is.null(plot_every)) {
      par(mfrow = mfrow_dims)
      plot(optimal_data$weights, type = "l",
           ylab = "weight", xlab = "time step (e.g days)")
      plot(plot_save[[n_par + 1]], type = "l",
           ylab = "AIC", xlab = "convergence step")
      for (j in seq_len(n_par)) {
        plot(plot_save[[j]], type = "l",
             ylab = par_labels[j], xlab = "convergence step")
      }
    }

    output <- append(output,
                     list(list(
                       dataset   = as.data.frame(plot_save)[-1, ] |>
                                     arrange(desc(AIC)),
                       bestModel = list(model = best_model,
                                        data  = optimal_data$bio_data),
                       weights   = list(par     = optimal_par,
                                        weights = optimal_data$weights),
                       weightfunc = weightfunc
                     )))

    row_data <- c(
      setNames(as.list(par),             paste0("start_", par_labels)),
      setNames(as.list(optim_result$par), paste0("end_",   par_labels)),
      list(AIC = AIC(best_model))
    )
    summary_output <- bind_rows(summary_output, as.data.frame(row_data))

  }

  summary_output <- summary_output |> arrange(AIC)

  return(climwin_weightwin(weightwin_summary = summary_output,
                           weightwin_output  = output,
                           range             = range))
}
