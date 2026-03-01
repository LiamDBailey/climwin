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
#' @param method Optimisation method passed to \code{optim}.
#'   Defaults to \code{"L-BFGS-B"}.
#' @param lower Lower bounds for the parameters. Set automatically for
#'   built-in \code{weightfunc} values when \code{NULL} (default).
#' @param upper Upper bounds for the parameters. Set automatically for
#'   built-in \code{weightfunc} values when \code{NULL} (default).
#' @param control List of control parameters passed to \code{optim}.
#'   Defaults to \code{list(maxit = 100)}.
#' @param plot_every Integer or \code{NULL}. Plot diagnostics every
#'   \code{plot_every} objective evaluations, and always after convergence.
#'   Pass \code{NULL} to suppress all plots.
#' @param par_min Numeric vector — lower bounds for random starting parameters
#'   when \code{n > 1}. Defaults to \code{lower}.
#' @param par_max Numeric vector — upper bounds for random starting parameters
#'   when \code{n > 1}. Defaults to \code{upper}.
#' @param cinterval Character string specifying the temporal resolution: \code{"day"} (default),
#'   \code{"week"}, or \code{"month"}. When \code{"month"}, climate data are aggregated to monthly
#'   means and \code{range} is interpreted in months. When \code{"week"}, data are aggregated to
#'   7-day blocks and \code{range} is in weeks.
#' @param aggfunc A function used to aggregate climate values within each period when
#'   \code{cinterval} is \code{"month"} or \code{"week"}. Defaults to \code{mean}. Any function
#'   that accepts a numeric vector and returns a single value is valid (e.g. \code{sum},
#'   \code{max}, \code{min}, \code{median}). Ignored when \code{cinterval = "day"}.
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
                          par_max = NULL,
                          cinterval = "day",
                          aggfunc = mean,
                          .basemodelIsCall = FALSE) {

  # Validate basemodel
  validate_arg("basemodel", basemodel, required = TRUE)
  if (!isTRUE(.basemodelIsCall)) basemodel <- substitute(basemodel)

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
      if (is.null(upper)) upper <- c(10, 1000)
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

  n_par      <- length(par_labels)
  mfrow_dims <- if (n_par >= 3) c(2, 3) else c(2, 2)

  # Ensure lower < upper for each parameter
  if (any(lower >= upper)) stop("lower bounds must be less than upper bounds")
  if (any(par < lower) || any(par > upper))
    stop("initial parameters must be within bounds")

  # Default random-start bounds to optimisation bounds when not specified
  if (is.null(par_min)) par_min <- lower
  if (is.null(par_max)) par_max <- upper

  if (any(par_min >= par_max))
    stop("par_min must be less than par_max for each parameter")

  # Objective function to minimize (AIC)
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
        cinterval    = cinterval,
        aggfunc      = aggfunc
      )

      bio_data <- fitted_output$bio_data

      # Fit the basemodel with the weighted climate data
      model <- eval(basemodel)

      outputAIC <- AIC(model)

      save_list <- c(setNames(as.list(params), par_labels), list(AIC = outputAIC))

      for (i in seq_along(save_list)) {
        fn_env$plot_save[[i]] <- append(fn_env$plot_save[[i]], save_list[[i]])
      }

      fn_env$iter         <- fn_env$iter + 1L
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
      setNames(as.list(par),              paste0("start_", par_labels)),
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
