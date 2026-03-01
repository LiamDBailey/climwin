#' Optimise Weibull Parameters for Weighted Climate Windows
#'
#' This function uses the 'optim' function to find the optimal Weibull parameters (shape and scale)
#' that create the best fitting model with the lowest AIC. It iteratively calls 'fit_weights'
#' to create weighted climate data and then fits the basemodel to extract AIC values.
#'
#' @param range A numeric vector specifying the number of days to look back from each date in bio_data.
#'              For example, 0 represents the date itself, while 100 represents 100 days before that date.
#' @param bio_data A data frame containing biological data with a date column. Required.
#' @param climate_data A data frame containing climate data. Required.
#' @param basemodel An lm model object that will be updated for each set of weights (e.g., lm(Mass ~ climate, data = bio_data)). Required.
#' @param cdate Character string specifying the name of the date column in climate_data. Defaults to "Date".
#' @param bdate Character string specifying the name of the date column in bio_data. Defaults to "Date".
#' @param xvar Character string specifying the name of the climate variable column in climate_data. Defaults to "Temp".
#' @param type Character string specifying the type of date range calculation. Must be either "relative" (default) or "absolute".
#' @param refday Character string in format "DD/MM/YYYY" specifying the reference date to use when type is "absolute".
#' @param par A numeric vector of length 3 containing initial Weibull function parameters:
#'            par[1] = shape, par[2] = scale, par[3] = location. Required.
#' @param method The optimization method to use. Defaults to "L-BFGS-B".
#' @param lower Lower bounds for the parameters. Defaults to c(0.1, 0.1).
#' @param upper Upper bounds for the parameters. Defaults to c(10, 1000).
#' @param weightfunc Character string specifying the weighting function. Either
#'   \code{"W"} (Weibull, default) or \code{"U"} (uniform). When \code{"U"},
#'   \code{par[1]} and \code{par[2]} represent the start and end of a uniform
#'   window (on the same scale as \code{range}), and \code{lower}/\code{upper}
#'   are set automatically from \code{range}.
#' @param control Additional control parameters for optim. Defaults to list(maxit = 100).
#' @param par_min A numeric vector of length 2 specifying the minimum values
#'   for randomly drawn starting parameters when n > 1. Defaults to \code{lower}
#'   when not specified.
#' @param par_max A numeric vector of length 2 specifying the maximum values
#'   for randomly drawn starting parameters when n > 1. Defaults to \code{upper}
#'   when not specified.
#'
#' @return A list containing:
#'         - par: The optimal Weibull parameters (shape, scale)
#'         - value: The minimum AIC value achieved
#'         - convergence: Convergence code from optim
#'         - message: Any messages from optim
#'         - counts: Function and gradient evaluation counts
#'         - optimal_data: The bio_data with the optimal weighted climate column
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' 
#' results <- run_weightwin(range = 0:100, 
#'                            bio_data = Mass,
#'                            climate_data = Climate,
#'                            cdate = "Date", bdate = "Date",
#'                            xvar = "Temp",
#'                            basemodel = lm(Mass ~ climate, data = bio_data),
#'                            par = c(1.25, 0.5))
#'                            
#' # Access the optimal parameters and data
#' optimal_params <- results$par
#' optimal_data <- results$optimal_data
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
                          lower = c(0.1, 0.1),
                          upper = c(10, 1000),
                          control = list(maxit = 100),
                          plot_every = 10,
                          par_min = NULL,
                          par_max = NULL) {
  
  # Validate basemodel
  validate_arg("basemodel", basemodel, required = TRUE)
  basemodel <- substitute(basemodel)

  weightfunc <- match.arg(weightfunc, choices = c("W", "U"))

  # For uniform weighting, bounds are determined by range
  if (weightfunc == "U") {
    lower <- c(min(range), min(range))
    upper <- c(max(range), max(range))
    if (par[1] > par[2])
      stop("For weightfunc = 'U', par[1] (window start) must be",
           " <= par[2] (window end)")
  }

  # Ensure lower < upper for each parameter
  if (any(lower >= upper)) stop("lower bounds must be less than upper bounds")
  if (any(par < lower) || any(par > upper)) stop("initial parameters must be within bounds")

  # Default random-start bounds to optimisation bounds when not specified
  if (is.null(par_min)) par_min <- lower
  if (is.null(par_max)) par_max <- upper

  if (any(par_min >= par_max))
    stop("par_min must be less than par_max for each parameter")
  
  # Objective function to minimize (AIC)
  objective_function <- function(params, fn_env) {
    tryCatch({
      
      # For uniform windows, snap parameters to integer day boundaries
      if (weightfunc == "U") params <- round(params)

      # Enforce ordering constraint for uniform windows
      if (weightfunc == "U" && params[1] > params[2]) return(1e6)

      # Create weighted climate data using current parameters
      fit_fn <- if (weightfunc == "U") fit_weights_uniform else fit_weights
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
      
      # Return AIC value
      outputAIC <- AIC(model)
      
      save_list <- list(shape = params[1], scale = params[2], AIC = outputAIC)
      
      for (i in 1:3){
        fn_env$plot_save[[i]] <- append(fn_env$plot_save[[i]], save_list[[i]])
      }

      fn_env$iter <- fn_env$iter + 1L
      fn_env$last_weights <- fitted_output$weights

      if (!is.null(plot_every) && fn_env$iter %% plot_every == 0L) {
        par(mfrow = c(2, 2))
        plot(fn_env$last_weights, type = "l", ylab = "weight", xlab = "time step (e.g days)", main = "Output of current weighted window being tested")
        plot(fn_env$plot_save[[3]], type = "l", ylab = "AIC", xlab = "convergence step")
        plot(fn_env$plot_save[[1]], type = "l", ylab = "shape parameter", xlab = "convergence step", main = "Weibull parameter values being tested")
        plot(fn_env$plot_save[[2]], type = "l", ylab = "scale parameter", xlab = "convergence step")
      }
      
      outputAIC
      
    }, error = function(e) {
      # Return a very high AIC if there's an error
      return(1e6)
    })
  }
  
  summary_output <- data.frame(start_par1 = numeric(), start_par2 = numeric(), AIC = numeric())
  output <- list()
  
  for (i in 1:n){
    
    if (i > 1){
      for (j in 1:length(par)){
        par[j] <- runif(n = 1, min = par_min[j], max = par_max[j])
      }
    }
    
    plot_save <- list(shape = par[1], scale = par[2], AIC = NA)
    iter <- 0L

    # Run optimization
    optim_result <- optim(
      par = par,
      fn = objective_function,
      method = method,
      control = control,
      fn_env = environment(),
      lower = lower, 
      upper = upper
    )
    
    # Get the optimal weighted climate data
    optimal_par <- if (weightfunc == "U") round(optim_result$par) else optim_result$par
    fit_fn <- if (weightfunc == "U") fit_weights_uniform else fit_weights
    optimal_data <- fit_fn(
      range = range,
      bio_data = bio_data,
      climate_data = climate_data,
      cdate = cdate,
      bdate = bdate,
      xvar = xvar,
      par = optimal_par
    )
    
    bio_data <- optimal_data$bio_data
    best_model <- eval(basemodel)

    if (!is.null(plot_every)) {
      par(mfrow = c(2, 2))
      plot(optimal_data$weights, type = "l", ylab = "weight",
           xlab = "time step (e.g days)",
           main = "Output of current weighted window being tested")
      plot(plot_save[[3]], type = "l", ylab = "AIC",
           xlab = "convergence step")
      plot(plot_save[[1]], type = "l", ylab = "shape parameter",
           xlab = "convergence step",
           main = "Weibull parameter values being tested")
      plot(plot_save[[2]], type = "l", ylab = "scale parameter",
           xlab = "convergence step")
    }
    
    output <- append(output,
                     list(list(dataset = as.data.frame(plot_save)[-1, ] |> 
                                 arrange(desc(AIC)),
                               bestModel = list(model = best_model,
                                                data = optimal_data$bio_data),
                               weights = list(par = optimal_par,
                                              weights = optimal_data$weights))))
    
    summary_output <- bind_rows(summary_output,
                                data.frame(start_par1 = par[1], end_par1 = optim_result$par[1],
                                           start_par2 = par[2], end_par1 = optim_result$par[2],
                                           AIC = AIC(best_model)))
    
  }
  
  summary_output <- summary_output |> arrange(AIC)
  
  # Return results
  return(climwin_weightwin(weightwin_summary = summary_output,
                           weightwin_output = output,
                           range = range))
}
