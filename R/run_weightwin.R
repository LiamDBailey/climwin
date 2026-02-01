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
#' @param control Additional control parameters for optim. Defaults to list(maxit = 100).
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
run_weightwin <- function(n = 1, range,
                          bio_data,
                          climate_data,
                          basemodel,
                          cdate,
                          bdate,
                          xvar,
                          par = c(3, 0.2),
                          type = "relative",
                          refday = NULL,
                          method = "L-BFGS-B",
                          lower = c(0.0001, 0.0001), 
                          upper = c(Inf, Inf),
                          control = list(maxit = 100)) {
  
  # Validate basemodel
  validate_arg("basemodel", basemodel, required = TRUE)
  
  basemodel <- substitute(basemodel)
  
  # Ensure lower < upper for each parameter
  if (any(lower >= upper)) stop("lower bounds must be less than upper bounds")
  if (any(par < lower) || any(par > upper)) stop("initial parameters must be within bounds")
  
  ## Handle basemodel substitution
  # basemodel <- substitute(basemodel)
  
  # Objective function to minimize (AIC)
  objective_function <- function(params, fn_env) {
    tryCatch({
      
      # Create weighted climate data using current parameters
      fitted_output <- fit_weights(
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
      
      par(mfrow = c(2, 2))
      plot(fitted_output$weights, type = "l", ylab = "weight", xlab = "time step (e.g days)", main = "Output of current weighted window being tested")
      plot(fn_env$plot_save[[3]], type = "l", ylab = "AIC", xlab = "convergence step")
      plot(fn_env$plot_save[[1]], type = "l", ylab = "shape parameter", xlab = "convergence step", main = "Weibull parameter values being tested")
      plot(fn_env$plot_save[[2]], type = "l", ylab = "scale parameter", xlab = "convergence step")
      
      outputAIC
      
    }, error = function(e) {
      # Return a very high AIC if there's an error
      return(1e6)
    })
  }
  
  plot_save <- list(shape = par[1], scale = par[2], AIC = NA)
  
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
  optimal_data <- fit_weights(
    range = range,
    bio_data = bio_data,
    climate_data = climate_data,
    cdate = cdate,
    bdate = bdate,
    xvar = xvar,
    par = optim_result$par
  )
  
  bio_data <- optimal_data$bio_data
  best_model <- eval(basemodel)
  
  # Return results
  return(climwin(dataset = as.data.frame(plot_save)[-1, ] |> 
                   arrange(desc(AIC)),
                 bestModel = list(model = best_model,
                                  data = optimal_data$bio_data),
                 range = range,
                 weights = list(par = optim_result$par,
                                weights = optimal_data$weights)))
}
