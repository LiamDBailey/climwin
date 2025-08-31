#' Optimise Weibull Parameters for Weighted Climate Windows
#'
#' This function uses the 'optim' function to find the optimal Weibull parameters (shape and scale)
#' that create the best fitting model with the lowest AIC. It iteratively calls 'fit_weightwin'
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
#' # Initial Weibull parameters: shape = 2, scale = 50, location = 0
#' results <- optimise_weights(range = 0:100, 
#'                            bio_data = Mass,
#'                            climate_data = Climate,
#'                            cdate = "Date", bdate = "Date",
#'                            xvar = "Temp",
#'                            basemodel = lm(Mass ~ climate, data = bio_data),
#'                            par = c(2, 50, 0))
#'                            
#' # Access the optimal parameters and data
#' optimal_params <- results$par
#' optimal_data <- results$optimal_data
#'
#' @export
optimise_weights <- function(range,
                            bio_data,
                            climate_data,
                            basemodel,
                            cdate,
                            bdate,
                            xvar,
                            par,
                            method = "L-BFGS-B",
                            lower = c(0.1, 0.1, 0),
                            upper = c(10, 1000, 100),
                            control = list(maxit = 100)) {
  
  ### ARGUMENT CHECKS ####
  # Validate par parameter
  validate_arg("par", par, required = TRUE, type = "numeric",
               additional_checks = function(x) {
                 if(length(x) != 3) stop("par must be a numeric vector of length 3")
                 if(any(x <= 0)) stop("all par values must be positive")
               })
  
  # Validate basemodel
  validate_arg("basemodel", basemodel, required = TRUE)
  
  # Validate bounds
  validate_arg("lower", lower, required = FALSE, type = "numeric",
               additional_checks = function(x) {
                 if(length(x) != 3) stop("lower must be a numeric vector of length 3")
                 if(any(x < 0)) stop("all lower bounds must be positive")
               })
  
  validate_arg("upper", upper, required = FALSE, type = "numeric",
               additional_checks = function(x) {
                 if(length(x) != 3) stop("upper must be a numeric vector of length 3")
                 if(any(x <= 0)) stop("all upper bounds must be positive")
               })
  
  # Ensure lower < upper for each parameter
  if (any(lower >= upper)) stop("lower bounds must be less than upper bounds")
  
  # Ensure initial parameters are within bounds
  if (any(par < lower) || any(par > upper)) stop("initial parameters must be within bounds")
  
  ## Handle basemodel substitution
  basemodel <- substitute(basemodel)
  
  # Objective function to minimize (AIC)
  objective_function <- function(params) {
    tryCatch({
      # Create weighted climate data using current parameters
      bio_data <- fit_weightwin(
        range = range,
        bio_data = bio_data,
        climate_data = climate_data,
        cdate = cdate,
        bdate = bdate,
        xvar = xvar,
        par = params
      )
      
      # Fit the basemodel with the weighted climate data
      model <- eval(basemodel)
      
      # Return AIC value
      AIC(model)
      
    }, error = function(e) {
      # Return a very high AIC if there's an error
      return(1e6)
    })
  }
  
  # Run optimization
  optim_result <- optim(
    par = par,
    fn = objective_function,
    method = method,
    lower = lower,
    upper = upper,
    control = control
  )
  
  # Get the optimal weighted climate data
  optimal_data <- fit_weightwin(
    range = range,
    bio_data = bio_data,
    climate_data = climate_data,
    cdate = cdate,
    bdate = bdate,
    xvar = xvar,
    par = optim_result$par
  )
  
  # Return results
  return(list(
    par = optim_result$par,
    value = optim_result$value,
    convergence = optim_result$convergence,
    message = optim_result$message,
    counts = optim_result$counts,
    optimal_data = optimal_data
  ))
}
