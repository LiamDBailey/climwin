#' Run Multiple Weighted Climate Window Optimizations
#'
#' This function runs the weighted climate window optimization multiple times with
#' randomly generated Weibull parameters. For each iteration, it generates random
#' shape and scale parameters from a uniform distribution and calls run_weightwin
#' to find the optimal model. Returns a dataframe with the top model results for
#' each iteration.
#'
#' @param n Integer specifying the number of iterations to run. Required.
#' @param range A numeric vector specifying the number of days to look back from each date in bio_data.
#'              For example, 0 represents the date itself, while 100 represents 100 days before that date.
#' @param bio_data A data frame containing biological data with a date column. Required.
#' @param climate_data A data frame containing climate data. Required.
#' @param basemodel An lm model object that will be updated for each set of weights (e.g., lm(Mass ~ climate, data = bio_data)). Required.
#' @param cdate Character string specifying the name of the date column in climate_data. Defaults to "Date".
#' @param bdate Character string specifying the name of the date column in bio_data. Defaults to "Date".
#' @param xvar Character string specifying the name of the climate variable column in climate_data. Defaults to "Temp".
#' @param method The optimization method to use. Defaults to "L-BFGS-B".
#' @param lower Lower bounds for the parameters. Defaults to c(0.1, 0.1).
#' @param upper Upper bounds for the parameters. Defaults to c(10, 1000).
#' @param control Additional control parameters for optim. Defaults to list(maxit = 100).
#' @param par_min Minimum value for random parameter generation. Defaults to 0.1.
#' @param par_max Maximum value for random parameter generation. Defaults to 10.
#'
#' @return A data frame containing the top model results for each iteration with columns:
#'         - iteration: The iteration number (1 to n)
#'         - shape: The optimal shape parameter found
#'         - scale: The optimal scale parameter found
#'         - AIC: The AIC value of the best model
#'         - convergence: Convergence code from optim
#'         - model: The best model object for this iteration
#'         - data: The optimal data used for this model
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' 
#' results <- run_weightwin_n(n = 5,
#'                           range = 0:100, 
#'                           bio_data = Mass,
#'                           climate_data = Climate,
#'                           cdate = "Date", 
#'                           bdate = "Date",
#'                           xvar = "Temp",
#'                           basemodel = lm(Mass ~ climate, data = bio_data))
#'                           
#' # View the results
#' print(results)
#'
#' @export
run_weightwin_n <- function(n,
                            range,
                            bio_data,
                            climate_data,
                            basemodel,
                            cdate = "Date",
                            bdate = "Date", 
                            xvar = "Temp",
                            method = "L-BFGS-B",
                            lower = c(0.1, 0.1),
                            upper = c(10, 1000),
                            control = list(maxit = 100),
                            par_min = 0.1,
                            par_max = 10) {
  
  basemodel <- substitute(basemodel)
  
  output_data <- tibble()
  
  for (i in 1:n){
    
    random_shape <- runif(n = 1, min = par_min, max = par_max)
    random_scale <- runif(n = 1, min = par_min, max = par_max)
    random_params <- c(random_shape, random_scale)
    
    output_n <- run_weightwin(range = range,
                              bio_data = bio_data, climate_data = climate_data,
                              basemodel = basemodel,
                              cdate = cdate, bdate = bdate,
                              xvar = xvar,
                              method = method,
                              lower = lower, upper = upper,
                              control = control,
                              par = random_params)
    
    top_model <- tibble(n = i, start_shape = random_shape,
                            start_scale = random_scale) |> 
      dplyr::bind_cols(output_n@dataset[1, ]) |> 
      mutate(bestModel = list(output_n@bestModel),
             weights = list(output_n@weights))
    
    output_data <- dplyr::bind_rows(output_data, top_model)
    
  }
  
  output_data <- output_data |> 
    arrange(desc(AIC))
  
  return(climwin(
    dataset = output_data,
    bestModel = output_data$bestModel[[1]],
    range = range(range),
    weights = output_data$weights[[1]]
  ))
  
}
