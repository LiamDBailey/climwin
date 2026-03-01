#' climwin S7 object output from run_slidingwin
#'
#' Allows us to have generic print and plot methods.
#' 
#' @examples
#' ## Create a climwin object from `run_slidingwin`
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' results <- run_slidingwin(range = 0:2, 
#'                         climate_data = Climate, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data))
#'
#' ## Use methods to extract dataset and bestmodel
#' getDataset(results)
#' getBestModel(results)
#' 
#' ## Use generic methods
#' ## Using 'plot' is the same as 'plot_slidingwin'
#' plot(results, y = "Mass")
#' 
#' @import S7
climwin <- S7::new_class("climwin", properties = list(
  dataset = class_data.frame,
  bestModel = class_list,
  range = class_numeric
))

S7::method(print, climwin) <- function(x) {
  minrange <- x@range[1]
  maxrange <- x@range[2]
  # bestwindow_start <- x@dataset$Start_Day[1]
  # bestwindow_end <- x@dataset$End_Day[1]
  bestwindow_start <- 0
  bestwindow_end <- 0
  model_formula <- Reduce(paste, deparse(formula(x@bestModel$model)))
  cat(glue::glue("Slidingwin output using climwin:
      Range: {minrange} - {maxrange}
      Best Window: {bestwindow_start} - {bestwindow_end}
      Model formula: {model_formula}"))
}

S7::method(plot, climwin) <- function(x, ...) {
  plot_slidingwin(x, ...)
}

getDataset <- new_generic("getDataset", "x")
S7::method(getDataset, climwin) <- function(x, ...) {
  x@dataset
}

getBestModel <- new_generic("getBestModel", "x")
S7::method(getBestModel, climwin) <- function(x, ...) {
  x@bestModel$model
}

getBestModelData <- new_generic("getBestModelData", "x")
S7::method(getBestModelData, climwin) <- function(x, ...) {
  x@bestModel$data
}

#' climwin_weightwin S7 object output from run_weightwin
#'
#' Allows us to have generic print and plot methods.
#' 
#' @examples
#' ## Create a climwin object from `run_weightwin`
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' results <- run_slidingwin(range = 0:2, 
#'                         climate_data = Climate, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data))
#'
#' ## Same method names exist, but behave differently internally
#' getDataset(results)
#' getBestModel(results)
#' 
#' ## Use generic methods
#' ## Using 'plot' is the same as 'plot_slidingwin'
#' plot(results, y = "Mass")
#' 
#' @import S7
climwin_weightwin <- S7::new_class("climwin_weightwin", properties = list(
  weightwin_summary = class_data.frame,
  weightwin_output = class_list,
  range = class_numeric
))

S7::method(print, climwin_weightwin) <- function(x) {
  minrange <- x@range[1]
  maxrange <- x@range[2]
  cat(glue::glue("Weightwin output using climwin:
      Range: {minrange} - {maxrange}"))
}

S7::method(plot, climwin_weightwin) <- function(x, ...) {
  plot_weightwin(x, ...)
}

S7::method(getDataset, climwin_weightwin) <- function(x, n) {
  output <- x@weightwin_output
  
  if (length(output) > 1 & missing(n)) {
    n <- 1
    message("Returning output from top iteration")
  }
  
  return(output[[n]]$dataset)
  
}

S7::method(getBestModel, climwin_weightwin) <- function(x, n) {
  output <- x@weightwin_output
  
  if (length(output) > 1 & missing(n)) {
    n <- 1
    message("Returning output from top iteration")
  }
  
  return(output[[n]]$bestModel$model)
}

S7::method(getBestModelData, climwin_weightwin) <- function(x, n) {
  output <- x@weightwin_output
  
  if (length(output) > 1 & missing(n)) {
    n <- 1
    message("Returning output from top iteration")
  }
  
  return(output[[n]]$bestModel$data)
}

getWeights <- new_generic("getWeights", "x")
S7::method(getWeights, climwin_weightwin) <- function(x, n) {
  output <- x@weightwin_output
  
  if (length(output) > 1 & missing(n)) {
    n <- 1
    message("Returning output from top iteration")
  }
  
  return(output[[n]]$weights$weights)
}