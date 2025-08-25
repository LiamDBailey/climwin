#' Plot Sliding Window Analysis Results
#'
#' This function creates a combined visualization of sliding window analysis results
#' by combining multiple plot types using patchwork. Available plot types include
#' 'delta', 'weights', 'windows', and 'best'.
#'
#' @param dataset Output from run_slidingwin (a list with 'dataset' and 'bestModel' items)
#' @param cw1 A numeric value between 0 and 1 defining the cumulative weight threshold. Defaults to 0.95.
#' @param plots A character vector specifying which plots to include. Can include any of:
#'              'delta', 'weights', 'windows', 'best'. Defaults to c('delta', 'weights', 'windows', 'best').
#' @param. ... Additional arguments passed to `plot_best` to define model predictions.
#'
#' @return A patchwork object combining the specified plots
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' results <- run_slidingwin(range = 0:100, 
#'                         climate_data = Climate, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data))
#' 
#' # Create combined plot with all plots (default)
#' plot_slidingwin(results)
#' 
#' # Create combined plot with specific plots
#' plot_slidingwin(results, plots = c('weights', 'windows'))
#'
#' @importFrom patchwork wrap_plots
#' @export
plot_slidingwin <- function(dataset, cw1 = 0.95, plots = c('delta', 'weights', 'windows', 'best'), ...) {
  
  # Validate plots argument
  valid_plots <- c('delta', 'weights', 'windows', 'best')
  if (!all(plots %in% valid_plots)) {
    stop("Invalid plot type. 'plots' can only include: ", paste(valid_plots, collapse = ", "))
  }
  
  # Create list to store plots
  plot_list <- list()
  
  # Generate requested plots
  if ('delta' %in% plots) {
    plot_list$delta <- plot_delta(dataset)
  }
  
  if ('weights' %in% plots) {
    plot_list$weights <- plot_weights(dataset, cw1)
  }
  
  if ('windows' %in% plots) {
    plot_list$windows <- plot_window(dataset, cw1, ...)
  }
  
  if ('best' %in% plots) {
    plot_list$best <- plot_best(dataset, ...)
  }
  
  # Combine plots using patchwork
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(patchwork::wrap_plots(plot_list, ncol = 2))
  }
} 