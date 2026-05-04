#' Plot Weighted Window Analysis Results
#'
#' This function creates a combined visualization of weighted window analysis results
#' by combining multiple plot types using patchwork. Available plot types include
#' 'weibull' and 'best'.
#'
#' @param dataset Output from run_weightwin (a climwin object with 'dataset', 'bestModel', 'range', and 'weights' slots)
#' @param plots A character vector specifying which plots to include. Can include any of:
#'              'weibull', 'best'. Defaults to c('weibull', 'best').
#' @param. ... Additional arguments passed to `plot_best` to define model predictions.
#'
#' @return A patchwork object combining the specified plots
#'
#' @examples
#' # Example usage:
#' data("MassClimate")
#' data("Mass")
#' results <- run_weightwin(range = 0:100, 
#'                         bio_data = Mass,
#'                         climate_data = MassClimate,
#'                         cdate = "Date", bdate = "Date",
#'                         xvar = "Temp",
#'                         basemodel = lm(Mass ~ climate, data = bio_data),
#'                         par = c(1.25, 0.5))
#' 
#' # Create combined plot with all plots (default)
#' plot_weightwin(results, y = "Mass")
#' 
#' # Create combined plot with specific plots
#' plot_weightwin(results, plots = c('weibull'))
#'
#' @importFrom patchwork wrap_plots
#' @export
plot_weightwin <- function(dataset, plots = c("weibull", "best"), ...){
  
  valid_plots <- c('weibull', 'best')
  if (!all(plots %in% valid_plots)) {
    stop("Invalid plot type. 'plots' can only include: ", paste(valid_plots, collapse = ", "))
  }
  
  # Create list to store plots
  plot_list <- list()
  
  # Generate requested plots
  if ('weibull' %in% plots) {
    plot_list$weibull <- plot_weibull(dataset)
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
