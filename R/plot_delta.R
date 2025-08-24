#' Plot Delta AIC Heatmap
#'
#' This function creates a heatmap visualization of sliding window analysis results.
#' The plot shows Delta AIC values as a color gradient with End_Day on the x-axis and 
#' Start_Day on the y-axis, using geom_tile for the visualization.
#'
#' @param dataset Output from run_slidingwin (either a data frame or a list with 'dataset' item) containing columns:
#'                Start_Day, End_Day, and AIC
#'
#' @return A ggplot object showing the heatmap of AIC values
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' results <- run_slidingwin(range = 0:100, 
#'                         climate_data = Climate, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data))
#' plot_slidingwin(results)
#'
#' @export
#' @import ggplot2
plot_delta <- function(dataset) {
  
  # Handle new list structure from run_slidingwin
  if (is.list(dataset) && "dataset" %in% names(dataset)) {
    dataset <- dataset$dataset
  }
  
  # Calculate Delta AIC relative to null model (highest AIC)
  max_aic <- max(dataset$AIC, na.rm = TRUE)
  dataset$Delta_AIC <- dataset$AIC - max_aic
  
  # Create the heatmap
  p <- ggplot(dataset, aes(x = Start_Day, y = End_Day, z = Delta_AIC)) +
    geom_tile(aes(fill = Delta_AIC)) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.5) +
    scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
    theme_climwin() +
    theme(legend.position = c(0.75, 0.3)) +
    coord_cartesian(expand = FALSE) + 
    ggtitle(expression(paste(Delta, "AICc (compared to null model)"))) +
    ylab("Window open") +
    xlab("Window close")
  
  return(p)
  
} 