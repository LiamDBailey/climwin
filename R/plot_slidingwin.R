#' Plot Sliding Window Analysis Results
#'
#' This function creates a heatmap visualization of sliding window analysis results.
#' The plot shows AIC values as a color gradient with End_Day on the x-axis and 
#' Start_Day on the y-axis, using geom_tile for the visualization.
#'
#' @param dataset A data frame output from run_slidingwin containing columns:
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
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 labs theme_minimal
#' @export
plot_slidingwin <- function(dataset) {
  
  # Calculate Delta AIC relative to null model (highest AIC)
  max_aic <- max(dataset$AIC, na.rm = TRUE)
  dataset$Delta_AIC <- dataset$AIC - max_aic
  
  # Create the heatmap
  p <- ggplot2::ggplot(dataset, ggplot2::aes(x = End_Day, y = Start_Day, fill = Delta_AIC)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "red", 
      mid = "yellow", 
      high = "blue",
      midpoint = median(dataset$Delta_AIC, na.rm = TRUE),
      na.value = "white",
      name = "ΔAICc"
    ) +
    ggplot2::labs(
      x = "Window close",
      y = "Window open",
      title = "ΔAICc (compared to null model)"
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.title = ggplot2::element_text(size = 11)
    )
  
  return(p)
} 