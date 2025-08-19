#' Plot Cumulative Weights Heatmap
#'
#' This function creates a heatmap visualization showing cumulative model weights.
#' It identifies models within a specified cumulative weight threshold and plots
#' them using geom_tile with cumulative weights as the color.
#'
#' @param dataset A data frame output from run_slidingwin containing columns:
#'                Start_Day, End_Day, AIC, and ModWeight
#' @param cw1 A numeric value between 0 and 1 defining the cumulative weight threshold
#'
#' @return A ggplot object showing the cumulative weights heatmap
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' results <- run_slidingwin(range = 0:100, 
#'                         climate_data = Climate, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data))
#' plot_weights(results, cw1 = 0.95)
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient labs theme_minimal scale_y_reverse
#' @export
plot_weights <- function(dataset, cw1) {
  
  # Calculate cumulative sum of ModWeight
  dataset$cumulative_weight <- cumsum(dataset$ModWeight)
  
  # Identify models within the cumulative weight threshold
  dataset$within_threshold <- dataset$cumulative_weight <= cw1
  
  # Create the heatmap
  p <- ggplot2::ggplot(dataset, ggplot2::aes(x = End_Day, y = Start_Day, fill = cumulative_weight)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "black",
      na.value = "white",
      name = "Cumulative Weight"
    ) +
    ggplot2::labs(
      x = "Window close",
      y = "Window open",
      title = paste0(round(cw1 * 100), "% of models fall within the ", round(cw1 * 100), "% confidence set")
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