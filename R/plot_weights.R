#' Plot Cumulative Weights Heatmap
#'
#' This function creates a heatmap visualization showing cumulative model weights.
#' It identifies models within a specified cumulative weight threshold and plots
#' them using geom_tile with cumulative weights as the color.
#'
#' @param dataset Output from run_slidingwin (either a data frame or a list with 'dataset' item) containing columns:
#'                Start_Day, End_Day, AIC, and ModWeight
#' @param cw1 A numeric value between 0 and 1 defining the cumulative weight threshold. Defaults to 0.95.
#'
#' @return A ggplot object showing the cumulative weights heatmap
#'
#' @examples
#' # Example usage:
#' data("MassClimate")
#' data("Mass")
#' results <- run_slidingwin(range = c(0, 100), 
#'                         climate_data = MassClimate, 
#'                         bio_data = Mass,
#'                         baseline = lm(Mass ~ climate, data = bio_data))
#' plot_weights(results, cw1 = 0.95)
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient labs theme_minimal scale_y_reverse
#' @export
plot_weights <- function(dataset, cw1 = 0.95, cw2 = 0.5, cw3 = 0.25) {
  
  # Handle new list structure from run_slidingwin
  dataset <- dataset@dataset
  
  a          <- c(cw1, cw2, cw3)
  b          <- a[order (-a)]
  WeightDist <- ceiling(100*mean(as.numeric(cumsum(dataset$ModWeight) <= cw1)))
  
  dataset$cw1    <- as.numeric(cumsum(dataset$ModWeight) <= cw1)
  dataset$cw2    <- as.numeric(cumsum(dataset$ModWeight) <= cw2)
  dataset$cw3    <- as.numeric(cumsum(dataset$ModWeight) <= cw3)
  dataset$cw.full <- dataset$cw1 + dataset$cw2 + dataset$cw3
  
  # Calculate cumulative sum of ModWeight
  dataset$cumulative_weight <- cumsum(dataset$ModWeight)
  
  # Identify models within the cumulative weight threshold
  dataset$within_threshold <- dataset$cumulative_weight <= cw1
  
  dataset$cw.full[which(dataset$cw.full == 3)] <- cw3
  dataset$cw.full[which(dataset$cw.full == 2)] <- cw2
  dataset$cw.full[which(dataset$cw.full == 1)] <- cw1
  dataset$cw.full[which(dataset$cw.full == 0)] <- 1
  
  # Create the heatmap
  p <- ggplot(dataset, aes(x = Start_Day, y = End_Day, z = cumulative_weight)) +
    geom_tile(aes(fill = cw.full)) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.5) +
    labs(title = paste(WeightDist, "% of models fall within the \n", 100*cw1, "% confidence set", sep = ""),
         y = "Window open", x = "Window close") +
    coord_cartesian(expand = FALSE) + 
    scale_fill_gradientn(colours = c("black", "grey98"), breaks=c(b[1], b[2], b[3]), limits = c(0, 1)) +
    theme_climwin() +
    theme(legend.position = c(0.85, 0.325),
          legend.title = element_blank(),
          legend.text.position = "left",
          legend.text = element_text(size = rel(3)))
  
  return(p)
} 
