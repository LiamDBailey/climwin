#' Plot Climate Window Range Distribution
#'
#' This function creates stacked boxplots showing the distribution of Start_Day and End_Day
#' for all climate windows that fall within a specified cumulative weight threshold.
#'
#' @param dataset A data frame output from run_slidingwin containing columns:
#'                Start_Day, End_Day, AIC, and ModWeight
#' @param cw1 A numeric value between 0 and 1 defining the cumulative weight threshold
#'
#' @return A ggplot object showing the distribution of climate window parameters
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' results <- run_slidingwin(range = 0:100, 
#'                         climate_data = Climate, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data))
#' plot_window(results, cw1 = 0.95)
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot labs theme_minimal coord_flip
#' @export
plot_window <- function(dataset, cw1) {
  
  # Calculate cumulative sum of ModWeight
  dataset$cumulative_weight <- cumsum(dataset$ModWeight)
  
  # Identify models within the cumulative weight threshold
  within_threshold <- dataset$cumulative_weight <= cw1
  
  # Filter dataset to only include models within threshold
  filtered_data <- dataset[within_threshold, ]
  
  # Reshape data for plotting (long format)
  plot_data <- data.frame(
    Climate_Window = c(filtered_data$Start_Day, filtered_data$End_Day),
    Window_Type = rep(c("Window Open", "Window Close"), each = nrow(filtered_data))
  )
  
  # Create the boxplot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Window_Type, y = Climate_Window)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "",
      y = "Climate window",
      title = paste0("Climate window range for ", round(cw1 * 100), " % confidence set")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 14, hjust = 0.5)
    )
  
  return(p)
} 