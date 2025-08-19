#' Plot Best Model Fit
#'
#' This function creates a scatterplot showing the relationship between the climate variable
#' and biological response from the best fitting model (lowest AIC) identified by run_slidingwin.
#' The plot includes the data points and the fitted regression line.
#'
#' @param dataset Output from run_slidingwin (a list with 'dataset' and 'bestModel' items)
#'
#' @return A ggplot object showing the scatter plot with fitted line
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' results <- run_slidingwin(range = 0:100, 
#'                         climate_data = Climate, 
#'                         bio_data = Mass,
#'                         basemodel = lm(Mass ~ climate, data = bio_data))
#' plot_best(results)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal
#' @importFrom rlang .data
#' @export
plot_best <- function(dataset) {
  
  # Extract the best model from the list
  if (!is.list(dataset) || !"bestModel" %in% names(dataset)) {
    stop("Input must be output from run_slidingwin containing 'bestModel' item")
  }
  
  best_model <- dataset$bestModel
  if (is.null(best_model)) {
    stop("No valid best model found in the dataset")
  }
  
  # Extract model data
  model_data <- best_model$model
  
  # Get variable names from the model
  response_var <- names(model_data)[1]  # First column is typically the response
  climate_var <- "climate"  # Climate variable is always named "climate"
  
  # Create the scatter plot with fitted line
  p <- ggplot2::ggplot(model_data, ggplot2::aes(x = .data[[climate_var]], y = .data[[response_var]])) +
    ggplot2::geom_point(color = "gray60", alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
    ggplot2::labs(
      x = "Climate variable",
      y = "Biological response",
      title = "Output of best model"
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