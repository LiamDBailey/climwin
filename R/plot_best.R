#' Plot Best Model Fit
#'
#' This function creates a scatterplot showing the relationship between the climate variable
#' and biological response from the best fitting model (lowest AIC) identified by run_slidingwin.
#' The plot includes the data points and the fitted regression line.
#'
#' @param dataset Output from run_slidingwin (a list with 'dataset' and 'bestModel' items)
#' @param x Character. Name of x variable to plot. If not provided, will be "climate".
#' @param n Integer. If weightwi
#' @param ... Additional arguments passed to 'predict' function to create model prediction line.
#'
#' @return A ggplot object showing the scatter plot with fitted line
#'
#' @examples
#' # Example usage:
#' data("MassClimate")
#' data("Mass")
#' results <- run_slidingwin(range = c(0, 100), 
#'                         climate_data = MassClimate, 
#'                         bio_data = Mass,
#'                         baseline = lm(Mass ~ climate, data = bio_data))
#' plot_best(results)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal
#' @importFrom rlang .data
#' @export
plot_best <- function(dataset, x, y, ...) {
  
  best_model <- getBestModel(dataset, ...)
  model_data <- getBestModelData(dataset, ...)
  
  ## If x and/or y are missing we pick them
  if (missing(x)){
    x <- "climate"
  }
  
  ## If no y is provided, we assume the first col is the response
  if (missing(y)){
    ## FIXME: Not great. Needs to be fixed
    y <- names(model_data)[1]  # First column is typically the response 
  }
  
  ## Create our model predicted line
  predict_data <- dplyr::tibble(!!as.symbol(x) := seq(min(model_data[[x]]),
                                                      max(model_data[[x]]),
                                                      length.out = 200))
  
  ## Check if there are other variables we can take the average...
  if (ncol(model_data) > 2){
    average_data <- model_data |> 
      summarise(across(.cols = !any_of(c(x, y)) & is.numeric, mean),
                across(.cols = !any_of(c(x, y)) & (is.character|is.factor), \(x) names(table(x))[which(table(x) == max(table(x)))][1]))
    predict_data <- dplyr::bind_cols(predict_data, average_data)
  }
  
  predict_data$y <- predict(best_model, newdata = predict_data, ...)
  
  # Create the scatter plot with fitted line
  p <- ggplot() +
    geom_point(data = model_data,
               aes(x = !!as.symbol(x), y = !!as.symbol(y)),
               color = "gray60", alpha = 0.7) +
    geom_line(data = predict_data,
              aes(x = !!as.symbol(x), y = y)) +
    # ggplot2::geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
    labs(
      x = "Climate variable",
      y = "Biological response",
      title = "Output of best model"
    ) +
    theme_climwin()
  
  return(p)
} 
