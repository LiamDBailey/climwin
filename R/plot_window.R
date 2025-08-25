#' Plot Climate Window Range Distribution
#'
#' This function creates stacked boxplots showing the distribution of Start_Day and End_Day
#' for all climate windows that fall within a specified cumulative weight threshold.
#'
#' @param dataset Output from run_slidingwin (either a data frame or a list with 'dataset' item) containing columns:
#'                Start_Day, End_Day, AIC, and ModWeight
#' @param cw1 A numeric value between 0 and 1 defining the cumulative weight threshold. Defaults to 0.95.
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
#' @export
#' @import ggplot2
#' @import dplyr
plot_window <- function(dataset, cw1 = 0.95, method = "box", ...) {
  
  # Handle new list structure from run_slidingwin
  if (is.list(dataset) && "dataset" %in% names(dataset)) {
    dataset <- dataset$dataset
  }
  
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
  
  label_data <- plot_data |> 
    dplyr::group_by(Window_Type) |> 
    dplyr::summarise(median = median(Climate_Window))
  
  # Create the boxplot
  p <- ggplot()
  
  if (method == "box"){
    p <- p +
      geom_boxplot(data = plot_data,
                   aes(x = Window_Type, y = Climate_Window)) +
      geom_text(data = label_data,
                aes(x = Window_Type, y = median, label = median),
                colour = "black", hjust = -0.5)
  } else if (method == "violin"){
    p <- p +
      geom_violin(data = plot_data,
                  aes(x = Window_Type, y = Climate_Window),
                  fill = "grey80", alpha = 0.75) +
      geom_point(data = label_data,
                   aes(x = Window_Type,
                       y = median)) +
      geom_text(data = label_data,
                aes(x = Window_Type, y = median, label = median),
                colour = "black", hjust = 0.5, vjust = -0.5)
  }
     
  p <- p +
    scale_y_continuous(limits = range(dataset$Start_Day), expand = c(0, 0)) +
    coord_flip() +
    ggplot2::labs(
      x = "",
      y = "Climate window",
      title = paste0("Climate window range for\n", round(cw1 * 100), " % confidence set")
    ) +
    theme_climwin() +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
  
  return(p)
} 