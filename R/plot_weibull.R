#' Plot Weibull Distribution Weights
#'
#' This function creates a line plot visualization of the Weibull distribution weights
#' used to create the weighted climate variable in the best model. The plot shows how
#' the weights are distributed across the time range, with Day on the x-axis and 
#' weight values on the y-axis.
#'
#' @param dataset Output from run_weightwin containing weights information in the @weights slot
#'
#' @return A ggplot object showing the Weibull distribution weights
#'
#' @examples
#' # Example usage:
#' Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
#' Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
#' results <- run_weightwin(range = 0:100, 
#'                         bio_data = Mass,
#'                         climate_data = Climate,
#'                         cdate = "Date", bdate = "Date",
#'                         xvar = "Temp",
#'                         basemodel = lm(Mass ~ climate, data = bio_data),
#'                         par = c(1.25, 0.5))
#' plot_weibull(results)
#'
#' @export
#' @import ggplot2
plot_weibull <- function(dataset){
  
  weights <- getWeights(dataset)
  
  p <- ggplot() +
    geom_line(aes(x = seq(min(dataset@range),
                          max(dataset@range), 1),
                  y = weights)) +
    labs(title = "Weibull distribution used to create weighted climate mean",
         x = "Day") +
    coord_cartesian() + 
    scale_y_continuous(expand = c(0, 0.001), limits = c(0, NA)) +
    theme_climwin() +
    theme(legend.position = c(0.85, 0.325),
          legend.title = element_blank(),
          legend.text.position = "left",
          legend.text = element_text(size = rel(3)),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  return(p)
  
}