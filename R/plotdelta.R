#' Plot deltaAICc of models
#' 
#'Create a colour plot of model deltaAICc values.
#'@param dataset A dataframe containing information on all fitted climate 
#' windows. Output from \code{\link{slidingwin}}.
#'@param plotall Used in conjunction with function \code{\link{plotall}}. 
#' Should not be changed manually.
#'@param plotallenv Used in conjunction with function \code{\link{plotall}}.
#' Should not be changed manually.
#'@param arrow TRUE or FALSE. Add arrows to plots to pinpoint best window.
#'@return Returns a colour plot of model deltaAICc values (larger negative
#' values indicate stronger models). DeltaAICc is the difference between AICc
#' of each climate window and a null model.
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'# Plot deltaAICc estimates for climate windows in the Mass dataset
#' 
#'data(MassOutput)
#' 
#'plotdelta(dataset = MassOutput)
#'@import ggplot2
#'@export

plotdelta <- function(dataset, arrow = FALSE, plotall = FALSE, plotallenv){
  
with(dataset, {
  if(arrow == FALSE){
    ARR <-   ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = deltaAICc))+
      geom_tile(aes(fill = deltaAICc))+
      scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "")+
      theme_classic()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(size=0.25, colour = "black"),
            plot.title = element_text(size = 16),
            legend.position = c(0.75, 0.3),
            panel.border = element_rect(colour = "black", fill = NA))+
      ggtitle(expression(paste(Delta, "AICc (compared to null model)")))+
      ylab("Window open")+
      xlab("Window close")
    if(plotall == TRUE){
      plotallenv$delta <- ARR
    } else {
      ARR
    }
  } else if(arrow == TRUE){
    
    CIRC <- circle(centre = c(dataset$WindowClose[1], dataset$WindowOpen[1]), diameter = 5, npoints = 1000)
    colnames(CIRC) <- c("WindowClose", "WindowOpen")
    CIRC$deltaAICc <- 0
    
    ARR <-   ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = deltaAICc))+
      geom_tile(aes(fill = deltaAICc))+
      scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "")+
      theme_classic()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(size=0.25, colour = "black"),
            plot.title = element_text(size = 16),
            legend.position = c(0.75, 0.3),
            panel.border = element_rect(colour = "black", fill = NA))+
      ggtitle(expression(paste(Delta, "AICc (compared to null model)")))+
      ylab("Window open") +
      xlab("Window close") +
      geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
      geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                   size = 1, linetype = "dashed") +
      geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                   size = 1, linetype = "dashed")
    
    if(plotall == TRUE){
      plotallenv$delta <- ARR
    } else {
      ARR
    }
  }

    }
  )
}