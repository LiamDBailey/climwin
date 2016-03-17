#'Plot distribution of model weights
#'
#'Create a plot showing the distribution of cumulative model weights for
#'all fitted climate windows.
#'@param dataset A dataframe containing information on all fitted climate 
#'  windows. Output from \code{\link{climatewin}}.
#'@param cw1,cw2,cw3 Cumulative weight levels used to visualise model weight 
#'  distribution. Cumulative weights represent the chance that the best model is
#'  contained within a set. For example, there is a 95 percent chance that the best
#'  climate window model is contained within the cumulative weight level of
#'  0.95. Parameter values must <= 1.
#'@param arrow TRUE or FALSE. Add arrows to plots to pinpoint best window.
#'@param plotall Used in conjunction with function \code{\link{plotall}}. 
#' Should not be changed manually.
#'@param plotallenv Used in conjunction with function \code{\link{plotall}}.
#' Should not be changed manually.

#'@return Returns a plot showing the distribution of cumulative model
#'  weights. Levels determined by parameters cw1,cw2 and cw3.
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'# Plot distribution of model weights for Mass dataset
#' 
#'data(MassOutput)
#' 
#'plotweights(dataset = MassOutput, cw1 = 0.95, cw2 = 0.75, cw3 = 0.25)
#'@import ggplot2
#'@export

plotweights <- function(dataset, cw1 = 0.95, cw2 = 0.5, cw3 = 0.25, arrow = FALSE, plotall = FALSE, plotallenv){

  a          <- c(cw1, cw2, cw3)
  b          <- a[order (-a)]
  cw         <- cw1
  cw1        <- b[1]
  cw2        <- b[2]
  cw3        <- b[3]
  WeightDist <- ceiling(100*mean(as.numeric(cumsum(dataset$ModWeight) <= cw1)))
  
  ConfidenceSet <- dataset[which(cumsum(dataset$ModWeight) <= cw1), ]
  SpreadMatrix <- matrix(nrow = (nrow(ConfidenceSet)- 1), ncol = 2)
  for(i in 2:nrow(ConfidenceSet)){
    SpreadMatrix[i - 1, 1] <- i
    SpreadMatrix[i - 1, 2] <- sqrt((ConfidenceSet$WindowOpen[1] - ConfidenceSet$WindowOpen[i])^2 + 
                               (ConfidenceSet$WindowClose[1] - ConfidenceSet$WindowClose[i])^2)
  }
  WeightSpread <- ceiling(max(Spread[, 2]))
  
  #Order models by weight#
  dataset        <- dataset[order(-dataset$ModWeight), ]
  dataset$cw1    <- as.numeric(cumsum(dataset$ModWeight) <= cw1)
  dataset$cw2    <- as.numeric(cumsum(dataset$ModWeight) <= cw2)
  dataset$cw3    <- as.numeric(cumsum(dataset$ModWeight) <= cw3)
  dataset$cw.full <- dataset$cw1 + dataset$cw2 + dataset$cw3
  
  dataset$cw.full[which(dataset$cw.full == 3)] <- cw3
  dataset$cw.full[which(dataset$cw.full == 2)] <- cw2
  dataset$cw.full[which(dataset$cw.full == 1)] <- cw1
  dataset$cw.full[which(dataset$cw.full == 0)] <- 1
  
with(dataset, {
  if(arrow == FALSE){
    ARR <- ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = cw.full))+
      geom_tile(aes(fill = cw.full))+
      scale_fill_gradientn(colours = c("black", "white"), breaks=c(b[1], b[2], b[3]), limits = c(0, 1), name = "")+
      theme_classic()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(size = 0.25, colour = "black"),
            plot.title = element_text(size = 16),
            legend.position = c(0.75, 0.3),
            panel.border = element_rect(colour = "black", fill = NA))+
      ggtitle(paste(100*cw, "% confidence set\n", "Model number:", WeightDist, "% of total models \n", 
                    "Model spread:", WeightSpread))+
      ylab("Window open")+
      xlab("Window close")
    
    if(plotall == TRUE){
      plotallenv$cw <- ARR
    } else {
      ARR
    }
    
  } else if(arrow == TRUE){
    
    ARR <- ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = cw.full))+
      geom_tile(aes(fill = cw.full))+
      scale_fill_gradientn(colours = c("black", "white"), breaks=c(b[1], b[2], b[3]), limits = c(0, 1), name = "")+
      theme_classic()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(size = 0.25, colour = "black"),
            plot.title = element_text(size = 16),
            legend.position = c(0.75, 0.3),
            panel.border = element_rect(colour = "black", fill = NA))+
      ggtitle(paste(100*cw, "% confidence set\n", "Model number:", WeightDist, "% of total models \n", 
                    "Model spread:", WeightSpread))+
      ylab("Window open")+
      xlab("Window close")+
      geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1]-1)), 
                   size = 1, arrow = grid::arrow(length = grid::unit(0.25, "cm"))) +
      geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1]-1), yend = WindowOpen[1]),
                   size = 1, arrow = grid::arrow(length = grid::unit(0.25, "cm")))
    
    if(plotall == TRUE){
      plotallenv$cw <- ARR
    } else {
      ARR
    }
    
  }

}
)  
}