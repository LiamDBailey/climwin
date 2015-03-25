#'Plot distribution of model weights
#'
#'Create a colour plot showing the distribution of cumulative model weights for
#'all fitted climate windows.
#'@param Dataset A dataframe containing information on all fitted climate 
#'  windows. Output from \code{\link{climatewin}}.
#'@param CW1,CW2,CW3 Cumulative weight levels used to visualise model weight 
#'  distribution. Cumulative weights represent the chance that the best model is
#'  contained within a set. For example, there is a 95 percent chance that the best
#'  climate window model is contained within the cumulative weight level of
#'  0.95. Parameter values must <= 1.
#'@return Returns a colour plot showing the distribution of cumulative model
#'  weights. Colour levels determined by parameters CW1,CW2 and CW3.
#'@author Liam D. Bailey and Martijn van de Pol
#' @examples
#' # Plot distribution of model weights for Mass dataset
#' 
#' data(MassOutput)
#' 
#' plotweights(Dataset = MassOutput, CW1 = 0.95, CW2 = 0.75, CW3 = 0.25)
#'@import ggplot2
#'@export


# last edited 19/2/15 by LDB
#TIDY CODE

plotweights <- function(Dataset, CW1=0.95, CW2=0.5, CW3=0.25){
  a   <- c(CW1, CW2, CW3)
  b   <- a[order (-a)]
  CW1 <- b[1]
  CW2 <- b[2]
  CW3 <- b[3]
  Dataset$delta  <- Dataset$ModelAICc - min(Dataset$ModelAICc)
  Dataset$weight <- (exp(-0.5 * Dataset$delta)) / sum(exp(-0.5 * Dataset$delta))
  #Order models by weight#
  Dataset        <- Dataset[order(-Dataset$weight), ]
  Dataset$CW1    <- as.numeric(cumsum(Dataset$weight) <= CW1)
  Dataset$CW2    <- as.numeric(cumsum(Dataset$weight) <= CW2)
  Dataset$CW3    <- as.numeric(cumsum(Dataset$weight) <= CW3)
  Dataset$CWFULL <- Dataset$CW1 + Dataset$CW2 + Dataset$CW3
  
  Dataset$CWFULL[which(Dataset$CWFULL == 3)] <- CW3
  Dataset$CWFULL[which(Dataset$CWFULL == 2)] <- CW2
  Dataset$CWFULL[which(Dataset$CWFULL == 1)] <- CW1
  Dataset$CWFULL[which(Dataset$CWFULL == 0)] <- 1
  
with(Dataset, {
  ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = CWFULL))+
    geom_tile(aes(fill = CWFULL))+
    scale_fill_gradientn(colours = c("black", "white"), breaks=c(b[1], b[2], b[3]), limits = c(0, 1), name = "")+
    theme_classic()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size = 0.25, colour = "black"),
          plot.title = element_text(size = 18),
          legend.position = c(0.75, 0.3))+
    ggtitle("Cumulative model weight")+
    ylab("Window open")+
    xlab("Window close")
}
)  
}