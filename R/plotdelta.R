#' Plot deltaAICc of models
#' 
#' Create a colour plot of model deltaAICc values.
#' @param Dataset A dataframe containing information on all fitted climate 
#'   windows. Output from \code{\link{climatewin}}.
#' @return Returns a colour plot of model deltaAICc values (larger negative
#'   values indicate stronger models). DeltaAICc is the difference between AICc
#'   of each climate window and a null model.
#' @author Liam D. Bailey and Martijn van de Pol
#' @examples
#' # Plot deltaAICc estimates for climate windows in the Mass dataset
#' 
#' data(MassOutput)
#' 
#' plotdelta(Dataset = MassOutput)
#' @import ggplot2
#' @export




#LAST EDITED: 18/02/2015
#EDITED BY: LIAM
#NOTES: TIDY CODE

plotdelta <- function(Dataset){
  Dataset$delta  <- Dataset$ModelAICc - min(Dataset$ModelAICc)
  #Calculate a second delta compared to a model with no climate#
  Dataset$delta2 <- Dataset$ModelAICc - Dataset$baselineAICc
  
with(Dataset, {
  ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = delta2))+
    geom_tile(aes(fill = delta2))+
    scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "")+
    theme_classic()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size=0.25, colour = "black"),
          plot.title = element_text(size = 18),
          legend.position = c(0.75, 0.3))+
    ggtitle(expression(paste(Delta, "AICc (compared to null model)")))+
    ylab("Window open")+
    xlab("Window close")
    }
  )
}