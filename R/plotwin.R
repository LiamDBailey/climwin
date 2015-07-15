#'Plot the opening and closing point of best climate windows
#'
#'Visualise the opening and closing point for a subset of best climate windows.
#'@param Dataset A dataframe containing information on all fitted climate
#'  windows. Output from \code{\link{climatewin}}.
#'@param CW Cumulative model weight used to subset the group of best models
#'@return Creates two boxplots showing the opening and closing point for a subset
#'  of best climate windows. Best climate windows make up the
#'  cumulative model weight equivalent to the largest value of CW1, CW2 and CW3.
#'@author Liam D. Bailey and Martijn van de Pol
#' @examples
#' # View window limits for climate windows in the top 95% of model weights.
#' 
#' data(MassOutput)
#' 
#' plotwin(Dataset = MassOutput, CW = 0.95)
#' 
#'@import ggplot2
#'@import reshape
#'@export


#LAST EDITED: 19/02/2015
#EDITED BY: LIAM
#NOTES: TIDY CODE

plotwin <- function(Dataset, CW = 0.95){
  
  Dataset$weight <- (exp(-0.5 * Dataset$deltaAICc)) / sum(exp(-0.5 * Dataset$deltaAICc))
  #Order models by weight#
  Dataset    <- Dataset[order(-Dataset$weight), ]
  Dataset$CW <- as.numeric(cumsum(Dataset$weight) <= CW)
  DatasetCW  <- subset(Dataset, CW == 1)
  keep=c("closest", "WindowClose", "WindowOpen")
  DatasetCW                  <- DatasetCW[keep]
  DatasetCW                  <- melt(DatasetCW, id = "closest")
  DatasetCW$variable         <- factor(DatasetCW$variable, levels = c("WindowOpen", "WindowClose"))
  levels(DatasetCW$variable) <- c("Window Open", "Window Close")
  
  p_meds <- data.frame(variable = levels(DatasetCW$variable), value = as.numeric(tapply(DatasetCW$value, DatasetCW$variable, median)))
  
  with(DatasetCW, {
  ggplot(DatasetCW, aes(x = variable, y = value))+
         scale_y_continuous(limits = c(min(Dataset$WindowClose), max(Dataset$WindowClose)))+
         geom_boxplot(width = 0.5)+
         geom_text(data = p_meds, aes(x = variable, y = value, label = value),
                   size = 7, vjust = -2.2) +
         coord_flip()+
         theme_classic()+
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(size = 0.25, colour = "black"),
               axis.text.y = element_text(angle = 90, hjust = 0.5,size = 10),
               plot.title = element_text(size = 18))+
         ggtitle(paste("Climate window range for top ", (CW*100), "% of model weights"))+
         xlab("")+
         ylab("Climate window")
}
)
}