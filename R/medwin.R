#' Return the value of the median climate window
#' 
#' Returns the median values for window open and window close
#' from all climate windows that fall within specified model weights.
#' 
#' @param Dataset Output from function \code{\link{climatewin}} with information on all fitted climate windows.
#' @param CW The model weight cut-off that determines which climate windows are used to calculate the median values.
#' @return Returns a list item containing median values of window open and window close
#' @author Liam D. Bailey and Martijn van de Pol
#' @examples
#' 
#' # Find median window values for climate windows fitted to the Mass dataset
#' 
#' data(MassOutput)
#' 
#' medwin(Dataset = MassOutput, CW = 0.95)
#' 
#' @export

medwin <- function(Dataset, CW = 0.95){
  
  Dataset$delta  <- Dataset$ModelAICc - min(Dataset$ModelAICc)
  Dataset$weight <- (exp(-0.5 * Dataset$delta)) / sum(exp(-0.5 * Dataset$delta))
  #Order models by weight#
  Dataset    <- Dataset[order(-Dataset$weight), ]
  Dataset$CW <- as.numeric(cumsum(Dataset$weight) <= CW)
  DatasetCW  <- subset(Dataset, CW == 1)
  keep=c("closest", "WindowClose", "WindowOpen")
  DatasetCW                  <- DatasetCW[keep]
  DatasetCW                  <- melt(DatasetCW, id = "closest")
  DatasetCW$variable         <- factor(DatasetCW$variable, levels = c("WindowOpen", "WindowClose"))
  levels(DatasetCW$variable) <- c("Window Open", "Window Close")
  
  WO <- DatasetCW[which(DatasetCW$variable == "Window Open"), ]
  WC <- DatasetCW[which(DatasetCW$variable == "Window Close"), ]
  
  return(list("Median Window Open" = median(WO$value), "Median Window Close" = median(WC$value)))
}