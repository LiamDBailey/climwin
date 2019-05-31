#'Determine the median start and end time for climate windows
#'
#'Determine the median start and end time for climate windows within
#'a chosen confidence set.
#'@param dataset Output dataframe of function \code{\link{slidingwin}}.
#'@param cw Cut-off for confidence set (0.95 by default)

#'@return Returns two values representing the median start and end
#'time of climate windows within the confidence set. 
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'# Determine median start and end time of MassOutput from the 95% confidence set
#'
#'medwin(MassOutput, cw = 0.95)
#'
#'@export

medwin <- function(dataset, cw = 0.95){
  
  #Order models by weight#
  dataset    <- dataset[order(-dataset$ModWeight), ]
  dataset$cw <- as.numeric(cumsum(dataset$ModWeight) <= cw)
  datasetcw  <- subset(dataset, cw == 1)
  
  keep=c("Closest", "WindowClose", "WindowOpen")
  
  datasetcw                  <- datasetcw[keep]
  datasetcw                  <- melt(datasetcw, id = "Closest")
  datasetcw$variable         <- factor(datasetcw$variable, levels = c("WindowOpen", "WindowClose"))
  levels(datasetcw$variable) <- c("Window Open", "Window Close")
  
  wo <- datasetcw[which(datasetcw$variable == "Window Open"), ]
  wc <- datasetcw[which(datasetcw$variable == "Window Close"), ]
  
  return(list("Median Window Open" = median(wo$value), "Median Window Close" = median(wc$value)))
}