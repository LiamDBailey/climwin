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
  
  WO <- subset(DatasetCW, variable == "Window Open")
  WC <- subset(DatasetCW, variable == "Window Close")
  
  return(list("Median Window Open" = median(WO$value), "Median Window Close" = median(WC$value)))
}