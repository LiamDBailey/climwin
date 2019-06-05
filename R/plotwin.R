#'Plot the start and end time of best climate windows
#'
#'Visualise the start and end time for a subset of best climate windows.
#'@param dataset A dataframe containing information on all fitted climate
#'  windows. Output from \code{\link{slidingwin}}.
#'@param cw Cumulative model weight used to subset the group of best models.
#'@return Creates two boxplots showing the start and end time for a subset
#'  of best climate windows. Best climate windows make up the
#'  cumulative model weight equivalent to the value of cw.
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'# View window limits for climate windows in the top 95% of model weights.
#' 
#'data(MassOutput)
#' 
#'plotwin(dataset = MassOutput, cw = 0.95)
#' 
#'@import ggplot2
#'@import reshape
#'@export

plotwin <- function(dataset, cw = 0.95){
  
  #Order models by weight#
  dataset    <- dataset[order(-dataset$ModWeight), ]
  
  #Firstly, check if the top model has a weight > cw. If so, just use the top model.
  if(dataset$ModWeight[1] > cw){
    
    datasetcw <- dataset[1, ]
    
    warning(paste0("Top window has a weight greater than ", cw, ". Plotting single best window only."))
    
  } else {
    
    dataset$cw <- as.numeric(cumsum(dataset$ModWeight) <= cw)
    datasetcw  <- subset(dataset, cw == 1) 
    
  }
  
  keep=c("Closest", "WindowClose", "WindowOpen")
  
  datasetcw                  <- datasetcw[keep]
  datasetcw                  <- melt(datasetcw, id = "Closest")
  datasetcw$variable         <- factor(datasetcw$variable, levels = c("WindowOpen", "WindowClose"))
  levels(datasetcw$variable) <- c("Window Open", "Window Close")
  
  p_meds <- data.frame(variable = levels(datasetcw$variable), value = as.numeric(tapply(datasetcw$value, datasetcw$variable, median)))
  
  with(datasetcw, {
    ggplot(datasetcw, aes(x = variable, y = value))+
      scale_y_continuous(limits = c(min(dataset$WindowClose), max(dataset$WindowClose)))+
      geom_boxplot(width = 0.5)+
      geom_text(data = p_meds, aes(x = variable, y = value, label = value),
                size = 5, vjust = -1.9) +
      coord_flip()+
      theme_climwin()+
      theme(axis.text.y = element_text(angle = 90, hjust = 0.5,size = 10))+
      ggtitle(paste("Climate window range for \n", (cw*100), "% confidence set"))+
      xlab("")+
      ylab("Climate window")
  })
}