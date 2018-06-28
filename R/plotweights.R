#'Plot distribution of model weights
#'
#'Create a plot showing the distribution of cumulative model weights for
#'all fitted climate windows.
#'@param dataset A dataframe containing information on all fitted climate 
#'  windows. Output from \code{\link{slidingwin}}.
#'@param cw1,cw2,cw3 Cumulative weight levels used to visualise model weight 
#'  distribution. Cumulative weights represent the chance that the best model is
#'  contained within a set. For example, there is a 95 percent chance that the best
#'  climate window model is contained within the cumulative weight level of
#'  0.95. Parameter values must <= 1.
#'@param arrow TRUE or FALSE. Add arrows to plots to pinpoint best window.
#'@param ThreeD TRUE or FALSE. Generate a 3-dimensional plot of the model weight landscape.
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

plotweights <- function(dataset, cw1 = 0.95, cw2 = 0.5, cw3 = 0.25, arrow = FALSE, plotall = FALSE, plotallenv, ThreeD = FALSE){

  #Order cw1, cw2 and cw3 so that cw1 is always the largest value
  a          <- c(cw1, cw2, cw3)
  b          <- a[order (-a)]
  cw         <- cw1
  cw1        <- b[1]
  cw2        <- b[2]
  cw3        <- b[3]
  
  #Determine the % of models that fall within the 95% cumulative set (i.e. Spread or C)
  WeightDist <- ceiling(100*mean(as.numeric(cumsum(dataset$ModWeight) <= cw1)))
  
  #Create a subset of the models that fall within the 95% confidence set
  ConfidenceSet <- dataset[which(cumsum(dataset$ModWeight) <= cw1), ]
  
  #If the best model is above cw1 just include the best model
  if(nrow(ConfidenceSet) == 0){
    
    ConfidenceSet <- dataset[1, ]
    
  }
    
  #This measure of spread is not currently used. It causes problems with small powerful windows. Remove it for now.
  
  #Create an empty matrix equivalent length to the confidence set
  #SpreadMatrix <- matrix(nrow = (nrow(ConfidenceSet)- 1), ncol = 2)
  
  #Determine Euclidan distances between each model and the best model
  #for(i in 2:nrow(ConfidenceSet)){
  #  SpreadMatrix[i - 1, 1] <- i
  #  SpreadMatrix[i - 1, 2] <- sqrt((ConfidenceSet$WindowOpen[1] - ConfidenceSet$WindowOpen[i])^2 + 
  #                             (ConfidenceSet$WindowClose[1] - ConfidenceSet$WindowClose[i])^2)
  #}
  
  #Determine the maximum Euclidian distance
  #WeightSpread <- ceiling(max(SpreadMatrix[, 2]))
  #N.B. This metric is currently not used in our code
  
  #Order models by weight#
  dataset        <- dataset[order(-dataset$ModWeight), ]
  dataset$cw1    <- as.numeric(cumsum(dataset$ModWeight) <= cw1)
  dataset$cw2    <- as.numeric(cumsum(dataset$ModWeight) <= cw2)
  dataset$cw3    <- as.numeric(cumsum(dataset$ModWeight) <= cw3)
  
  #If there is no ONE model that fits in one of the confidence sets (e.g. the top model is > 0.25) we currently will not plot that point. This is a bit misleading. The top model does occur in the 25% confidence set (i.e. we can be at least 25% confident that this is the best model). Therefore, if there is a scenario where the top model not included in a set, we make the best model part of that set.
  
  if(all(dataset$cw3 == 0)){
    
    dataset$cw3[1] <- 1
    
  }
  
  if(all(dataset$cw2 == 0)){
    
    dataset$cw2[1] <- 1
    
  }
  
  if(all(dataset$cw1 == 0)){
    
    dataset$cw1[1] <- 1
    
  }
  
  dataset$cw.full <- dataset$cw1 + dataset$cw2 + dataset$cw3
  
  if(ThreeD == TRUE){
    
    stop("3D plotting temporarily disabled due to issues with the rgl package.")
    
    #Matrix_3d <- matrix(nrow = max(dataset$WindowOpen), ncol = max(dataset$WindowOpen), data = 0)
    #Matrix_cw <- matrix(nrow = max(dataset$WindowOpen), ncol = max(dataset$WindowOpen), data = 0)
    #for(i in 1:nrow(dataset)){
      
    #  Matrix_3d[dataset$WindowOpen[i], dataset$WindowClose[i]] <- dataset$ModWeight[i]
      
    #}
    
    #for(i in 1:nrow(dataset)){
      
    #  Matrix_cw[dataset$WindowOpen[i], dataset$WindowClose[i]] <- dataset$cw.full[i] + 1
      
    #}
    
    #norm_palette <- colorRampPalette(c("white", "grey", "black"))
    
    #cw <- Matrix_cw + 1
    #z <- Matrix_3d;
    #x <- (1:nrow(z));
    #y <- (1:nrow(z));
    #zlim <- range(z);
    #z2 <- ((z/zlim[2])*100)
    #colourlut <- norm_palette(5);
    #col <- colourlut[cw];
    #open3d();
    #rgl.surface(x, y, z2, color = col, alpha = 1, back = "lines");
    #rgl.surface(x, y, matrix(0, nrow(z), ncol(z)), color = "grey", alpha = 0.5, back = "fill")
    
  } else {
  
  dataset$cw.full[which(dataset$cw.full == 3)] <- cw3
  dataset$cw.full[which(dataset$cw.full == 2)] <- cw2
  dataset$cw.full[which(dataset$cw.full == 1)] <- cw1
  dataset$cw.full[which(dataset$cw.full == 0)] <- 1

with(dataset, {
  if(arrow == FALSE){
    ARR <- ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = cw.full))+
      geom_tile(aes(fill = cw.full))+
      scale_fill_gradientn(colours = c("black", "white"), breaks=c(b[1], b[2], b[3]), limits = c(0, 1), name = "")+
      theme_climwin()+
      theme(legend.position = c(0.75, 0.3))+
      ggtitle(paste(WeightDist, "% of models fall within the \n", 100*cw, "% confidence set", sep = ""))+
      ylab("Window open")+
      xlab("Window close")
    
    if(plotall == TRUE){
      plotallenv$cw <- ARR
    } else {
      ARR
    }
    
  } else if(arrow == TRUE){
    
    CIRC <- circle(centre = c(dataset$WindowClose[1], dataset$WindowOpen[1]), diameter = 5, npoints = 100)
    colnames(CIRC) <- c("WindowClose", "WindowOpen")
    CIRC$cw.full <- 0
    
    ARR <- ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = cw.full))+
      geom_tile(aes(fill = cw.full))+
      scale_fill_gradientn(colours = c("black", "white"), breaks=c(b[1], b[2], b[3]), limits = c(0, 1), name = "")+
      theme_climwin()+
      theme(legend.position = c(0.75, 0.3))+
      ggtitle(paste(WeightDist, "% of models fall within the \n", 100*cw, "% confidence set", sep = ""))+
      ylab("Window open")+
      xlab("Window close")+
      geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
      geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                   size = 1, linetype = "dashed") +
      geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                   size = 1, linetype = "dashed")
    
    if(plotall == TRUE){
      plotallenv$cw <- ARR
    } else {
      ARR
    }
    
  }

}
)
  }
}