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
#'@param ThreeD TRUE or FALSE. Generate a 3-dimensional plot of the deltaAICc landscape.
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
#'@importFrom grDevices colorRampPalette
#'@export

plotdelta <- function(dataset, arrow = FALSE, plotall = FALSE, plotallenv, ThreeD = FALSE){
  
  if(ThreeD == TRUE){
    
    stop("3D plotting temporarily disabled due to issues with the rgl package.")
    
    #Matrix_3d <- matrix(nrow = max(dataset$WindowOpen), ncol = max(dataset$WindowOpen), data = 0)
    #for(i in 1:nrow(dataset)){
    
    #  Matrix_3d[dataset$WindowOpen[i], dataset$WindowClose[i]] <- dataset$deltaAICc[i]
    
    #}
    
    #norm_palette <- colorRampPalette(c("blue", "yellow", "red"))
    
    #z <- -(Matrix_3d);
    #x <- (1:nrow(z));
    #y <- (1:nrow(z));
    #zlim <- range(z);
    #zlen <- zlim[2] - zlim[1]+1;
    #colourlut <- norm_palette(zlen);
    #col <- colourlut[z-zlim[1]+1];
    #open3d();
    #rgl.surface(x, y, z, color = col, alpha = 1, back = "lines");
    #rgl.surface(x, y, matrix(1, nrow(z), ncol(z)), color = "grey", alpha = 0.5, back = "fill")
    
  } else {
    
with(dataset, {
  if(arrow == FALSE){
    ARR <-   ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = deltaAICc))+
      geom_tile(aes(fill = deltaAICc))+
      scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "")+
      theme_climwin()+
      theme(legend.position = c(0.75, 0.3))+
      ggtitle(expression(paste(Delta, "AICc (compared to null model)")))+
      ylab("Window open")+
      xlab("Window close")
    if(plotall == TRUE){
      plotallenv$delta <- ARR
    } else {
      ARR
    }
  } else if(arrow == TRUE){
    
    CIRC <- circle(centre = c(dataset$WindowClose[1], dataset$WindowOpen[1]), diameter = 5, npoints = 100)
    colnames(CIRC) <- c("WindowClose", "WindowOpen")
    CIRC$deltaAICc <- 0
    
    ARR <-   ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = deltaAICc))+
      geom_tile(aes(fill = deltaAICc))+
      scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "")+
      theme_climwin()+
      theme(legend.position = c(0.75, 0.3))+
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
}