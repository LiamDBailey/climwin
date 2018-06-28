#'Plot model beta estimates
#' 
#'Create colour plots of model beta estimates. Will include quadratic and cubic
#'beta estimates where appropriate.
#'@param dataset A dataframe containing information on all fitted climate 
#' windows. Output from \code{\link{slidingwin}}.
#'@param plotall Used in conjunction with function \code{\link{plotall}}. 
#' Should not be changed manually.
#'@param plotallenv Used in conjunction with function \code{\link{plotall}}.
#' Should not be changed manually.
#'@param arrow TRUE or FALSE. Add arrows to plots to pinpoint best window.
#'@return Returns colour plots of model beta estimates. Where applicable, 2nd 
#' order coefficients (quadratic) and 3rd order coefficients (cubic) will be 
#' plotted separately.
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'# Plot model beta estimates for linear models in the Mass dataset
#' 
#'data(MassOutput)
#'
#'plotbetas(dataset = MassOutput)
#' 
#'@import ggplot2
#'@import gridExtra
#'@export

plotbetas <- function(dataset, arrow = FALSE, plotallenv, plotall = FALSE){
  
  with(dataset, {
    if(dataset$Function[1] == "lin" || dataset$Function[1] == "log" || dataset$Function[1] == "inv"){
      beta <-ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBeta)) +
        geom_tile(aes(fill = ModelBeta)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_climwin() +
        theme(legend.position = c(0.75,0.3)) +
        ggtitle("Beta linear") +
        ylab("Window open") +
        xlab("Window close")
      
      if(arrow == TRUE){
        
        CIRC <- circle(centre = c(dataset$WindowClose[1], dataset$WindowOpen[1]), diameter = 5, npoints = 1000)
        colnames(CIRC) <- c("WindowClose", "WindowOpen")
        CIRC$ModelBeta <- 0
        
        beta <- beta +
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                       size = 1, linetype = "dashed")
      }
      
      if(plotall == TRUE){
        plotallenv$beta <-beta
      } else {
        beta
      }
    } else if(dataset$Function[1] == "quad"){
      beta <-ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBeta)) +
        geom_tile(aes(fill = ModelBeta)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_climwin() +
        theme(legend.position = c(0.75,0.3)) +
        ggtitle("Beta linear") +
        ylab("Window open") +
        xlab("Window close")
      
      beta2 <- ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBetaQ)) +
        geom_tile(aes(fill = ModelBetaQ)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_climwin() +
        theme(legend.position = c(0.75, 0.3)) +
        ggtitle("Beta quadratic") +
        ylab("Window open") +
        xlab("Window close")
      
      if(arrow == TRUE){
        
        CIRC <- circle(centre = c(dataset$WindowClose[1], dataset$WindowOpen[1]), diameter = 5, npoints = 1000)
        colnames(CIRC) <- c("WindowClose", "WindowOpen")
        CIRC$ModelBeta <- 0
        CIRC$ModelBetaQ <- 0
        
        beta <- beta +
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                       size = 1, linetype = "dashed")        
        beta2 <- beta2 +
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                       size = 1, linetype = "dashed")        
      }
      
      if(plotall == TRUE){
        plotallenv$beta  <- beta
        plotallenv$beta2 <- beta2
      } else {
        grid.arrange(beta, beta2, nrow = 1)
      }
    } else if(dataset$Function[1] == "cub"){
      beta <-ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBeta)) +
        geom_tile(aes(fill = ModelBeta)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_climwin() +
        theme(legend.position = c(0.75,0.3)) +
        ggtitle("Beta linear") +
        ylab("Window open") +
        xlab("Window close")
      
        beta2 <- ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBetaQ)) +
          geom_tile(aes(fill = ModelBetaQ)) +
          scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
          theme_climwin() +
          theme(legend.position = c(0.75, 0.3)) +
          ggtitle("Beta quadratic") +
          ylab("Window open") +
          xlab("Window close")
        
        beta3 <- ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBetaC)) +
          geom_tile(aes(fill = ModelBetaC)) +
          scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "")+
          theme_climwin() +
          theme(legend.position = c(0.75, 0.3)) +
          ggtitle("Beta cubic") +
          ylab("Window open") +
          xlab("Window close")
      
      if(arrow == TRUE){
        
        CIRC <- circle(centre = c(dataset$WindowClose[1], dataset$WindowOpen[1]), diameter = 5, npoints = 1000)
        colnames(CIRC) <- c("WindowClose", "WindowOpen")
        CIRC$ModelBeta <- 0
        CIRC$ModelBetaQ <- 0
        CIRC$ModelBetaC <- 0
                
        beta <- beta +
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                       size = 1, linetype = "dashed")        
        beta2 <- beta2 +
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                       size = 1, linetype = "dashed")        
        beta3 <- beta3 +
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                       size = 1, linetype = "dashed")        
      }      
      
      if(plotall == TRUE){
        plotallenv$beta  <- beta
        plotallenv$beta2 <- beta2
        plotallenv$beta3 <- beta3
      } else {
        grid.arrange(beta, beta2, beta3, nrow = 1)
      }      
    } else if(dataset$Function[1] == "centre"){
      wgmean <- ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = WithinGrpMean)) +
        geom_tile(aes(fill = WithinGrpMean)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_climwin() +
        theme(legend.position = c(0.75,0.3)) +
        ggtitle("Within group mean coefficient") +
        ylab("Window open") +
        xlab("Window close")
      
      wgdev <- ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = WithinGrpDev)) +
        geom_tile(aes(fill = WithinGrpDev)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_climwin() +
        theme(legend.position = c(0.75,0.3)) +
        ggtitle("Within group deviation coefficient") +
        ylab("Window open") +
        xlab("Window close")
      
      if(arrow == TRUE){
        
        CIRC <- circle(centre = c(dataset$WindowClose[1], dataset$WindowOpen[1]), diameter = 5, npoints = 1000)
        colnames(CIRC) <- c("WindowClose", "WindowOpen")
        CIRC$WithinGrpMean <- 0
        CIRC$WithinGrpDev <- 0
        
        wgmean <- wgmean +
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                       size = 1, linetype = "dashed")        
        wgdev <- wgdev +
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                       size = 1, linetype = "dashed")              
      }
      
      if(is.null(dataset$WithinGrpDev) == FALSE & is.null(dataset$WithinGrpMean) == FALSE){
        
        if(plotall == TRUE){
          plotallenv$wgmean <- wgmean
          plotallenv$wgdev  <- wgdev
        } else {
          grid.arrange(wgmean, wgdev, nrow = 1)
        }
        
      } else if(is.null(dataset$WithinGrpDev) == TRUE){
        
        if(plotall == TRUE){
          plotallenv$wgmean <- wgmean
        } else {
          grid.arrange(wgmean, nrow = 1)
        }
        
      } else  if(is.null(dataset$WithinGrpMean) == TRUE){
        
        if(plotall == TRUE){
          plotallenv$wgdev  <- wgdev
        } else {
          grid.arrange(wgdev, nrow = 1)
        }
        
      }
      

    }
  }
  )
}