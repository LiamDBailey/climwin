#'Plot model beta estimates
#' 
#'Create colour plots of model beta estimates. Will include quadratic and cubic
#'beta estimates where appropriate.
#'@param Dataset A dataframe containing information on all fitted climate 
#' windows. Output from \code{\link{climatewin}}.
#'@param plotall Used in conjunction with function \code{\link{plotall}}. 
#' Should not be changed manually.
#'@param plotallenv Used in conjunction with function \code{\link{plotall}}.
#' Should not be changed manually.
#'@return Returns colour plots of model beta estimates. Where applicable, 2nd 
#' order coefficients (quadratic) and 3rd order coefficients (cubic) will be 
#' plotted seperately.
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'# Plot model beta estimates for linear models in the Mass dataset
#' 
#'data(MassOutput)
#'
#'plotbetas(Dataset = MassOutput)
#' 
#'@import ggplot2
#'@import gridExtra
#'@export


#LAST EDITED: 18/02/2015
#EDITED BY: LIAM
#NOTES: TIDY CODE

plotbetas <- function(Dataset, plotallenv, plotall = FALSE){
  
  with(Dataset, {
    if(Dataset$Function[1] == "lin" || Dataset$Function[1] == "log" || Dataset$Function[1] == "inv"){
      BETA <- ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBeta)) +
        geom_tile(aes(fill = ModelBeta)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_classic() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(size = 0.25, colour = "black"),
              plot.title = element_text(size = 18),
              legend.position = c(0.75,0.3)) +
        ggtitle("Beta linear") +
        ylab("Window open") +
        xlab("Window close")
      if(plotall == TRUE){
        plotallenv$BETA <- BETA
      } else {
        BETA
      }
    } else if(Dataset$Function[1] == "quad"){
      BETA <- ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBeta)) +
        geom_tile(aes(fill = ModelBeta)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_classic() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(size = 0.25, colour = "black"),
              plot.title = element_text(size = 18),
              legend.position = c(0.75,0.3)) +
        ggtitle("Beta linear") +
        ylab("Window open") +
        xlab("Window close")
      
      BETA2 <- ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBetaQ)) +
        geom_tile(aes(fill = ModelBetaQ)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_classic() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(size = 0.25, colour = "black"),
              plot.title = element_text(size = 18),
              legend.position = c(0.75, 0.3)) +
        ggtitle("Beta quadratic") +
        ylab("Window open") +
        xlab("Window close")
      if(plotall == TRUE){
        plotallenv$BETA  <- BETA
        plotallenv$BETA2 <- BETA2
      } else {
        grid.arrange(BETA, BETA2, nrow = 1)
      }
    } else if(Dataset$Function[1] == "cub"){
      BETA <- ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBeta)) +
        geom_tile(aes(fill = ModelBeta)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_classic() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(size = 0.25, colour = "black"),
              plot.title = element_text(size = 18),
              legend.position = c(0.75,0.3)) +
        ggtitle("Beta linear") +
        ylab("Window open") +
        xlab("Window close")
      
        BETA2 <- ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBetaQ)) +
          geom_tile(aes(fill = ModelBetaQ)) +
          scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                plot.title = element_text(size = 18),
                legend.position = c(0.75, 0.3)) +
          ggtitle("Beta quadratic") +
          ylab("Window open") +
          xlab("Window close")
        
        BETA3 <- ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = ModelBetaC)) +
          geom_tile(aes(fill = ModelBetaC)) +
          scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "")+
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                plot.title = element_text(size = 18),
                legend.position = c(0.75, 0.3)) +
          ggtitle("Beta cubic") +
          ylab("Window open") +
          xlab("Window close")
      if(plotall == TRUE){
        plotallenv$BETA  <- BETA
        plotallenv$BETA2 <- BETA2
        plotallenv$BETA3 <- BETA3
      } else {
        grid.arrange(BETA, BETA2, BETA3, nrow = 1)
      }      
    } else if(Dataset$Function[1] == "centre"){
      WGMEAN <- ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = WithinGrpMean)) +
        geom_tile(aes(fill = WithinGrpMean)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_classic() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(size = 0.25, colour = "black"),
              plot.title = element_text(size = 18),
              legend.position = c(0.75,0.3)) +
        ggtitle("Within group mean coefficient") +
        ylab("Window open") +
        xlab("Window close")
      
      WGDEV <- ggplot(Dataset, aes(x = WindowClose, y = WindowOpen, z = WithinGrpDev)) +
        geom_tile(aes(fill = WithinGrpDev)) +
        scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") +
        theme_classic() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(size = 0.25, colour = "black"),
              plot.title = element_text(size = 18),
              legend.position = c(0.75,0.3)) +
        ggtitle("Within group deviation coefficient") +
        ylab("Window open") +
        xlab("Window close")
      
      if(plotall == TRUE){
        plotallenv$WGMEAN <- WGMEAN
        plotallenv$WGDEV  <- WGDEV
      } else {
        grid.arrange(WGMEAN, WGDEV, nrow = 1)
      }
    }
  }
  )
}