#'Create a histogram of randomised deltaAICc values
#'
#'Create a histogram of deltaAICc values from randomised data.
#'@param dataset A dataframe containing information on all fitted climate
#'  windows from observed data. Output from \code{\link{slidingwin}}.
#'@param datasetrand A dataframe containing information on all fitted climate 
#'  windows using randomised data. Output from \code{\link{randwin}}.
#'@return plothist will return a histograms of deltaAICc values from 
#'  randomised data. Values of PdeltaAICc and Pc will be provided to help 
#'  determine the likelihood that an observed deltaAICc value would occur
#'  by chance. 
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'# Plot randomised data for the Mass dataset
#' 
#'data(MassOutput)
#'data(MassRand)
#' 
#'plothist(datasetrand = MassRand)
#' 
#'@import ggplot2
#'@export

plothist <- function(dataset, datasetrand){
  
  if(is.null(datasetrand) == TRUE){
    stop("Please provide randomised data")
  }  
  
if(max(datasetrand$Repeat) <= 100){
      print("PDeltaAICc may be unreliable with so few randomisations")
    }
          
    if(is.null(dataset$shape) == TRUE){
      
      P  <- round(pvalue(rand.dataset = datasetrand, full.dataset = dataset, metric = "Spread", sample.size = dataset$sample.size[1]), digits = 3)
      P2 <- round(pvalue(rand.dataset = datasetrand, full.dataset = dataset, metric = "AIC", sample.size = dataset$sample.size[1]), digits = 3)
      if(P2 < 0.01){
        P2 = as.character("<0.01")
      }
      if(P < 0.01){
        P = as.character("<0.01")
      }
      
      with(datasetrand, {ggplot(datasetrand, aes(x = deltaAICc, fill = Randomised))+
                        geom_histogram(aes(y = 2 * ..density..), colour = "black", binwidth = 2, alpha = 0.5)+
                        theme_classic()+
                        theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.line = element_line(size = 0.25, colour = "black"),
                              legend.position = "none",
                              plot.title = element_text(size = 16))+
                        geom_vline(aes(xintercept = dataset$deltaAICc[1]), linetype = "dashed", size = 1.5)+
                        ggtitle(bquote(atop(Histogram~of~Delta*AICc,P[Delta*AICc]~.(P2)~~P[C]~.(P))))+
                        ylab("Proportion")+
                        xlab(expression(paste(Delta,"AICc (compared to null model)")))
                      })        
      } else {
        
        P2 <- round(pvalue(rand.dataset = datasetrand, full.dataset = dataset, metric = "AIC", sample.size = dataset$sample.size[1]), digits = 3)
        if(P2 < 0.01){
          P2 = as.character("<0.01")
        }
        
        with(datasetrand, {
          ggplot(datasetrand, aes(x = deltaAICc)) +
            geom_histogram(aes(y = 2 * ..density..), colour = "black", fill = "red", binwidth = 2, alpha = 0.5) +
            theme_classic() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(size = 0.25, colour = "black"),
                  legend.position = "none",
                  plot.title = element_text(size = 16),
                  panel.border = element_rect(colour = "black", fill = NA))+
            geom_vline(aes(xintercept = dataset$deltaAICc[1]), linetype = "dashed", size = 1.5)+
            ggtitle(bquote(atop(Histogram~of~Delta*AICc,P[Delta*AICc]~.(P2)))) +
            ylab("Proportion") +
            xlab(expression(paste(Delta, "AICc (compared to null model)")))
        }) 
      } 
    }