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
#'plothist(datasetrand = MassRand, dataset = MassOutput)
#' 
#'@import ggplot2
#'@export

plothist <- function(dataset, datasetrand){
  
  if(is.null(datasetrand) == TRUE){
    stop("Please provide randomised data")
  }  
  
if(max(datasetrand$Repeat) < 100){
      warning("PDeltaAICc may be unreliable with so few randomisations")
    }
          
    if(is.null(dataset$shape) == TRUE){
      
      P  <- round(pvalue(datasetrand = datasetrand, dataset = dataset, metric = "C", sample.size = dataset$sample.size[1]), digits = 3)
      P2 <- pvalue(datasetrand = datasetrand, dataset = dataset, metric = "AIC", sample.size = dataset$sample.size[1])
      if(P2 < 0.01 | P2 == "<0.001"){
        P2 = as.character("<0.001")
      }
      if(P < 0.001){
        P = as.character("<0.001")
      }
      
      with(datasetrand, {ggplot(datasetrand, aes(x = deltaAICc, fill = Randomised))+
                        geom_histogram(aes(y = after_stat(2*density)), colour = "black", binwidth = 2, alpha = 0.5, size = 1)+
                        theme_climwin()+
                        geom_vline(aes(xintercept = dataset$deltaAICc[1]), linetype = "dashed", size = 1.5)+
                        ggtitle(bquote(atop(Histogram~of~Delta*AICc,P[Delta*AICc]~.(P2)~~P[C]~.(P))))+
                        ylab("Proportion")+
                        xlab(expression(paste(Delta,"AICc (compared to null model)")))
                      })        
      } else {
        
        P2 <- round(pvalue(datasetrand = datasetrand, dataset = dataset, metric = "AIC", sample.size = dataset$sample.size[1]), digits = 3)
        if(P2 < 0.01){
          P2 = as.character("<0.01")
        }
        
        with(datasetrand, {
          ggplot(datasetrand, aes(x = deltaAICc)) +
            geom_histogram(aes(y = after_stat(2*density)), colour = "black", fill = "red", binwidth = 2, alpha = 0.5, size = 1) +
            theme_climwin() +
            geom_vline(aes(xintercept = dataset$deltaAICc[1]), linetype = "dashed", size = 1.5)+
            ggtitle(bquote(atop(Histogram~of~Delta*AICc,P[Delta*AICc]~.(P2)))) +
            ylab("Proportion") +
            xlab(expression(paste(Delta, "AICc (compared to null model)")))
        }) 
      } 
    }