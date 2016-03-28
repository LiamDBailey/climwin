#'Create a histogram of deltaAICc values
#'
#'Create a histogram of deltaAICc values for all fitted climate windows. Compare
#'with randomised data if provided.
#'@param dataset A dataframe containing information on all fitted climate 
#'  windows. Output from \code{\link{climatewin}}.
#'@param datasetrand A dataframe containing information on all fitted climate 
#'  windows using randomised data. Output from \code{\link{randwin}}.
#'@param histq If datasetrand is provided. The quantile of the randomised data 
#'  that will be compared to non-randomised data. Used to determine the 
#'  likelihood of finding a climate window model of a given deltaAICc value at 
#'  random.
#'@return If datasetrand is provided, plothist will return two stacked histograms
#'  to compare the deltaAICc of non-randomised and randomised data. This can 
#'  help determine the likelihood of obtaining a deltaAICc value of fitted 
#'  climate windows at random. Without datasetrand, plotall will create a single
#'  histogram of deltaAICc values for all fitted climate windows.
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'# Plot real and randomised data for the Mass dataset
#' 
#'data(MassOutput)
#'data(MassRand)
#' 
#'plothist(dataset = MassOutput, datasetrand = MassRand)
#'
#'# Plot deltaAICc when no randomised data is provided
#' 
#'data(MassOutput)
#' 
#'plothist(dataset = MassOutput)
#' 
#'@import ggplot2
#'@export

plothist <- function(dataset, datasetrand = NULL){
  
  if (is.null(datasetrand) == TRUE){
    
    if(is.null(dataset$WeightedOutput) == TRUE){
      
      with(dataset, {
        ggplot(dataset, aes(x = deltaAICc)) +
          geom_histogram(aes(y = 2 * ..density..), colour = "black", fill = "red", binwidth = 2, alpha = 0.5) +
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                legend.position = "none",
                plot.title = element_text(size = 16),
                panel.border = element_rect(colour = "black", fill = NA))+
          ggtitle(expression(paste("Histogram of ", Delta,"AICc"))) +
          ylab("Proportion") +
          xlab(expression(paste(Delta, "AICc (compared to null model)")))
      })
      
    } else {
      
      par(mfrow = c(1,1))
      
      if(dataset$WeightedOutput$Weight_function == "W"){
        
        j        <- seq(1:dataset$WeightedOutput$duration) / dataset$WeightedOutput$duration
        weight <- weibull3(x = j[1:dataset$WeightedOutput$duration], shape = dataset$WeightedOutput$shape, 
                           scale = dataset$WeightedOutput$scale, location = dataset$WeightedOutput$location)
        weight[is.na(weight)] <- 0
        
        if (sum(weight) == 0){
          weight <- weight + 1
        }
        
        title <- c("Weibull", paste("shape=", dataset$WeightedOutput$shape, "scale=", dataset$WeightedOutput$scale, 
                                    "location=", dataset$WeightedOutput$location))
        plot((weight / sum(weight)), type = "l", ylab = "weight", xlab = "timesteps", main = title, xaxt = 'n')
        
      } else {
        
        k        <- seq(-10, 10, by = (2 * 10 / dataset$WeightedOutput$duration))
        weight <- dgev(k[1:dataset$WeightedOutput$duration], loc = dataset$WeightedOutput$location, 
                       scale = dataset$WeightedOutput$scale, shape = dataset$WeightedOutput$shape, log = FALSE)
        weight[is.na(weight)] <- 0
        
        if (sum(weight) == 0){
          weight <- weight + 1
        }
        title <- c("GEV", paste("shape=", dataset$WeightedOutput$shape, "scale=", dataset$WeightedOutput$scale, 
                                "location=", dataset$WeightedOutput$loc))
        plot((weight / sum(weight)), type = "l", ylab = "weight", xlab = "timesteps", main = title, xaxt = 'n')
        
      }
    }
  } else { 
    
    if(max(datasetrand$Repeat) <= 100){
      
      if(is.null(dataset$WeightedOutput) == TRUE){
        
        P <- round(pvalue(rand.dataset = datasetrand, full.dataset = dataset, metric = "Spread", sample.size = dataset$sample.size[1]), digits = 3)
        
        with(dataset, {
          ggplot(dataset, aes(x = deltaAICc)) +
            geom_histogram(aes(y = 2 * ..density..), colour = "black", fill = "red", binwidth = 2, alpha = 0.5) +
            theme_classic() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(size = 0.25, colour = "black"),
                  legend.position = "none",
                  plot.title = element_text(size = 16),
                  panel.border = element_rect(colour = "black", fill = NA))+
            ggtitle(bquote(atop(Histogram~of~Delta*AICc,Pspread~.(P))))+
            ylab("Proportion") +
            xlab(expression(paste(Delta, "AICc (compared to null model)")))
        }) 
        
      } else {

        P <- round(pvalue(rand.dataset = datasetrand, full.dataset = dataset$WeightedOutput, 
                          metric = "Spread", sample.size = dataset$WeightedOutput$sample.size[1]), digits = 3)
        par(mfrow = c(1,1))
        
        if(dataset$WeightedOutput$Weight_function == "W"){
          
          j      <- seq(1:dataset$WeightedOutput$duration) / dataset$WeightedOutput$duration
          weight <- weibull3(x = j[1:dataset$WeightedOutput$duration], shape = dataset$WeightedOutput$shape, 
                             scale = dataset$WeightedOutput$scale, location = dataset$WeightedOutput$location)
          weight[is.na(weight)] <- 0
          
          if (sum(weight) == 0){
            weight <- weight + 1
          }
          
          title <- c("Weibull", paste("shape=", dataset$WeightedOutput$shape, "scale=", dataset$WeightedOutput$scale, 
                                      "location=", dataset$WeightedOutput$location, "\n Pspread:", P))
          plot((weight / sum(weight)), type = "l", ylab = "weight", xlab = "timesteps", main = title, xaxt = 'n')
          
        } else {
          
          k        <- seq(-10, 10, by = (2 * 10 / dataset$WeightedOutput$duration))
          weight <- dgev(k[1:dataset$WeightedOutput$duration], loc = dataset$WeightedOutput$location, 
                         scale = dataset$WeightedOutput$scale, shape = dataset$WeightedOutput$shape, log = FALSE)
          weight[is.na(weight)] <- 0
          
          if (sum(weight) == 0){
            weight <- weight + 1
          }
          title <- c("GEV", paste("shape=", dataset$WeightedOutput$shape, "scale=", dataset$WeightedOutput$scale, 
                                  "location=", dataset$WeightedOutput$loc, "\n Pspread:", P))
          plot((weight / sum(weight)), type = "l", ylab = "weight", xlab = "timesteps", main = title, xaxt = 'n')
          
        }
        
      }
    } else if(max(datasetrand$Repeat) > 100 & max(datasetrand$Repeat) < 1000){
      
      P  <- round(pvalue(rand.dataset = datasetrand, full.dataset = dataset, metric = "Spread", sample.size = dataset$sample.size[1]), digits = 3)
      P2 <- round(pvalue(rand.dataset = datasetrand, full.dataset = dataset, metric = "AIC", sample.size = dataset$sample.size[1]), digits = 3)
      
      with(dataset, {
        ggplot(dataset, aes(x = deltaAICc)) +
          geom_histogram(aes(y = 2 * ..density..), colour = "black", fill = "red", binwidth = 2, alpha = 0.5) +
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                legend.position = "none",
                plot.title = element_text(size = 16),
                panel.border = element_rect(colour = "black", fill = NA))+
          ggtitle(bquote(atop(Histogram~of~Delta*AICc,Pspread~.(P)~PAICc~.(P2))))+
          ylab("Proportion") +
          xlab(expression(paste(Delta, "AICc (compared to null model)")))
      })      
      
    } else if(max(datasetrand$Repeat) >= 1000){
      
      P  <- round(pvalue(rand.dataset = datasetrand, full.dataset = dataset, metric = "AIC", sample.size = dataset$sample.size[1]), digits = 3)
      
      with(dataset, {
        ggplot(dataset, aes(x = deltaAICc)) +
          geom_histogram(aes(y = 2 * ..density..), colour = "black", fill = "red", binwidth = 2, alpha = 0.5) +
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                legend.position = "none",
                plot.title = element_text(size = 16),
                panel.border = element_rect(colour = "black", fill = NA))+
          ggtitle(bquote(atop(Histogram~of~Delta*AICc,Pspread~.(P))))+
          ylab("Proportion") +
          xlab(expression(paste(Delta, "AICc (compared to null model)")))
      })
      
    }
  }
}