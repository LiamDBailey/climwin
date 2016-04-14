#'Determine the probability that a given climate signal is 'true'
#'
#'Calculate either PDAICc or Pc of a given climate window. See ___
#'for more detail
#'@param rand.dataset Output dataframe of function \code{\link{randwin}}.
#'@param full.dataset Output dataframe of function \code{\link{climatewin}}.
#'@param metric "AIC" or "Spread". Determine whether a value of PDAICc or
#'  Pc will be returned.
#'@param sample.size Sample size of analysis. Relevant for metric Pw only.

#'@return Returns a value representing the probability that a given climate 
#'  window result is a false positive. 
#'@author Liam D. Bailey and Martijn van de Pol
#'@export

pvalue <- function(rand.dataset, full.dataset, metric, sample.size){
  
  if(metric == "AIC"){
    
    Percentile <- ecdf(rand.dataset$deltaAICc)
    return(Percentile(full.dataset$deltaAICc[1]))
    
  } else if(metric == "Spread"){
    
    if(is.null(sample.size) == TRUE){
      stop("Please provide a value for sample size")
    }
    
    if(sample.size > 47){
      sample.size = 47
      print("Pc will be overly conservative when sample size is greater than 47")
    } else if(sample.size < 10){
      sample.size = 10
      print("Pc will be overly liberal when sample size is less than 10")
    }
    
    WeightDist <- sum(as.numeric(cumsum(full.dataset$ModWeight) <= 0.95))/nrow(full.dataset)
    
    DeltaW <- WeightDist - median(rand.dataset$WeightDist)
      
      if(full.dataset$K[1] >= 10){
        Pc <- (1/(1 + exp(-1 * (-0.621031 + 11.563537 * DeltaW + 0.058663 * sample.size + 6.882248 * DeltaW * sample.size))))
      } else {
        Pc <- (1/(1 + exp(-1 * (-0.540324 + 1.947674 * DeltaW + 0.078708 * sample.size + 0.313567 * DeltaW * sample.size))))
      }

    return(Pc)
    
  } else {
    stop("'metric' should be either 'AIC' or 'Spread'")
  }
}