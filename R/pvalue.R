#'Determine the probability that a given climate signal is 'true'
#'
#'Calculate either PDAICc or Pw of a given climate window. See ___
#'for more detail
#'@param rand.dataset Output dataframe of function \code{\link{randwin}}.
#'@param full.dataset Output dataframe of function \code{\link{climatewin}}.
#'@param metric "AIC" or "Weight". Determine whether a value of PDAICc or
#'  Pw will be returned.
#'@param sample.size Sample size of analysis. Relevant for metric Pw only.

#'@return Returns a value representing the probability that a given climate 
#'  window result is a false positive.#'  
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
    
    DeltaW <- full.dataset$ModWeight[1] - median(rand.dataset$ModWeight)
      
      if(full.dataset$K[1] >= 10){
        Pw <- 1 - (1/(1 + exp(-1 * (-0.621031 + 11.563537 * DeltaW + 0.058663 * sample.size + 6.882248 * DeltaW * sample.size))))
      } else {
        Pw <- 1 - (1/(1 + exp(-1 * (-0.540324 + 1.947674 * DeltaW + 0.078708 * sample.size + 0.313567 * DeltaW * sample.size))))
      }

    return(Pw)
    
  } else {
    stop("'metric' should be either 'AIC' or 'Spread'")
  }
}