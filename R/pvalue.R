#'Determine the probability that a given climate signal is 'true'.
#'
#'Calculate probability that a given climate signal is 'true' using
#' either PDAICc or Pc.
#'
#'@param dataset A dataframe containing information on all fitted climate 
#'  windows. Output from \code{\link{slidingwin}}.
#'@param datasetrand A dataframe containing information on all fitted climate 
#'  windows using randomised data. Output from \code{\link{randwin}}.
#'@param metric "AIC" or "C". Determine whether a value of PDAICc or
#'  Pc will be returned.
#'@param sample.size Sample size of analysis.

#'@return Returns a value representing the probability that a given climate 
#'  window result is a false positive. 
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'
#'# Calculate PDAICc for the Mass dataset
#'
#'pvalue(datasetrand = MassRand, dataset = MassOutput, 
#'       metric = "AIC", sample.size = 47)
#'       
#'# Calculate Pc for the Mass dataset
#'
#'pvalue(datasetrand = MassRand, dataset = MassOutput,
#'       metric = "C", sample.size = 47) 
#'
#'@export

pvalue <- function(dataset, datasetrand, metric, sample.size){
  
  if(metric == "AIC"){
    
    if(is.null(dataset$WeightedOutput)){
      
      Percentile <- ecdf(datasetrand$deltaAICc)
      PDAIC <- ifelse(Percentile(dataset$deltaAICc[1]) == 0, "<0.001", Percentile(dataset$deltaAICc[1]))
      return(PDAIC)
      
    } else {
      
      Percentile <- ecdf(datasetrand$deltaAICc)
      PDAIC <- ifelse(Percentile(dataset$WeightedOutput$deltaAICc) == 0, "<0.001", Percentile(dataset$WeightedOutput$deltaAICc))
      return(PDAIC)
      
    }
    
  } else if(metric == "C"){
    
    if(!is.null(dataset$WeightedOutput)){
      
      stop("p-values can only be calculated for the weightwin function using the AIC metric.")
      
    }
    
    if(is.null(sample.size) == TRUE){
      stop("Please provide a value for sample size")
    }
    
    if(sample.size > 47){
      sample.size = 47
      warning("Pc will be overly conservative when sample size is greater than 47")
    } else if(sample.size < 10){
      sample.size = 10
      warning("Pc will be overly liberal when sample size is less than 10")
    }
    
    WeightDist <- sum(as.numeric(cumsum(dataset$ModWeight) <= 0.95))/nrow(dataset)
    
    DeltaW <- WeightDist - median(datasetrand$WeightDist)
      
      if(dataset$K[1] >= 1){
        Pc <- (1/(1 + exp(-1 * (-0.621031 + 11.563537 * DeltaW + 0.058663 * sample.size + 6.882248 * DeltaW * sample.size))))
      } else {
        Pc <- (1/(1 + exp(-1 * (-0.540324 + 1.947674 * DeltaW + 0.078708 * sample.size + 0.313567 * DeltaW * sample.size))))
      }
    
    return(Pc)
    
  } else {
    stop("'metric' should be either 'AIC' or 'C'")
  }
}