#'Calculate within group deviance.
#'
#'Calculate within group deviance of a variable. Used to test for 'within individual effects'.
#'See methods outlined in van de Pol and Wright 2009 for more detail.
#'@param covar Continuous variable for which within group deviance will be calculated.
#'Please specify the dataset in which the variable is found (i.e. data$var).
#'@param groupvar Grouping variable within which deviance will be calculated.
#'For example, individual ID or site/plot ID. Please specify the dataset in which
#'the variable is found (i.e. data$var).
#'@return Returns a vector containing numeric values. This can be used to
#'differentiate within and between group effects in a model. 
#'See function \code{\link{wgmean}} to calculate within group means.
#'See van de Pol and Wright 2009 for more detail.
#'@author Martijn van de Pol and Jonathan Wright
#'@examples
#'# Calculate within year deviance in temperature from the MassClimate dataset.  
#'
#'data(MassClimate)
#'
#'#Calculate year column
#'library(lubridate)
#'MassClimate$Year <- year(as.Date(MassClimate$Date, format = "%d/%m/%Y"))
#'
#'#Calculate within year deviance of temperature
#'within_yr_dev <- wgdev(MassClimate$Temp, MassClimate$Year)
#'
#'#Add this variable to the original dataset
#'MassClimate$Within_yr_dev <- within_yr_dev
#'              
#'@export

wgdev <- function(covar, groupvar) {
  a            <- unique(factor(groupvar))
  groups       <- length(a)
  temp         <- rep(NA, groups)
  observations <- length(covar)
  groupmean    <- rep(NA, observations)
  groupdev     <- rep(NA, observations)
  
  for (i in 1:groups){
    b       <- which(groupvar == a[i])
    temp[i] <- mean(covar[b], na.rm=TRUE)
  }
  
  for (j in 1:observations){
    c            <- which(a == groupvar[j])
    groupmean[j] <- temp[c]
    groupdev[j]  <- covar[j] - groupmean[j]
  }
  return(groupdev)
}