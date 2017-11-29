#'Calculate within group means.
#'
#'Calculate group means of a variable. Used to test for 'between individual effects'.
#'See methods outlined in van de Pol and Wright 2009 for more detail.
#'@param covar Continuous variable for which group means will be calculated.
#'Please specify the dataset in which the variable is found (i.e. data$var).
#'@param groupvar Grouping variable within which means will be calculated.
#'For example, individual ID or site/plot ID. Please specify the dataset in which
#'the variable is found (i.e. data$var).
#'@return Returns a vector containing numeric values. This can be used to
#'differentiate within and between group effects in a model.
#'See function \code{\link{wgdev}} to calculate within group deviance. 
#'See van de Pol and Wright 2009 for more details on the method.
#'@author Martijn van de Pol and Jonathan Wright
#'@examples
#'# Calculate mean temperature within years from the MassClimate dataset.  
#'
#'data(MassClimate)
#'
#'#Calculate year column
#'library(lubridate)
#'MassClimate$Year <- year(as.Date(MassClimate$Date, format = "%d/%m/%Y"))
#'
#'#Calculate mean temperature within each year
#'within_yr_mean <- wgmean(MassClimate$Temp, MassClimate$Year)
#'
#'#Add this variable to the original dataset
#'MassClimate$Within_yr_mean <- within_yr_mean
#'              
#'@export

wgmean <- function(covar, groupvar){
  a            <- unique(factor(groupvar))
  groups       <- length(a)
  observations <- length(covar)
  temp         <- rep(NA, groups)
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
  groupmean[which(is.nan(groupmean)==TRUE)]<-NA
  return(groupmean)
}