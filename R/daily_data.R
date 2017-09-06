#'Convert climate data to a daily format.
#'
#'Convert data from a monthly or weekly format into a daily format for use in climwin.
#'@param date The climate date variable (dd/mm/yyyy). Please specify the parent
#'  environment and variable name (e.g. Climate$Date). 
#'@param climate The climate variables of interest. 
#'  Please specify the parent environment and variable name (e.g. Climate$Temp).
#'@param scale The temporal scale of the original dataset. Either "week" or "month". 
#'  
#'@return Returns a data frame with daily date information and corresponding climate data.
#'Where a single climate record is available per week/month, this value will be replicated
#'across all other days in that same week/month.
#'  
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'\dontrun{
#'
#'#Load data
#'data(Monthly_data)
#'
#'#View original data to see monthly format.
#'head(Monthly_data)
#'
#'#Convert data into a daily data format.
#'newdat <- daily_data(date = Monthly_data$Date, climate = Monthly_data$Temp, scale = "month")
#'
#'#View new data to see daily format.
#'head(newdat, 35)
#'
#'#Note that data from each month has been duplicated across all days within the month.
#'#This is needed to work within climwin. Results will not be impacted.
#'
#'}
#'
#'@export

#Function to turn weekly/monthly data into a daily format

daily_data <- function(date, climate, scale){
  
  dates <- data.frame(date = as.Date(date, format = "%d/%m/%Y"), clim = climate)
  
  dates$year <- lubridate::year(dates$date)
  
  fulldates <- data.frame(date = seq(min(dates$date), max(dates$date), "days"))
  
  fulldates$year <- lubridate::year(fulldates$date)
  
  dates$year2 <- dates$year - min(fulldates$year)
  
  fulldates$year2 <- fulldates$year - min(fulldates$year)
  
  if(scale == "week"){
    
    dates$weeks     <- lubridate::week(dates$date)
    
    dates$weeks     <- ifelse(dates$weeks == 53, 52, dates$weeks)
    
    dates$date <- NULL
    
    #In case there are multiple records for a given week, take the mean
    dates2 <- melt(dates, id = c("weeks", "year"))
    dates2 <- cast(dates2, weeks + year ~ variable, mean, na.rm = T)
    
    fulldates$weeks  <- lubridate::week(fulldates$date)
    
    fulldates$weeks  <- ifelse(fulldates$weeks == 53, 52, fulldates$weeks)
    
    fulldates$weeks2 <- fulldates$weeks + 52 * fulldates$year2
    dates2$weeks2     <- dates2$weeks + 52 * dates2$year2
    
    fulldates$xvar  <- NA
    
    for(i in 1:nrow(fulldates)){
      
      fulldates$xvar[i]  <- ifelse(fulldates$weeks2[i] %in% dates2$weeks2, 
                                   dates2$clim[which(fulldates$weeks2[i] == dates2$weeks2)],
                                   NA)
      
    }
    
  } else if(scale == "month"){
    
    dates$months     <- lubridate::month(dates$date)
    
    dates$date <- NULL
    
    #In case there are multiple records for a given week, take the mean
    dates2 <- melt(dates, id = c("months", "year"))
    dates2 <- cast(dates2, months + year ~ variable, mean, na.rm = T)
    
    fulldates$months  <- lubridate::month(fulldates$date)
    
    fulldates$months2 <- fulldates$months + 12 * fulldates$year2
    dates2$months2     <- dates2$months + 12 * dates2$year2
    
    fulldates$xvar  <- NA
    
    for(i in 1:nrow(fulldates)){
      
      fulldates$xvar[i]  <- ifelse(fulldates$months2[i] %in% dates2$months2, 
                                   dates2$clim[which(fulldates$months2[i] == dates2$months2)],
                                   NA)
      
    }
    
  }
  
  return(data.frame(Date = paste(lubridate::day(fulldates$date), "/", lubridate::month(fulldates$date), "/", lubridate::year(fulldates$date), sep = ""),
                    xvar = fulldates$xvar))
  
}