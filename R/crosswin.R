#'Test the correlation between two climate variables.
#'@param xvar The first climate variable of interest. Please specify the parent 
#'  environment and variable name (e.g. Climate$Temp).
#'@param xvar2 The second climate variable of interest. Please specify the parent 
#'  environment and variable name (e.g. Climate$Temp).
#'@param cdate The climate date variable (dd/mm/yyyy). Please specify the parent
#'  environment and variable name (e.g. Climate$Date).
#'@param bdate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param range Two values signifying respectively the furthest and closest number 
#'  of time intervals (set by cinterval) back from the cutoff date or biological record to include 
#'  in the climate window search.
#'@param stat The aggregate statistic used to analyse the climate data. Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format function(x) 
#'  (...). See FUN in \code{\link{apply}} for more detail.
#'@param stat2 Second aggregate statistic used to analyse climate data (xvar2). Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format function(x) 
#'  (...). See FUN in \code{\link{apply}} for more detail.
#'@param type "absolute" or "relative", whether you wish the climate window to be relative
#'  (e.g. the number of days before each biological record is measured) or absolute
#'  (e.g. number of days before a set point in time).
#'@param refday If type is absolute, the day and month respectively of the 
#'  year from which the absolute window analysis will start.
#'@param cmissing cmissing Determines what should be done if there are 
#'  missing climate data. Three approaches are possible: 
#'   - FALSE; the function will not run if missing climate data is encountered.
#'   An object 'missing' will be returned containing the dates of missing climate.
#'   - "method1"; missing climate data will be replaced with the mean climate
#'   of the preceding and following 2 days.
#'   - "method2"; missing climate data will be replaced with the mean climate
#'   of all records on the same date.
#'@param cinterval The resolution at which climate window analysis will be 
#'  conducted. May be days ("day"), weeks ("week"), or months ("month"). Note the units 
#'  of parameter 'range' will differ depending on the choice 
#'  of cinterval
#'@param spatial A list item containing:
#'  1. A factor that defines which spatial group (i.e. population) each biological
#'  record is taken from. The length of this factor should correspond to the length 
#'  of the biological dataset.
#'  2. A factor that defines which spatial group (i.e. population) climate data
#'  corresponds to. This length of this factor should correspond to the length of
#'  the climate dataset.
#'@param cohort A variable used to group biological records that occur in the same biological
#'  season but cover multiple years (e.g. southern hemisphere breeding season). By default,
#'  autowin will use year (extracted from parameter bdate) as the cohort variable. 
#'  The cohort variable should be in the same dataset as the variable bdate.
#'@param cutoff.day,cutoff.month Redundant parameters. Now replaced by refday.
#'@param furthest,closest Redundant parameters. Now replaced by range.
#'@return Will return a dataframe containing the correlation between the two
#'  climate variables.
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'
#'#Simple test example
#'#Create data from a subset of our test dataset
#'#Just use two years
#'biol_data <- Mass[1:2, ]
#'clim_data <- MassClimate[grep(pattern = "1979|1986", x = MassClimate$Date), ]
#'                    
#'cross <- crosswin(xvar  = list(Temp = clim_data$Temp),
#'                  xvar2 = list(Rain = clim_data$Rain),
#'                  cdate = clim_data$Date, bdate = biol_data$Date,
#'                  range = c(1, 0), 
#'                  stat = "mean", stat2 = "mean",
#'                  type = "relative",
#'                  cmissing = FALSE, cinterval = "day")
#'
#'\dontrun{
#'# Full working example
#'# Test correlation between temperature and rainfall in the MassClimate dataset.
#' 
#'data(Mass)
#'data(MassClimate)
#'
#'cross <- crosswin(xvar = list(Temp = MassClimate$Temp), 
#'                  xvar2 = list(Rain = MassClimate$Rain), 
#'                  cdate = MassClimate$Date, bdate = Mass$Date, 
#'                  range = c(365, 0),
#'                  stat = "mean", stat2 = "mean", type = "relative",
#'                  cmissing = FALSE, cinterval = "day")
#'                 
#'# View the output
#'head(cross)
#' 
#'# Plot the output
#'plotcor(cross, type = "C")
#' 
#'}
#' 
#'@export

crosswin <- function(xvar, xvar2, cdate, bdate, range, 
                     stat, stat2, type, refday,
                     cinterval = "day", 
                     cmissing = FALSE, spatial = NULL, cohort = NULL,
                     cutoff.day = NULL, cutoff.month = NULL,
                     furthest = NULL, closest = NULL){
  
  message("Initialising, please wait...")
  
  #Check date formats
  if(all(is.na(as.Date(cdate, format = "%d/%m/%Y")))){
    
    stop("cdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  if(all(is.na(as.Date(bdate, format = "%d/%m/%Y")))){
    
    stop("bdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  if(is.null(cohort) == TRUE){
    cohort = lubridate::year(as.Date(bdate, format = "%d/%m/%Y")) 
  }
  
  if(type == "variable" || type == "fixed"){
    stop("Parameter 'type' now uses levels 'relative' and 'absolute' rather than 'variable' and 'fixed'.")
  }
  
  if(is.null(furthest) == FALSE & is.null(closest) == FALSE){
    stop("furthest and closest are now redundant. Please use parameter 'range' instead.")
  }
  
  if(is.null(cutoff.day) == FALSE & is.null(cutoff.month) == FALSE){
    stop("cutoff.day and cutoff.month are now redundant. Please use parameter 'refday' instead.")
  }
  
  xvar  <- xvar[[1]]
  xvar2 <- xvar2[[1]]
  
  duration <- (range[1] - range[2]) + 1
  maxmodno <- (duration * (duration + 1))/2 
  cont     <- convertdate(bdate = bdate, cdate = cdate, xvar = xvar, xvar2 = xvar2, 
                          cinterval = cinterval, type = type, cohort = cohort,
                          refday = refday, cross = TRUE, spatial = spatial)   # create new climate dataframe with continuous daynumbers, leap days are not a problem
  
  modno    <- 1  #Create a model number variable that will count up during the loop#
  modlist  <- list()   # dataframes to store ouput
  cmatrix1 <- matrix(ncol = (duration), nrow = length(bdate))  # matrix that stores the weather data for variable or fixed windows
  cmatrix2 <- matrix(ncol = (duration), nrow = length(bdate))  # matrix that stores the weather data for variable or fixed windows

  if(is.null(spatial) == FALSE){
    for (i in 1:length(bdate)){
        cmatrix1[i, ] <- cont$xvar[which(cont$cintno$spatial %in% cont$bintno$spatial[i] & cont$cintno$Date %in% (cont$bintno$Date[i] - c(range[2]:range[1]))), 1]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
        cmatrix2[i, ] <- cont$xvar2[which(cont$cintno$spatial %in% cont$bintno$spatial[i] & cont$cintno$Date %in% (cont$bintno$Date[i] - c(range[2]:range[1]))), 1]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  } else {
    for (i in 1:length(bdate)){
      cmatrix1[i, ] <- cont$xvar[which(cont$cintno %in% (cont$bintno[i] - c(range[2]:range[1])))]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
      cmatrix2[i, ] <- cont$xvar2[which(cont$cintno %in% (cont$bintno[i] - c(range[2]:range[1])))]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  }
  cmatrix1 <- as.matrix(cmatrix1[, c(ncol(cmatrix1):1)])
  cmatrix2 <- as.matrix(cmatrix2[, c(ncol(cmatrix2):1)])
  
  if(is.null(spatial) == FALSE){
    if (cmissing == FALSE && length(which(is.na(cmatrix1))) > 0){
      if(cinterval == "day"){
        .GlobalEnv$missing <- as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if(cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", month(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if(cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", month(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      stop(c("Climate data xvar should not contain NA values: ", length(.GlobalEnv$missing),
             " NA value(s) found. Please add missing climate data or set cmissing=TRUE.
             See object missing for all missing climate data"))
    }  
    if (cmissing == FALSE && length(which(is.na(cmatrix2))) > 0){
      if(cinterval == "day"){
        .GlobalEnv$missing <- as.Date(cont$cintno$Date[is.na(cont$xvar2$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if(cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", month(as.Date(cont$cintno$Date[is.na(cont$xvar2$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", year(as.Date(cont$cintno$Date[is.na(cont$xvar2$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if(cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", month(as.Date(cont$cintno$Date[is.na(cont$xvar2$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", year(as.Date(cont$cintno$Date[is.na(cont$xvar2$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      stop(c("Climate data xvar2 should not contain NA values: ", length(.GlobalEnv$missing),
             " NA value(s) found. Please add missing climate data or set cmissing=TRUE.
             See object missing for all missing climate data"))
    }
  } else {
    if (cmissing == FALSE && length(which(is.na(cmatrix1))) > 0){
      if(cinterval == "day"){
        .GlobalEnv$missing <- as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if(cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", month(as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", year(as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if(cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", month(as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", year(as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      stop(c("Climate data xvar should not contain NA values: ", length(.GlobalEnv$missing),
             " NA value(s) found. Please add missing climate data or set cmissing=TRUE.
           See object missing for all missing climate data"))
    }  
    if (cmissing == FALSE && length(which(is.na(cmatrix2))) > 0){
      if(cinterval == "day"){
        .GlobalEnv$missing <- as.Date(cont$cintno[is.na(cont$xvar2)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if(cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", month(as.Date(cont$cintno[is.na(cont$xvar2)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", year(as.Date(cont$cintno[is.na(cont$xvar2)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if(cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", month(as.Date(cont$cintno[is.na(cont$xvar2)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", year(as.Date(cont$cintno[is.na(cont$xvar2)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      stop(c("Climate data xvar2 should not contain NA values: ", length(.GlobalEnv$missing),
             " NA value(s) found. Please add missing climate data or set cmissing=TRUE.
           See object missing for all missing climate data"))
    } 
  }
  
  if (cmissing != FALSE && length(which(is.na(cmatrix1))) > 0 | cmissing != FALSE && length(which(is.na(cmatrix2))) > 0){
    
    message("Missing climate data detected. Please wait while appropriate data is calculated to replace NAs.")
    
    if(cmissing == "method1"){
      
      for(i in which(is.na(cmatrix1))){
        
        cmatrix1[i] <- mean(c(cmatrix1[i - (1:2)], cmatrix1[i + (1:2)]), na.rm = T)
        
        if(is.na(cmatrix1[i])){
          
          stop("Too many consecutive NAs present in first climate variable. Consider using method2 or manually replacing NAs.")
          
        }
        
      }
      
      for(i in which(is.na(cmatrix2))){
        
        cmatrix2[i] <- mean(c(cmatrix2[i - (1:2)], cmatrix2[i + (1:2)]), na.rm = T)
        
        if(is.na(cmatrix2[i])){
          
          stop("Too many consecutive NAs present in second climate variable. Consider using method2 or manually replacing NAs.")
          
        }
        
      }
      
    } else if(cmissing == "method2"){
      
      cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"),
                              Year  = lubridate::year(as.Date(cdate, format = "%d/%m/%Y")),
                              Month = lubridate::month(as.Date(cdate, format = "%d/%m/%Y")),
                              Day   = lubridate::day(as.Date(cdate, format = "%d/%m/%Y")))
      
      if(cinterval == "week"){
        
        for(j in 1:nrow(cdate_new)){
          
          cdate_new$Week[j] <- ceiling((as.numeric(cdate_new$Date[j]) - min(as.numeric(subset(cdate_new, cdate_new$Year == cdate_new$Year[j])$Date)) + 1) / 7)
          
        }
        
      }
      
      for(i in which(is.na(cmatrix1))){
        
        col <- floor(i/nrow(cmatrix1))
        
        if(is.null(spatial)){
          
          brecord <- cont$bintno[i - col*nrow(cmatrix1)] - (range[2] + col) - 1
          
        } else {
          
          brecord <- cont$bintno$Date[i - col*nrow(cmatrix1)] - (range[2] + col) - 1
          
        }
        
        min_date <- min(as.Date(cdate, format = "%d/%m/%Y"))
        
        if(cinterval == "day"){
          
          missing_rec <- as.Date(brecord, format = "%d/%m/%Y", origin = min_date)
          
          cmatrix1[i] <- mean(xvar[which(cdate_new$Month == lubridate::month(missing_rec) & cdate_new$Day == lubridate::day(missing_rec))], na.rm = T)
          
        } else if(cinterval == "week"){
          
          missing_week <- ceiling(((as.numeric((as.Date(bdate[i - col*nrow(cmatrix1)], format = "%d/%m/%Y"))) - (col*7)) - as.numeric(as.Date(paste("01/01/", lubridate::year(as.Date(bdate[i - col*nrow(cmatrix1)], format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) + 1) / 7)
          
          cmatrix1[i] <- mean(xvar[which(cdate_new$Week == missing_week)], na.rm = T)
          
        } else if(cinterval == "month"){
          
          missing_month <- (lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1)) - (floor((lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1))/12)*12)
          
          cmatrix1[i] <- mean(xvar[which(cdate_new$Month == missing_month)], na.rm = T)
          
        }
        
        if(is.na(cmatrix1[i])){
          
          stop("There is no data available for certain climate records across all years in the first climate variable. Consider using method1 or manually replacing NAs.")
          
        }
        
      }
      
      for(i in which(is.na(cmatrix2))){
        
        col <- floor(i/nrow(cmatrix2))
        
        if(is.null(spatial)){
          
          brecord <- cont$bintno[i - col*nrow(cmatrix2)] - (range[2] + col) - 1
          
        } else {
          
          brecord <- cont$bintno$Date[i - col*nrow(cmatrix2)] - (range[2] + col) - 1
          
        }
        
        min_date <- min(as.Date(cdate, format = "%d/%m/%Y"))
        
        if(cinterval == "day"){
          
          missing_rec <- as.Date(brecord, format = "%d/%m/%Y", origin = min_date)
          
          cmatrix2[i] <- mean(xvar[which(cdate_new$Month == lubridate::month(missing_rec) & cdate_new$Day == lubridate::day(missing_rec))], na.rm = T)
          
        } else if(cinterval == "week"){
          
          missing_week <- ceiling(((as.numeric((as.Date(bdate[i - col*nrow(cmatrix2)], format = "%d/%m/%Y"))) - (col*7)) - as.numeric(as.Date(paste("01/01/", lubridate::year(as.Date(bdate[i - col*nrow(cmatrix2)], format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) + 1) / 7)
          
          cmatrix2[i] <- mean(xvar[which(cdate_new$Week == missing_week)], na.rm = T)
          
        } else if(cinterval == "month"){
          
          missing_month <- (lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1)) - (floor((lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1))/12)*12)
          
          cmatrix2[i] <- mean(xvar[which(cdate_new$Month == missing_month)], na.rm = T)
          
        }
        
        if(is.na(cmatrix2[i])){
          
          stop("There is no data available for certain climate records across all years in the second climate variable. Consider using method1 or manually replacing NAs.")
          
        }
        
      }
      
    } else {
      
      stop("cmissing should be FALSE, 'method1' or 'method2'")
      
    }
    
  }
  
  climate1 <- matrix(ncol = 1, nrow = nrow(cmatrix1), 1)
  climate2 <- matrix(ncol = 1, nrow = nrow(cmatrix2), 1)
  
  pb <- txtProgressBar(min = 0, max = maxmodno, style = 3, char = "|")
  
  for (m in range[2]:range[1]){
    for (n in 1:duration){
      if ( (m - n) >= (range[2] - 1)){  # do not use windows that overshoot the closest possible day in window   
        if (stat != "slope" || stat2 != "slope" || n > 1){
          windowopen  <- m - range[2] + 1
          windowclose <- windowopen - n + 1
          if (stat == "slope"){ 
            time <- seq(1, n, 1)
            climate1 <- apply(cmatrix1[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
            climate2 <- apply(cmatrix2[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
          } else { 
            if (n == 1) {
              climate1 <- cmatrix1[, windowclose:windowopen]
              climate2 <- cmatrix2[, windowclose:windowopen]
            } else {
              climate1 <- apply(cmatrix1[, windowclose:windowopen], 1, FUN = stat) 
              climate2 <- apply(cmatrix2[, windowclose:windowopen], 1, FUN = stat)
            }
            if (stat2 == "slope"){ 
              time     <- seq(1, n, 1)
              climate2 <- apply(cmatrix2[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
            } else { 
              if (n == 1) {
                climate2 <- cmatrix2[, windowclose:windowopen]
              } else {
                climate2 <- apply(cmatrix2[, windowclose:windowopen], 1, FUN = stat)
              }
            }
          }
          # Run the model
          modeloutput <- cor(climate1, climate2)
          # Add model parameters to list#
          modlist$cor[modno]         <- modeloutput
          modlist$WindowOpen[modno]  <- m
          modlist$WindowClose[modno] <- m - n + 1
          modno                        <- modno + 1 # Increase modno#
        }
      }
    }  
    if (interactive()){
      setTxtProgressBar(pb, modno - 1) 
    }
  }
  modlist$Furthest    <- range[1]
  modlist$Closest     <- range[2]
  modlist$Statistics  <- stat
  modlist$Statistics2 <- stat2
  modlist$Type        <- type
  
  if (type == "fixed"){
    modlist$Reference.day   <- refday[1]
    modlist$Reference.month <- refday[2]
  }
  return(as.data.frame(modlist))
}