#'Fit a single climate window
#'
#'Fit a single climate window with a known start and end time.
#'@param xvar A list object containing all climate variables of interest. 
#'  Please specify the parent environment and variable name (e.g. Climate$Temp).
#'@param cdate The climate date variable (dd/mm/yyyy). Please specify the parent
#'  environment and variable name (e.g. Climate$Date).
#'@param bdate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for testing correlation. 
#'  Currently known to support lm, glm, lmer and glmer objects.
#'@param range Two values signifying respectively the furthest and closest number 
#'  of time intervals (set by cinterval) back from the cutoff date or biological record to include 
#'  in the climate window search.
#'@param stat The aggregate statistic used to analyse the climate data. Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format function(x)
#'  (...). See FUN in \code{\link{apply}} for more detail.
#'@param func The functions used to fit the climate variable. Can be linear 
#'  ("lin"), quadratic ("quad"), cubic ("cub"), inverse ("inv") or log ("log").
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
#'  of cinterval.
#'@param upper Cut-off values used to determine growing degree days or positive 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, climatewin will instead calculate an 
#'  optimal climate zone.
#'@param lower Cut-off values used to determine chill days or negative 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, climatewin will instead calculate an 
#'  optimal climate zone.
#'@param binary TRUE or FALSE. Determines whether to use values of upper and
#'  lower to calculate binary climate data (thresh = TRUE), or to use for
#'  growing degree days (thresh = FALSE).
#'@param centre A list item containing:
#'  1. The variable used for mean centring (e.g. Year, Site, Individual). 
#'  Please specify the parent environment and variable name (e.g. Biol$Year).
#'  2. Whether the model should include both within-group means and variance ("both"),
#'  only within-group means ("mean"), or only within-group variance ("dev").
#'@param cohort A variable used to group biological records that occur in the same biological
#'  season but cover multiple years (e.g. southern hemisphere breeding season). Only required
#'  when type is "absolute". The cohort variable should be in the same dataset as the variable bdate.
#'@param spatial A list item containing:
#'  1. A factor that defines which spatial group (i.e. population) each biological
#'  record is taken from. The length of this factor should correspond to the length 
#'  of the biological dataset.
#'  2. A factor that defines which spatial group (i.e. population) climate data
#'  corresponds to. This length of this factor should correspond to the length of
#'  the climate dataset.
#'@param cutoff.day,cutoff.month Redundant parameters. Now replaced by refday.
#'@param furthest,closest Redundant parameters. Now replaced by range.
#'@param thresh Redundant parameter. Now replaced by binary.
#'@return Will return a list containing two objects:
#'  
#'  \itemize{
#'  \item BestModel, a model object of the fitted climate window
#'  model.
#'  
#'  \item BestModelData, a dataframe with the biological and climate data
#'  used to fit the climate window model.}
#'  
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'
#'#Simple test example
#'#Create data from a subset of our test dataset
#'#Just use two years
#'biol_data <- Mass[1:2, ]
#'clim_data <- MassClimate[grep(pattern = "1979|1986", x = MassClimate$Date), ]
#'
#'single <- singlewin(xvar = list(Temp = clim_data$Temp),
#'                    cdate = clim_data$Date, 
#'                    bdate = biol_data$Date, 
#'                    baseline = lm(Mass ~ 1, data = biol_data),
#'                    range = c(1, 0), 
#'                    type = "relative", stat = "mean", 
#'                    func = c("lin"), cmissing = FALSE, cinterval = "day")
#'
#'\dontrun{
#'# Full working example
#'# Fit a known climate window to the datasets Mass and MassClimate
#'
#'data(Mass)
#'data(MassClimate)
#'
#'# Test for a fixed climate window, starting from 20th May
#'# Fit a climate window starting 72 days ago and ending 15 days ago
#'# Fit a linear term for the mean climate
#'# Fit climate windows at the resolution of days
#'
#'single <- singlewin(xvar = list(Temp = MassClimate$Temp), 
#'                    cdate = MassClimate$Date, bdate = Mass$Date,
#'                    baseline = lm(Mass ~ 1, data = Mass), 
#'                    range = c(72, 15),
#'                    stat = "mean", func = "lin",
#'                    type = "absolute", refday = c(20, 5),
#'                    cmissing = FALSE, cinterval = "day")
#'                
#'##View data##
#'single$BestModel
#'head(single$BestModelData)
#'}
#'
#'@importFrom MuMIn AICc
#'@importFrom lubridate year
#'@importFrom lubridate month
#'@export

singlewin <- function(xvar, cdate, bdate, baseline, 
                      range, stat, func, 
                      type, refday, 
                      cmissing = FALSE, cinterval = "day",
                      cohort = NULL, spatial = NULL,
                      upper = NA, lower = NA, binary = FALSE,
                      centre = list(NULL, "both"), cutoff.day = NULL, cutoff.month = NULL,
                      furthest = NULL, closest = NULL, thresh = NULL){
  
  ### Implementing scientific notation can cause problems because years
  ### are converted to characters in scientific notation (e.g. 2000 = "2e+3")
  ### Check options and convert scipen TEMPORARILY if needed.
  if(getOption("scipen") < 0){
    
    current_option <- getOption("scipen")
    options(scipen = 0)
    
  }
  
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
  
  thresholdQ <- "N"
  
  if((!is.na(upper) || !is.na(lower)) && (cinterval == "week" || cinterval == "month")){
    
    thresholdQ <- readline("You specified a climate threshold using upper and/or lower and are working at a weekly or monthly scale. 
                           Do you want to apply this threshold before calculating weekly/monthly means (i.e. calculate thresholds for each day)? Y/N")
    
    thresholdQ <- toupper(thresholdQ)
    
    if(thresholdQ != "Y" & thresholdQ != "N"){
      
      thresholdQ <- readline("Please specify yes (Y) or no (N)")
      
    }
    
  }
  
  
  
  if(is.null(thresh) == FALSE){
    stop("Parameter 'thresh' is now redundant. Please use parameter 'binary' instead.")
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
  
  xvar = xvar[[1]]
  
  if(stat == "slope" & func == "log" || stat == "slope" & func == "inv"){
    stop("stat = slope cannot be used with func = LOG or I as negative values may be present")
  }
  
  duration  <- (range[1] - range[2]) + 1
  
  bdate  <- as.Date(bdate, format = "%d/%m/%Y") # Convert the date variables into the R date format
  
  if(is.null(spatial) == FALSE){
    SUB.DATE <- list()
    NUM <- 1
    for(i in levels(as.factor(spatial[[2]]))){
     SUB <- cdate[which(spatial[[2]] == i)]
     SUB.DATE[[NUM]] <- data.frame(Date = seq(min(as.Date(SUB, format = "%d/%m/%Y")), max(as.Date(SUB, format = "%d/%m/%Y")), "days"),
                                   spatial = i)
     if (length(SUB.DATE[[NUM]]$Date) != length(unique(SUB.DATE[[NUM]]$Date))){
       stop ("There are duplicate dayrecords in climate data")
     }
     NUM <- NUM + 1
    }
    spatialcdate <- do.call(rbind, SUB.DATE)
    cdate2       <- spatialcdate$Date
    cintno       <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to cintno 1
    realbintno   <- as.numeric(bdate) - min(as.numeric(cdate2)) + 1
  } else {
    cdate2     <- seq(min(as.Date(cdate, format = "%d/%m/%Y")), max(as.Date(cdate, format = "%d/%m/%Y")), "days")
    cintno     <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to cintno 1
    realbintno <- as.numeric(bdate) - min(as.numeric(cdate2)) + 1
    if (length(cintno) != length(unique(cintno))){
      stop ("There are duplicate dayrecords in climate data")
    }
  }
  
  cdate  <- as.Date(cdate, format = "%d/%m/%Y")
  
  if(is.null(spatial) == FALSE){
    for(i in levels(as.factor(spatial[[2]]))){
      SUB <- cdate[which(spatial[[2]] == i)]
      if (min(SUB) > min(bdate) | max(SUB) < max(bdate)){
        stop("Climate data does not cover all years of biological data. Please increase range of climate data")
      }
    }
  } else if (min(cdate) > min(bdate) | max(cdate) < max(bdate)){
    stop("Climate data does not cover all years of biological data. Please increase range of climate data")
  }
  
  if(is.null(spatial) == FALSE){
    xvar       <- data.frame(Clim = xvar, spatial = spatial[[2]])
    cdate      <- data.frame(Date = cdate, spatial = spatial[[2]])
    split.list <- list()
    NUM <- 1
    for(i in unique(xvar$spatial)){
      SUB <- subset(xvar, spatial == i)
      SUBcdate  <- subset(cdate, spatial == i)
      SUBcdate2 <- subset(spatialcdate, spatial == i)
      rownames(SUB) <- seq(1, nrow(SUB), 1)
      rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
      NewClim    <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)]
      Newspatial <- rep(i, times = length(NewClim))
      split.list[[NUM]] <- data.frame(NewClim, Newspatial)
      NUM <- NUM + 1
    }
    xvar    <- (do.call(rbind, split.list))$NewClim
    climspatial <- (do.call(rbind, split.list))$Newspatial
  } else {
    xvar    <- xvar[match(cdate2, cdate)]
  }  
  if (cinterval != "day" && cinterval != "week" && cinterval != "month"){
    stop("cinterval should be either day, week or month")
  }
  
  if(cinterval == "day"){  
    if(type == "absolute"){   
      if(is.null(cohort) == FALSE){
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for(i in levels(as.factor(cohort))){
          sub                               <- subset(newdat, cohort == i)
          bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
        }
      } else {
        bintno            <- as.numeric(as.Date(paste(refday[1], refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1 
      }
    } else {
      bintno <- realbintno
    }
  } else if (cinterval == "week"){
    
    if(!is.na(thresholdQ) && thresholdQ == "Y"){
      
      if(binary == T){
        
        if(is.na(upper) == FALSE && is.na(lower) == TRUE){
          
          xvar <- ifelse(xvar > upper, 1, 0)
          
        } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){
          
          xvar <- ifelse(xvar < lower, 1, 0)
          
        } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){
          
          xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)
          
        }
        
      } else {
        
        if(is.na(upper) == FALSE && is.na(lower) == TRUE){
          
          xvar <- ifelse(xvar > upper, xvar, 0)
          
        } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){
          
          xvar <- ifelse(xvar < lower, xvar, 0)
          
        } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){
          
          xvar <- ifelse(xvar > lower & xvar < upper, xvar, 0)
          
        }
        
      }
    }
    
    cweek      <- lubridate::week(cdate2) # atrribute week numbers for both datafiles with first week in climate data set to cintno 1
    #A year doesn't divide evenly into weeks (what a silly method of splitting up a year!)
    #Therefore, there is a week 53 which has 1-2 days (depending on leapyears)
    #For our purposes, we will group the 1-2 days in week 53 into week 52.
    cweek[which(cweek == 53)] <- 52
    cyear      <- lubridate::year(cdate2) - min(lubridate::year(cdate2))
    cintno     <- cweek + 52 * cyear
    realbintno <- lubridate::week(bdate) + 52 * (lubridate::year(bdate) - min(lubridate::year(cdate2)))
    #cintno     <- ceiling((as.numeric(cdate2) - min(as.numeric(cdate2)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to cintno 1
    #realbintno <- ceiling((as.numeric(bdate) - min(as.numeric(cdate2)) + 1) / 7)
    if(is.null(spatial) == FALSE){
      newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial)
      newclim2    <- melt(newclim, id = c("cintno", "spatial"))
      newclim3    <- cast(newclim2, cintno + spatial ~ variable, stat, na.rm = T)
      newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ]
      cintno      <- newclim3$cintno
      xvar        <- newclim3$xvar
      climspatial <- newclim3$spatial
    } else {
      newclim     <- data.frame("cintno" = cintno, "xvar" = xvar)
      newclim2    <- melt(newclim, id = "cintno")
      newclim3    <- cast(newclim2, cintno ~ variable, stat, na.rm = T)
      cintno      <- newclim3$cintno
      xvar        <- newclim3$xvar
    }
    if (type == "absolute"){
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for(i in levels(as.factor(cohort))){
          sub                               <- subset(newdat, cohort == i)
          bintno[as.numeric(rownames(sub))] <- lubridate::month(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) + 53 * (min(lubridate::year(sub$bdate)) - min(year(cdate2)))
        }
    } else {
      bintno <- realbintno
    }
  } else if (cinterval == "month"){
    
    if(!is.na(thresholdQ) && thresholdQ == "Y"){
      
      if(binary == T){
        
        if(is.na(upper) == FALSE && is.na(lower) == TRUE){
          
          xvar <- ifelse(xvar > upper, 1, 0)
          
        } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){
          
          xvar <- ifelse(xvar < lower, 1, 0)
          
        } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){
          
          xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)
          
        }
        
      } else {
        
        if(is.na(upper) == FALSE && is.na(lower) == TRUE){
          
          xvar <- ifelse(xvar > upper, xvar, 0)
          
        } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){
          
          xvar <- ifelse(xvar < lower, xvar, 0)
          
        } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){
          
          xvar <- ifelse(xvar > lower & xvar < upper, xvar, 0)
          
        }
        
      }
    }
    
    cmonth     <- lubridate::month(cdate2)
    cyear      <- year(cdate2) - min(year(cdate2))
    cintno     <- cmonth + 12 * cyear
    realbintno <- lubridate::month(bdate) + 12 * (year(bdate) - min(year(cdate2)))
    if(is.null(spatial) == FALSE){
      newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial)
      newclim2    <- melt(newclim, id = c("cintno", "spatial"))
      newclim3    <- cast(newclim2, cintno + spatial ~ variable, stat)
      newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ]
      cintno      <- newclim3$cintno
      xvar        <- newclim3$xvar
      climspatial <- newclim3$spatial
    } else {
      newclim    <- data.frame("cintno" = cintno, "xvar" = xvar)
      newclim2   <- melt(newclim, id = "cintno")
      newclim3   <- cast(newclim2, cintno ~ variable, stat)
      cintno     <- newclim3$cintno
      xvar       <- newclim3$xvar 
    }
    if (type == "absolute"){ 
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for(i in levels(as.factor(cohort))){
          sub                               <- subset(newdat, cohort == i)
          bintno[as.numeric(rownames(sub))] <- refday[2] + 12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
        }
    } else {
      bintno <- realbintno
    }
  }
  
  if(cinterval == "day"){
    if((min(bintno) - range[1]) < min(cintno)){
      stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
    }
  }

  #if(cinterval == "week"){
  #  if((min(bintno) - range[1] * 7) < min(cintno)){
  #    stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data."#)
  #  }
  #}
  
  if(cinterval == "month"){
    if((as.numeric(min(as.Date(bdate, format = "%d/%m/%Y")) - months(range[1])) - (as.numeric(min(as.Date(cdate, format = "%d/%m/%Y"))))) <= 0){
      stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
    }
  }
  
  if(max(bintno) > max(cintno)){
    if(type == "absolute"){
      stop("You need more recent biological data. This error may be caused by your choice of refday")
    } else {
      stop("You need more recent biological data")
    }
  }
  
  if(class(baseline)[1] == "lme"){
    
    baseline  <- update(baseline, .~.)
    
  } else {
    
    baseline  <- my_update(baseline, .~.) 
    
  }
  nullmodel <- AICc(baseline)  
  modlist   <- list()   # dataframes to store ouput
  cmatrix   <- matrix(ncol = (duration), nrow = length(bdate))
  modeldat  <- model.frame(baseline)
  
  if(attr(baseline, "class")[1] == "lme"){
    
    if(is.null(baseline$modelStruct$varStruct) == FALSE && !is.null(attr(baseline$modelStruct$varStruct, "groups"))){
      
      modeldat <- cbind(modeldat, attr(baseline$modelStruct$varStruct, "groups"))
      
      colnames(modeldat)[ncol(modeldat)] <- strsplit(x = as.character(attr(baseline$modelStruct$varStruct, "formula"))[2], split = " | ")[[1]][3]

    }
    
    non_rand <- ncol(modeldat)
    
    modeldat <- cbind(modeldat, baseline$data[, colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")]])
    
    colnames(modeldat)[-(1:non_rand)] <- colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")]
    
    modeloutput <- update(baseline, .~., data = modeldat)
    
  }
  
  colnames(modeldat)[1] <- "yvar"
  
  

  if(is.null(centre[[1]]) == FALSE){
    func = "centre"
  }
  
  if(length(modeldat$yvar) != length(bdate)){
    stop("NA values present in biological response. Please remove NA values")
  }
  
  if(cinterval == "day" || (!is.na(thresholdQ) && thresholdQ == "N")){ #If dealing with daily data OR user chose to apply threshold later...
    
    if(is.null(spatial) == FALSE){ #...and spatial information is provided...
      
      if (is.na(upper) == FALSE && is.na(lower) == TRUE){ #...and an upper bound is provided...
        if (binary == TRUE){ #...and we want data to be binary (i.e. it's above the value or it's not)
          xvar$Clim <- ifelse (xvar$Clim > upper, 1, 0) #Then turn climate data into binary data.
        } else { #Otherwise, if binary is not true, simply make all data below the upper limit into 0.
          xvar$Clim <- ifelse (xvar$Clim > upper, xvar$Clim, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == TRUE){ #If a lower limit has been provided, do the same.
        if (binary == TRUE){
          xvar$Clim <- ifelse (xvar$Clim < lower, 1, 0)
        } else {
          xvar$Clim <- ifelse (xvar$Clim < lower, xvar$Clim, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == FALSE){ #If both an upper and lower limit are provided, do the same.
        if (binary == TRUE){
          xvar$Clim <- ifelse (xvar$Clim > lower && xvar$Clim < upper, 1, 0)
        } else {
          xvar$Clim <- ifelse (xvar$Clim > lower && xvar$Clim < upper, xvar$Clim - lower, 0)
        } 
      }
      
    } else { #Do the same with non-spatial data (syntax is just a bit different, but method is the same.)
      
      if (!is.na(upper) && is.na(lower)){
        if (binary == TRUE){
          xvar <- ifelse (xvar > upper, 1, 0)
        } else {
          xvar <- ifelse (xvar > upper, xvar, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == TRUE){
        if (binary == TRUE){
          xvar <- ifelse (xvar < lower, 1, 0)
        } else {
          xvar <- ifelse (xvar < lower, xvar, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == FALSE){
        if (binary == TRUE){
          xvar <- ifelse (xvar > lower & xvar < upper, 1, 0)
        } else {
          xvar <- ifelse (xvar > lower & xvar < upper, xvar - lower, 0)
        } 
      } 
      
    }
    
  }
  
  if(is.null(spatial) == FALSE){
    cintno = data.frame(Date = cintno, spatial = climspatial)
    bintno = data.frame(Date = bintno, spatial = spatial[[1]])
    xvar = data.frame(Clim = xvar, spatial = climspatial)
    for (i in 1:length(bdate)){
      cmatrix[i, ] <- xvar[which(cintno$spatial %in% bintno$spatial[i] & cintno$Date %in% (bintno$Date[i] - c(range[2]:range[1]))), 1]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  } else {
    for (i in 1:length(bdate)){
      cmatrix[i, ] <- xvar[which(cintno %in% (bintno[i] - c(range[2]:range[1])))]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    } 
  }
  cmatrix <- as.matrix(cmatrix[, c(ncol(cmatrix):1)])
  
  if(cmissing == FALSE && any(is.na(cmatrix))){ #If the user doesn't expect missing climate data BUT there are missing data present...
    if(is.null(spatial) == FALSE){ #And spatial data has been provided...
      
      if (cinterval == "day"){ #Where a daily interval is used...
        #...save an object 'missing' with the full dates of all missing data.
        .GlobalEnv$missing <- as.Date(cintno$Date[is.na(xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      
      if (cinterval == "month"){ #Where a monthly interval is used...
        #...save an object 'missing' with the month and year of all missing data.
        .GlobalEnv$missing <- c(paste("Month:", cintno$Date[is.na(xvar$Clim)] - (floor(cintno$Date[is.na(xvar$Clim)]/12) * 12),
                                      #lubridate::month(as.Date(cintno$Date[is.na(xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cintno$Date[is.na(xvar$Clim)]/12)))
        #lubridate::year(as.Date(cintno$Date[is.na(xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if (cinterval == "week"){ #Where weekly data is used...
        #...save an object 'missing' with the week and year of all missing data.
        .GlobalEnv$missing <- c(paste("Week:", cintno$Date[is.na(xvar$Clim)] - (floor(cintno$Date[is.na(xvar$Clim)]/52) * 52),
                                      #lubridate::week(as.Date(cintno$Date[is.na(xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cintno$Date[is.na(xvar$Clim)]/52)))
        #lubridate::year(as.Date(cintno$Date[is.na(xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
    } else { #If spatial data is not provided.
      
      if (cinterval == "day"){ #Do the same for day (syntax is just a bit differen)
        .GlobalEnv$missing <- as.Date(cintno[is.na(xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if (cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", (lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(xvar)) - 1)) - (floor((lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(xvar)) - 1))/12)*12),
                                      "Year:", (floor((which(is.na(xvar)) - 1)/12) + lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))))))
      }
      if (cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", cintno[is.na(xvar)] - (floor(cintno[is.na(xvar)]/52) * 52),
                                      #ceiling(((as.numeric((as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y"))) - (floor(which(is.na(cmatrix))/nrow(cmatrix))*7)) - as.numeric(as.Date(paste("01/01/", lubridate::year(as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) + 1) / 7),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cintno[is.na(xvar)]/52)))
        #lubridate::year(as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y"))))
      }
    }
    
    #Create an error to warn about missing data
    stop(c("Climate data should not contain NA values: ", length(.GlobalEnv$missing),
           " NA value(s) found. Please add missing climate data or set cmissing to `method1` or `method2`.
           See object 'missing' for all missing climate data"))
  }
  
  #If we expect NAs and choose a method to deal with them...
  if (cmissing != FALSE && any(is.na(cmatrix))){
    
    message("Missing climate data detected. Please wait while NAs are replaced.")
    
    for(i in which(is.na(cmatrix))){
      
      if(i %% nrow(cmatrix) == 0){
        
        col <- i/nrow(cmatrix)
        row <- nrow(cmatrix)
        
      } else {
        
        col <- i%/%nrow(cmatrix) + 1
        row <- i %% nrow(cmatrix)
        
      }
      
      #If we are using method1
      if(cmissing == "method1"){
        
        #If we are using a daily interval
        if(cinterval == "day"){
          
          #For the original cdate data extract date information.
          cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"))
          
          #Extract the original biological date
          bioldate <- as.Date(bdate[row], format = "%d/%m/%Y")
          
          #Determine from this on which date data is missing
          missingdate <- bioldate - (col + range[2] - 1)
          
          #If we have spatial replication
          if(is.null(spatial) == FALSE){
            
            cdate_new$spatial <- spatial[[2]]
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)) & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)))], na.rm = T)
            
          }
          
        } else if(cinterval == "week" || cinterval == "month"){
          
          if(is.null(spatial) == FALSE){
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cintno$Date,
                                    spatial = cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- bintno$Date[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar$Clim[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)) & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cintno)
            
            #Extract the biological week number that is missing
            bioldate <- bintno[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)))], na.rm = T)
            
          }
          
        }
        
        #If the record is still an NA, there must be too many NAs. Give an error.
        if(is.na(cmatrix[row, col])){
          
          stop("Too many consecutive NAs present in the data. Consider using method2 or manually replacing NAs.")
          
        }
        
      } else if(cmissing == "method2"){
        
        if(cinterval == "day"){
          
          #For the original cdate data, determine the year, month, week and day.
          cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"),
                                  Month = lubridate::month(as.Date(cdate, format = "%d/%m/%Y")),
                                  Day   = lubridate::day(as.Date(cdate, format = "%d/%m/%Y")))
          
          #Extract the original biological date
          bioldate <- as.Date(bdate[row], format = "%d/%m/%Y")
          
          #Determine from this on which date data is missing
          missingdate <- bioldate - (col + range[2] - 1)
          
          missingdate <- data.frame(Date  = missingdate,
                                    Month = lubridate::month(missingdate),
                                    Day   = lubridate::day(missingdate))
          
          if(is.null(spatial) == FALSE){
            
            cdate_new$spatial <- spatial[[2]]
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% missingdate$Month & cdate_new$Day %in% missingdate$Day & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% missingdate$Month & cdate_new$Day %in% missingdate$Day)], na.rm = T)
            
          }
          
        } else if(cinterval == "week" || cinterval == "month"){
          
          if(is.null(spatial) == FALSE){
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cintno$Date,
                                    spatial = cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- bintno$Date[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            #Convert all dates back into year specific values
            if(cinterval == "week"){
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/52) * 52)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 52, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/52) * 52)
              missingdate <- ifelse(missingdate == 0, 52, missingdate)
              
            } else {
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/12) * 12)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 12, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/12) * 12)
              missingdate <- ifelse(missingdate == 0, 12, missingdate)
              
            }
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar$Clim[which(cdate_new$Date %in% missingdate & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cintno)
            
            #Extract the biological week number that is missing
            bioldate <- bintno[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            #Convert all dates back into year specific values
            if(cinterval == "week"){
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/52) * 52)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 52, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/52) * 52)
              missingdate <- ifelse(missingdate == 0, 52, missingdate)
              
            } else {
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/12) * 12)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 12, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/12) * 12)
              missingdate <- ifelse(missingdate == 0, 12, missingdate)
              
            }
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% missingdate)], na.rm = T)
            
          }
        }
        
        if(is.na(cmatrix[row, col])){
          
          stop("There is not enough data to replace missing values using method2. Consider dealing with NA values manually")
          
        }
        
      } else {
        
        stop("cmissing should be method1, method2 or FALSE")
        
      }
    }
  }

  #Check to see if the model contains a weight function. If so, incorporate this into the data used for updating the model.
  if("(weights)" %in% colnames(model.frame(baseline))){
    
    modeldat$model_weights  <- weights(baseline)
    #baseline <- update(baseline, yvar~., weights = model_weights, data = modeldat)
    
    call <- as.character(getCall(baseline))
    
    weight_name <- call[length(call)]
    
    names(modeldat)[length(names(modeldat))] <- weight_name
    
  }
  # if (is.null(weights(baseline)) == FALSE){
  #   if (class(baseline)[1] == "glm" && sum(weights(baseline)) == nrow(model.frame(baseline)) || attr(class(baseline), "package") == "lme4" && sum(weights(baseline)) == nrow(model.frame(baseline))){
  #   } else {
  #     
  #     modeldat$model_weights  <- weights(baseline)
  #     #baseline <- update(baseline, yvar~., weights = model_weights, data = modeldat)
  #     
  #     call <- as.character(getCall(baseline))
  #     
  #     weight_name <- call[length(call)]
  #     
  #     names(modeldat)[length(names(modeldat))] <- weight_name
  #     
  #   }
  # }
  
  #If using a mixed model, ensure that maximum likelihood is specified (because we are comparing models with different fixed effects)
  if(!is.null(attr(class(baseline), "package")) && attr(class(baseline), "package") == "lme4" && class(baseline)[1] == "lmerMod" && baseline@resp$REML == 1){
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, REML = F)
    
  }
  
  if(attr(baseline, "class")[1] == "lme" && baseline$method == "REML"){
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, method = "ML")
    
  }
  
  #If using a mixed model, ensure that maximum likelihood is specified (because we are comparing models with different fixed effects)
  if(!is.null(attr(class(baseline), "package")) && attr(class(baseline), "package") == "lme4" && class(baseline)[1] == "lmerMod" && baseline@resp$REML == 1){
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, REML = F)
    
  }
  
  if(attr(baseline, "class")[1] == "lme" && baseline$method == "REML"){
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, method = "ML")
    
  }
  
  #Create a new dummy variable called climate, that is made up all of 1s (unless it's using lme, because this will cause errors).
  if(attr(baseline, "class")[1] == "lme"){
    
    modeldat$climate <- seq(1, nrow(modeldat), 1)
    
  } else {
    
    modeldat$climate <- 1
    
  }

  if (func == "lin"){
    modeloutput <- update(baseline, yvar~. + climate, data = modeldat)
  } else if (func == "quad") {
    modeloutput <- update(baseline, yvar~. + climate + I(climate ^ 2), data = modeldat)
  } else if (func == "cub") {
    modeloutput <- update(baseline, yvar~. + climate + I(climate ^ 2) + I(climate ^ 3), data = modeldat)
  } else if (func == "log") {
    modeloutput <- update(baseline, yvar~. + log(climate), data = modeldat)
  } else if (func == "inv") {
    modeloutput <- update(baseline, yvar~. + I(climate ^ -1), data = modeldat)
  } else if (func == "centre"){
    if(centre[[2]] == "both"){
      modeldat$wgdev  <- matrix(ncol = 1, nrow = nrow(cmatrix), seq(from = 1, to = nrow(cmatrix), by = 1))
      modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(cmatrix), seq(from = 1, to = nrow(cmatrix), by = 1))
      modeloutput <- update(baseline, yvar ~. + wgdev + wgmean, data = modeldat)
    }
    if(centre[[2]] == "mean"){
      modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(cmatrix), seq(from = 1, to = nrow(cmatrix), by = 1))
      modeloutput <- update(baseline, yvar ~. + wgmean, data = modeldat)
    }
    if(centre[[2]] == "dev"){
      modeldat$wgdev  <- matrix(ncol = 1, nrow = nrow(cmatrix), seq(from = 1, to = nrow(cmatrix), by = 1))
      modeloutput     <- update(baseline, yvar ~. + wgdev, data = modeldat)
    }
  } else {
    stop("Define func")
  }

  #CREATE A FOR LOOP TO FIT DIFFERENT CLIMATE WINDOWS#
  m     <- range[2]
  n     <- duration

  #Save the best model output
  if (stat == "slope"){
    time             <- n:1
    modeldat$climate <- apply(cmatrix, 1, FUN = function(x) coef(lm(x ~ time))[2])
  } else {
    ifelse(n == 1, modeldat$climate <- cmatrix, modeldat$climate <- apply(cmatrix, 1, FUN = stat))
  }

  if (is.null(centre[[1]]) == FALSE){
    if(centre[[2]] == "both"){
      modeldat$wgdev  <- wgdev(modeldat$climate, centre[[1]])
      modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
      LocalBestModel  <- update(modeloutput, .~., data = modeldat)
    }
    if(centre[[2]] == "mean"){
      modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
      LocalBestModel     <- update(modeloutput, .~., data = modeldat)
    }
    if(centre[[2]] == "dev"){
      modeldat$wgdev  <- wgdev(modeldat$climate, centre[[1]])
      LocalBestModel  <- update(modeloutput, .~. + wgdev, data = modeldat)
    }
  } else {
    LocalBestModel <- update(modeloutput, .~., data = modeldat)
  }
  LocalData <- model.frame(LocalBestModel)
  
  #If we changed scipen at the start, switch it back to default
  if(exists("current_option")){
    
    options(scipen = current_option)
    
  }
  
  return(list(BestModel = LocalBestModel, BestModelData = LocalData, Dataset = data.frame(ModelAICc = AICc(LocalBestModel), deltaAICc = AICc(LocalBestModel) - nullmodel, WindowOpen = range[1], WindowClose = range[2], Function = func)))
}