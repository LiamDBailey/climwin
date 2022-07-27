convertdate_devel <- function(bdate, cdate, xvar, xvar2 = NULL, cinterval, type, 
                              refday, cross = FALSE, cohort, spatial, 
                              upper, lower, binary, thresholdQ = NA) {
  
  ######################################################################
  
  #INITIAL CHECKS
  
  if (cinterval != "day" && cinterval != "week" && cinterval != "month") {
    stop("cinterval should be either day, week or month")
  }
  
  ######################################################################
  
  ## FIXME: This will be fixed with #22 when bdate and cdate are df already with spatial, cohort etc!
  #Create dataframes for bdate and cdate with spatial info
  bdate_df <- data.frame(bdate = bdate, spatial = spatial[[1]])
  cdate_df <- data.frame(cdate = cdate, spatial = spatial[[2]])
  
  #Check there are no duplicate dates at any site
  #Do this BEFORE our task below, because we make a sequence of ALL dates which can be intensive for large data
  duplicates <- cdate_df %>% 
    dplyr::group_by(.data$spatial) %>% 
    dplyr::filter(dplyr::n() > length(unique(.data$cdate)))
  
  if (nrow(duplicates) > 0) {
    stop("There are duplicate climate records in the data!")      
  }
  
  #Now that we know there are no duplicates, we can expand out our climate data
  spatialcdate <- cdate_df %>% 
    dplyr::group_by(.data$spatial) %>% 
    dplyr::summarise(Date = seq(min(cdate), max(cdate), by = "days"), .groups = "drop")
  
  cdate2       <- spatialcdate$Date # Save this new date data as cdate2..
  cintno       <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 1   # atrribute daynumbers for both climate and biological data with first date in the climate data set to cintno 1
  realbintno   <- as.numeric(bdate) - min(as.numeric(cdate2)) + 1
  
  #For bdate and cdate determine earliest and latest date available for each site
  bdate_minmax <- bdate_df %>% 
    dplyr::group_by(.data$spatial) %>% 
    dplyr::summarise(min_bdate = min(.data$bdate),
                     max_bdate = max(.data$bdate), .groups = "drop")
  cdate_minmax <- cdate_df %>% 
    dplyr::group_by(.data$spatial) %>% 
    dplyr::summarise(min_cdate = min(.data$cdate),
                     max_cdate = max(.data$cdate), .groups = "drop")
  
  #Left join together
  spatial_join <- bdate_minmax %>% 
    dplyr::left_join(cdate_minmax, by = "spatial")
  
  #Identify cdate issues
  #Too late (cdate starts later than bdate)
  toolate <- spatial_join %>% 
    dplyr::filter(.data$min_cdate > .data$min_bdate)
  
  #Too early (cdate ends before bdate)
  tooearly <- spatial_join %>% 
    dplyr::filter(.data$max_cdate < .data$max_bdate)
  
  if (nrow(toolate) > 0) {
    stop(paste0("\nClimate data does not cover all years of biological data at site ", toolate$spatial,". Earliest climate data is ", toolate$min_cdate, " Earliest biological data is ", toolate$min_bdate, ". Please increase range of climate data"))
  }
  
  if (nrow(tooearly) > 0) {
    stop(paste0("\nClimate data does not cover all years of biological data at site ", tooearly$spatial ,". Latest climate data is ", tooearly$max_cdate, " Latest biological data is ", tooearly$max_bdate, ". Please increase range of climate data"))
  }
  
  ## FIXME: DO WE NEED THESE xvar2 CHECKS?
  ## Don't worry about the spatial check here. Deal with that later when we deal with xvar2 checks
  if (!is.null(xvar2)) { # if there are multiple climate variables included (i.e. for crosswin/autowin)...
    if (!is.null(spatial)) { # ...and there is spatial data provided...
      xvar2      <- data.frame(Clim = xvar2, spatial = spatial[[2]]) # ...create a new dataframe with the second climate variable and spatial info
      cdatetemp  <- data.frame(Date = cdate, spatial = spatial[[2]]) # ...do the same for date information
      split.list <- list()
      NUM <- 1
      for (i in unique(xvar2$spatial)) { # For each spatial site...
        SUB <- subset(xvar2, spatial == i) # ...subset out relevant climate data from that site...
        SUBcdate  <- subset(cdatetemp, spatial == i) # ...extract relevant date information...
        SUBcdate2 <- subset(spatialcdate, spatial == i)
        rownames(SUB) <- seq(1, nrow(SUB), 1)
        rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
        NewClim    <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)] #Work out where date information matches (i.e. there will be NAs where there is no match)
        Newspatial <- rep(i, times = length(NewClim))
        split.list[[NUM]] <- data.frame(NewClim, Newspatial) # Create a new dataframe with second climate variable and site ID
        NUM <- NUM + 1
      }
      xvar2    <- (do.call(rbind, split.list))$NewClim # Extract second climate data (with NAs where there is missing date info)
      climspatial <- (do.call(rbind, split.list))$Newspatial #Extract the new spatial data as well
    } else {
      xvar2    <- xvar2[match(cdate2, cdate)] # if there is no spatial replication, simply check for matches (i.e. include NAs where appropriate)
    }
  }
  
  ## FIXME: THIS CODE MAY NOT BE NEEDED AT ALL WHEN WE HAVE A DATAFRAME INPUT
  joined_climate <- spatialcdate %>% 
    dplyr::left_join(data.frame(Clim = xvar, cdate = cdate, spatial = spatial[[2]]), by = c("Date" = "cdate", "spatial"))
  
  xvar <- joined_climate$Clim
  climspatial <- joined_climate$spatial
  
  ## FIXME: Again, is this needed?? Code with cross will be overlooked for now
  if (!cross) { #When we are not running crosswin...
    if (cinterval == "day") { #...and we are using daily data...
      if (type == "absolute") { #..and using an absolute window...
        
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Combine biological data with cohort info (N.B. when cohort isn't provided, it will be made the year of capture by default. See code at start of slidingwin).
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for (i in unique(cohort)) { # For each cohort (i.e. year, unless specified otherwise)...
          sub                               <- subset(newdat, cohort == i) # ...subset bdate data from that cohort...
          # As we are using an absolute value, determine the biological date number as refday/minimum year in the cohort (i.e. assume they are in the previous year) - earliest cdate
          bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
        }
        
      } else {
        bintno <- realbintno #If we are using relative windows, biological date number is just the same as bdate - earliest cdate
      }
    } else if (cinterval == "week") { #...if we are using weekly data...
      
      #For daily data, we can determine upper and lower binary limits AFTER calculating cdate info.
      #However, for weekly and monthly data, we need to group our daily data into weeks or months.
      
      #If the user has specified a threshold, they may want to apply this to the daily data 
      #(i.e. if they have daily data but are using weekly/monthly to speed up analysis)
      #We have checked to see if this is the case using 'thresholdQ'.
      #If 'thresholdQ' is Y, the user wants thresholds estimated BEFORE calculated weekly/monthly data.
      
      if (!is.na(thresholdQ) && thresholdQ == "Y") {
        
        if (binary) {
          
          if (!is.na(upper) && is.na(lower)) {
            
            xvar <- ifelse(xvar > upper, 1, 0)
            
          } else if (is.na(upper) && !is.na(lower)) {
            
            xvar <- ifelse(xvar < lower, 1, 0)
            
          } else if (!is.na(upper) && !is.na(lower)) {
            
            xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)
            
          }
          
        } else {
          
          if (!is.na(upper) && is.na(lower)) {
            
            xvar <- ifelse(xvar > upper, xvar, 0)
            
          } else if (is.na(upper) && !is.na(lower)) {
            
            xvar <- ifelse(xvar < lower, xvar, 0)
            
          } else if (!is.na(upper) && !is.na(lower)) {
            
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
      newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial) %>%
        dplyr::group_by(.data$cintno, .data$spatial) %>% 
        dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("xvar"),
                                       .fns = ~mean(., nr.rm = TRUE)), .groups = "drop") %>% 
        dplyr::arrange(.data$spatial, .data$cintno)
      cintno      <- newclim$cintno #Extract week numbers
      xvar        <- newclim$xvar #Extract climate
      climspatial <- newclim$spatial #Extract site ID
      
      if (type == "absolute") { #If we are dealing with absolute windows
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Combine date numbers from biological data with the cohort (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for (i in unique(cohort)) { # For each cohort...
          sub                               <- subset(newdat, cohort == i) #...subset out biological date data
          #Turn this date info into the same values based on refday
          bintno[as.numeric(rownames(sub))] <- lubridate::week(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) + 52 * (min(lubridate::year(sub$bdate)) - min(year(cdate2)))
          #lubridate::week(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(cweek + 53 * cyear) + 1
        }
      } else { #...Otherwise just leave the biological date info as is.
        bintno <- realbintno
      }
    } else if (cinterval == "month") { # If cinterval is month instead...
      
      #Once again, check if the user wants to calculate thresholds before determining weekly/monthly means.
      
      if (!is.na(thresholdQ) && thresholdQ == "Y") {
        
        if (binary) {
          
          if (!is.na(upper) && is.na(lower)) {
            
            xvar <- ifelse(xvar > upper, 1, 0)
            
          } else if (is.na(upper) && !is.na(lower)) {
            
            xvar <- ifelse(xvar < lower, 1, 0)
            
          } else if (!is.na(upper) && !is.na(lower)) {
            
            xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)
            
          }
          
        } else {
          
          if (!is.na(upper) && is.na(lower)) {
            
            xvar <- ifelse(xvar > upper, xvar, 0)
            
          } else if (is.na(upper) && !is.na(lower)) {
            
            xvar <- ifelse(xvar < lower, xvar, 0)
            
          } else if (!is.na(upper) && !is.na(lower)) {
            
            xvar <- ifelse(xvar > lower & xvar < upper, xvar, 0)
            
          }
          
        }
      }
      
      cmonth     <- lubridate::month(cdate2) # Determine month numbers for all data...
      cyear      <- lubridate::year(cdate2) - min(lubridate::year(cdate2))
      cintno     <- cmonth + 12 * cyear
      realbintno <- lubridate::month(bdate) + 12 * (lubridate::year(bdate) - min(lubridate::year(cdate2)))
      
      newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial) %>% 
        dplyr::group_by(.data$cintno, .data$spatial) %>% 
        dplyr::summarise(xvar = mean(.data$xvar, na.rm = TRUE), .groups = "drop") %>% 
        dplyr::arrange(.data$spatial, .data$cintno)
      cintno      <- newclim$cintno #Save month, climate data and site ID
      xvar        <- newclim$xvar
      climspatial <- newclim$spatial
      if (type == "absolute") { #When using absolute windows...
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Bind biological date and cohort info (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for (i in unique(cohort)) { #For each year...
          sub <- subset(newdat, cohort == i) #Extract biological date info
          #Set the biological date the same for each cohort.
          bintno[as.numeric(rownames(sub))] <- refday[2] + 12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
        }
      } else { #Otherwise, just leave the biological date unchanged.
        bintno <- realbintno
      }
    }
  } else { # When we are running cross win...
    if (cinterval == "day") {  #And using a daily interval...
      if (type == "absolute") { #And an absolute window...
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Combine biological date and cohort info (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for (i in unique(cohort)) { #For each cohort group...
          sub <- subset(newdat, cohort == i) #...subset out data.
          #Set all records within a cohort to the same value
          bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
        }
      } else { #If using relative windows, biological date data stays the same.
        bintno <- realbintno
      }    
    } else if (cinterval == "week") { #If using weekly data...
      
      cweek      <- lubridate::week(cdate2) # atrribute week numbers for both datafiles with first week in climate data set to cintno 1
      cyear      <- lubridate::year(cdate2) - min(lubridate::year(cdate2))
      cintno     <- cweek + 53 * cyear
      cintno     <- cintno - min(cintno) + 1
      realbintno <- lubridate::month(bdate) + 53 * (year(bdate) - min(year(cdate2)))
      newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2, "spatial" = climspatial) %>% 
        dplyr::group_by(.data$cintno, .data$spatial) %>% 
        dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("xvar"),
                                       .fns = ~mean(., nr.rm = TRUE)), .groups = "drop")
      cintno      <- newclim$cintno #Save info.
      xvar        <- newclim$xvar
      xvar2       <- newclim$xvar2
      climspatial <- newclim$spatial
      if (type == "absolute") { #If using an absolute window.
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Combine biological data and cohort (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for (i in unique(cohort)) { #For each cohort...
          sub                               <- subset(newdat, cohort == i) #...subset data.
          #Create the same week value for every record in the same cohort.
          bintno[as.numeric(rownames(sub))] <- lubridate::month(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) + 53 * (min(lubridate::year(sub$bdate)) - min(year(cdate2)))
        }
      } else { #If using relative windows just keep biological date data the same.
        bintno <- realbintno
      }
    } else if (cinterval == "month") { #If using monthly data...
      
      cmonth     <- lubridate::month(cdate2) #Determine month number
      cyear      <- year(cdate2) - min(year(cdate2))
      cintno     <- cmonth + 12 * cyear
      realbintno <- lubridate::month(bdate) + 12 * (year(bdate) - min(year(cdate2)))
      
      newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2, "spatial" = climspatial) %>% 
        dplyr::group_by(.data$cintno, .data$spatial) %>% 
        dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("xvar"),
                                       .fns = ~mean(., nr.rm = TRUE)), .groups = "drop")
      
      cintno      <- newclim$cintno #Save extracted data.
      xvar        <- newclim$xvar
      xvar2       <- newclim$xvar2
      climspatial <- newclim$spatial
      
      if (type == "absolute") { #If using absolute windows.
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Extract date data and cohort (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for (i in unique(cohort)) { #For each cohort
          sub                               <- subset(newdat, cohort == i) #Subset data
          #Set each record within a cohort to have the same month
          bintno[as.numeric(rownames(sub))] <- refday[2] + 12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
        }
      } else { #If not using absolute windows then just keep data as is.
        bintno <- realbintno
      }
    }
  }
  
  #Sometimes data may have infinity variables. Always make these NAs
  xvar <- ifelse(is.infinite(xvar), NA, xvar)
  
  if (!is.null(xvar2)) { #Do the same for the second climate variable if it is present.
    
    xvar2 <- ifelse(is.infinite(xvar2), NA, xvar2)
    
  }
  
  if (!is.null(xvar2)) { #If a second climate variable is provided...
    #Return climate date, biological date and both climate variables (with spatial data included)
    return(list(cintno = data.frame(Date = cintno, spatial = climspatial),
                bintno = data.frame(Date = bintno, spatial = spatial[[1]]),
                xvar = data.frame(Clim = xvar, spatial = climspatial), 
                xvar2 = data.frame(Clim = xvar2, spatial = climspatial)))
  } else { #If there is only one climate variable
    #Return climate date, biological date and single climate variables (with spatial data included)
    return(list(cintno = data.frame(Date = cintno, spatial = climspatial),
                bintno = data.frame(Date = bintno, spatial = spatial[[1]]),
                xvar = data.frame(Clim = xvar, spatial = climspatial)))
  }
}

#'Test for a climate windows in data.
#'
#'Finds the time period when a biological variable is most strongly affected 
#'by climate. Note that climate data and biological data should be loaded as 
#'two separate objects. Both objects should contain a date column to designate
#'when the data were recorded (dd/mm/yyyy).
#'
#'Note that slidingwin allows you to test multiple possible parameters with the
#'same code (e.g. func, stat, xvar). See examples for more detail.
#'@param exclude Two values (duration and distance) which allow users
#'  to exclude short-duration long-lag climate windows from analysis (e.g., 
#'  windows with a duration of 10 days which occur over a month ago).
#'  These windows are often considered to be biologically implausible.
#'@param xvar A list object containing all climate variables of interest. 
#'  Please specify the parent environment and variable name (e.g. climate$Temp).
#'@param cdate The climate date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. climate$Date).
#'@param bdate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for model testing. 
#'  Currently known to support lm, glm, lmer, glmer and coxph objects.
#'@param range Two values signifying respectively the furthest and closest number 
#'  of time intervals (set by cinterval) back from the refday or biological record to include 
#'  in the climate window search.
#'@param stat The aggregate statistics used to analyse the climate data. Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format 
#'  function(x) (...). See FUN in \code{\link{apply}} for more detail.
#'@param func The functions used to fit the climate variable. Can be linear 
#'  ("lin"), quadratic ("quad"), cubic ("cub"), inverse ("inv") or log ("log").
#'@param type "absolute" or "relative", whether you wish the climate window to be relative
#'  (e.g. the number of days before each biological record is measured) or absolute
#'  (e.g. number of days before a set point in time).
#'@param refday If type is "absolute", the day and month respectively of the 
#'  year from which the absolute window analysis will start.
#'@param cmissing Determines what should be done if there are 
#'  missing climate data. Three approaches are possible: 
#'   - FALSE; the function will not run if missing climate data is encountered.
#'   An object 'missing' will be returned containing the dates of missing climate.
#'   - "method1"; missing climate data will be replaced with the mean climate
#'   of the preceding and following 2 records.
#'   - "method2"; missing climate data will be replaced with the mean climate
#'   of all records on the same date.
#'   
#'   Note: Other methods are possible. Users should consider those methods most
#'   appropriate for their data and apply them manually before using climwin if
#'   required.
#'
#'@param cinterval The resolution at which climate window analysis will be 
#'  conducted. May be days ("day"), weeks ("week"), or months ("month"). Note the units
#'  of parameter 'range' will differ depending on the choice
#'  of cinterval.
#'@param k The number of folds used for k-fold cross validation. By default
#'  this value is set to 0, so no cross validation occurs. Value should be a
#'  minimum of 2 for cross validation to occur.
#'@param upper Cut-off values used to determine growing degree days or positive 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, slidingwin will instead calculate an 
#'  optimal climate zone.
#'@param lower Cut-off values used to determine chill days or negative 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, slidingwin will instead calculate an 
#'  optimal climate zone.
#'@param binary TRUE or FALSE. Determines whether to use values of upper and
#'  lower to calculate binary climate data (binary = TRUE), or to use for
#'  growing degree days (binary = FALSE).
#'@param centre A list item containing:
#'  1. The variable used for mean centring (e.g. Year, Site, Individual). 
#'  Please specify the parent environment and variable name (e.g. Biol$Year).
#'  2. Whether the model should include both within-group means and variance ("both"),
#'  only within-group means ("mean"), or only within-group variance ("var").
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
#'@return Will return a list with an output for each tested set of climate
#'  window parameters. Each list item contains three objects:
#'  
#'  \itemize{
#'  \item BestModel, a model object. The strongest climate window model based on AICc. 
#'  \item BestModelData, a dataframe used to fit the strongest climate window model.
#'  \item Dataset, a dataframe with information on all fitted climate windows. 
#'  Ordered using deltaAICc, with most negative deltaAICc values first. 
#'  See \code{\link{MassOutput}} as an example.}
#'  
#'  In addition, the returned list includes an object 'combos', a summary of all
#'  tested sets of climate window parameters. 
#'@author Liam D. Bailey and Martijn van de Pol
#'@import stats
#'@import utils
#'@import graphics
#'@import lme4
#'@import purrr
#'@importFrom plyr rbind.fill
#'@importFrom lubridate weeks
#'@importFrom MuMIn AICc
#'@importFrom Matrix Matrix
#'@importFrom RcppRoll roll_mean
#'@importFrom nlme lme
#'@importFrom nlme varIdent
#'@importFrom nlme varExp  
#'@importFrom nlme lme.formula
#'@examples
#'
#'#Simple test example
#'#Create data from a subset of our test dataset
#'#Just use two years
#'biol_data <- Mass[1:2, ]
#'clim_data <- MassClimate[grep(pattern = "1979|1986", x = MassClimate$Date), ]
#'
#'output <- slidingwin(xvar = list(Temp = clim_data$Temp),
#'                     cdate = clim_data$Date, 
#'                     bdate = biol_data$Date, 
#'                     baseline = lm(Mass ~ 1, data = biol_data),
#'                     range = c(1, 0), 
#'                     type = "relative", stat = "mean", 
#'                     func = c("lin"), cmissing = FALSE, cinterval = "day")
#'
#'\dontrun{
#'
#'# Full working examples
#'
#'##EXAMPLE 1## 
#'  
#'# Test both a linear and quadratic variable climate window using datasets "Offspring"
#'# and "OffspringClimate".
#'
#'# Load data.
#'
#'data(Offspring) 
#'data(OffspringClimate)
#'
#'# Test both linear and quadratic functions with climate variable temperature
#'
#'OffspringWin <- slidingwin(xvar = list(Temp = OffspringClimate$Temperature), 
#'                           cdate = OffspringClimate$Date, 
#'                           bdate = Offspring$Date, 
#'                           baseline = glm(Offspring ~ 1, data = Offspring, family = poisson),
#'                           range = c(150, 0), 
#'                           type = "relative", stat = "mean", 
#'                           func = c("lin", "quad"), cmissing = FALSE, cinterval = "day")
#'
#'# Examine tested combinations
#'  
#'OffspringWin$combos
#'      
#'# View output for func = "lin"
#'  
#'head(OffspringWin[[1]]$Dataset) 
#'summary(OffspringWin[[1]]$BestModel)
#'  
#'# View output for func = "quad"
#'  
#'head(OffspringWin[[2]]$Dataset)
#'summary(OffspringWin[[2]]$BestModel)
#'  
#'##EXAMPLE 2##
#'  
#'# Test for an absolute climate window with both 'mean' and 'max' aggregate statistics
#'# using datasets 'Mass' and 'MassClimate'.
#'  
#'# Load data.
#'  
#'data(Mass)
#'data(MassClimate)
#'  
#'# Test an absolute window, starting 20 May (refday = c(20, 5))
#'# Test for climate windows between 100 and 0 days ago (range = c(100, 0))
#'# Test both mean and max aggregate statistics (stat = c("mean", "max"))
#'# Fit a linear term (func = "lin")
#'# Test at the resolution of days (cinterval = "day")
#'  
#'MassWin <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
#'                      bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
#'                      range = c(100, 0),
#'                      stat = c("mean", "max"), func = "lin",
#'                      type = "absolute", refday = c(20, 5),
#'                      cmissing = FALSE, cinterval = "day")
#'                        
#'# Examine tested combinations
#'  
#'MassWin$combos                      
#'  
#'# View output for mean temperature
#'  
#'head(MassWin[[1]]$Dataset)
#'summary(MassWin[[1]]$BestModel)
#'  
#'# View output for max temperature
#'  
#'head(MassWin[[2]]$Dataset)
#'summary(MassWin[[2]]$BestModel)
#'  
#'}
#'  
#'@export

devel_slidingwin <- function(exclude = NA, xvar, cdate, bdate, baseline, 
                             type, refday, stat = "mean", func = "lin", range, 
                             cmissing = FALSE, cinterval = "day", k = 0,
                             upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"),
                             spatial = NULL, cohort = NULL, CPU = 1) {
  
  #Check bdate argument
  bdate   <- check_date(bdate, arg_name = "bdate")
  cdate   <- check_date(cdate, arg_name = "cdate")
  
  ### Implementing scientific notation can cause problems because years
  ### are converted to characters in scientific notation (e.g. 2000 = "2e+3")
  ### Check options and convert scipen TEMPORARILY if needed.
  if (getOption("scipen") < 0) {
    
    current_option <- getOption("scipen")
    options(scipen = 0)
    
  }
  
  #### INITIAL CHECKS ####
  
  if (cmissing != FALSE && cmissing != "method1" && cmissing != "method2") {
    
    stop("cmissing must be FALSE, 'method1' or 'method2'.")
    
  }
  
  if (type != "absolute" && type != "relative") {
    
    stop("type must be either absolute or relative.")
    
  }
  
  if (is.null(cohort)) {
    cohort = lubridate::year(bdate) #bdate is now definitely a date due to check function
  }
  
  if (k > 0 && inherits(baseline, "coxph")) {
    stop("Sorry, cross-validation is not available yet for coxph models")
  }
  
  #If the baseline model is fitted with nlme and cross validation is requested, return an error.
  if (k > 0 && inherits(baseline, "lme")) {
    stop("Sorry, cross-validation is currently not functioning for nlme models. Consider using lme4 if possible.")
  }
  
  #If spatial information is not specified, check that there are no duplicate calendar dates.
  if (is.null(spatial) & length(unique(cdate)) < length(cdate)) {
    stop("Your cdate variable has repeated date measures. Do you have climate data from multiple sites? If so, you should specify the parameter `spatial`.")
  }
  
  #Do the spatial check at this point for now
  ## FIXME: Might not be needed once we are providing df
  spatial <- check_spatial(spatial = spatial,
                           length_b = length(bdate),
                           length_c = length(cdate))
  
  #Create a centre function that over-rides quadratics etc. when centre != NULL
  if (!is.null(centre[[1]])) {
    func = "centre"
  }
  
  #Check xvar is a list where the name of list object is the climate variable (e.g. Rain, Temp)
  if (!is.list(xvar)) {
    stop("xvar should be an object of type list")
  }
  
  if (is.null(names(xvar))) {
    numbers <- seq(1, length(xvar), 1)
    for (xname in 1:length(xvar)) {
      names(xvar)[xname] = paste("climate", numbers[xname])
    }
  }
  
  if (!is.na(upper) && !is.na(lower)) {
    combos       <- expand.grid(list(upper = upper, lower = lower))
    combos       <- combos[which(combos$upper >= combos$lower), ]
    allcombos    <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, gg = c(1:nrow(combos)), binary = binary))
    allcombos    <- cbind(allcombos, combos[allcombos$gg, ], deparse.level = 2)
    binarylevel  <- "two"
    allcombos$gg <- NULL
  } else if (!is.na(upper) && is.na(lower)) {
    allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, upper = upper, lower = lower, binary = binary))
    binarylevel <- "upper"
  } else if (is.na(upper) && !is.na(lower)) {
    allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, upper = upper, lower = lower, binary = binary))
    binarylevel <- "lower"
  } else if (is.na(upper) && is.na(lower)) {
    allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func))
    binarylevel <- "none"
  }
  
  rownames(allcombos) <- seq(1, nrow(allcombos), 1)
  
  combined <- list()
  for (combo in 1:nrow(allcombos)) {
    runs <- devel_basewin(exclude = exclude, xvar = xvar[[paste(allcombos[combo, 1])]], cdate = cdate, bdate = bdate, baseline = baseline,
                          range = range, type = paste(allcombos[combo, 2]), refday = refday, stat = paste(allcombos[combo, 3]), func = paste(allcombos[combo, 4]),
                          cmissing = cmissing, cinterval = cinterval, k = k, 
                          upper = ifelse(binarylevel == "two" || binarylevel == "upper", allcombos$upper[combo], NA),
                          lower = ifelse(binarylevel == "two" || binarylevel == "lower", allcombos$lower[combo], NA),
                          binary = paste(allcombos$binary[combo]), centre = centre, cohort = cohort,
                          spatial = spatial, CPU = CPU)
    
    class(runs) <- c("climwinfit", "list")
    
    combined[[combo]]            <- runs
    allcombos$DeltaAICc[combo]   <- round(runs$Dataset$deltaAICc[1], digits = 2)
    allcombos$WindowOpen[combo]  <- runs$Dataset$WindowOpen[1]
    allcombos$WindowClose[combo] <- runs$Dataset$WindowClose[1]
    
    if (any(grepl("climate", model.frame(baseline)))) {
      
      if (length(which("lin" == levels(allcombos$func))) > 0) {
        allcombos$betaL[combo] <- round(runs$Dataset$ModelBeta[1], digits = 2)
      }
      if (allcombos$func[1] == "centre") {
        if (centre[[2]] == "both") {
          allcombos$WithinGrpMean <- round(runs$Dataset$WithinGrpMean[1], digits = 2)
          allcombos$WithinGrpDev  <- round(runs$Dataset$WithinGrpDev[1], digits = 2)
        }
        if (centre[[2]] == "dev") {
          allcombos$WithinGrpDev  <- round(runs$Dataset$WithinGrpDev[1], digits = 2)
        }
        if (centre[[2]] == "mean") {
          allcombos$WithinGrpMean <- round(runs$Dataset$WithinGrpMean[1], digits = 2)
        }
      }
      if (length(which("quad" == levels(allcombos$func))) > 0) {
        allcombos$betaL[combo]   <- round(runs$Dataset$ModelBeta[1], digits = 2)
        allcombos$betaQ[combo]   <- round(runs$Dataset$ModelBetaQ[1], digits = 2)
      }
      if (length(which("cub" == levels(allcombos$func))) > 0) {
        allcombos$betaL[combo]   <- round(runs$Dataset$ModelBeta[1], digits = 2)
        allcombos$betaQ[combo]   <- round(runs$Dataset$ModelBetaQ[1], digits = 2)
        allcombos$betaC[combo]   <- round(runs$Dataset$ModelBetaC[1], digits = 2)
      }
      if (length(which("inv" == levels(allcombos$func))) > 0) {
        allcombos$betaInv[combo] <- round(runs$Dataset$ModelBeta[1], digits = 2)
      }
      if (length(which("log" == levels(allcombos$func))) > 0) {
        allcombos$betaLog[combo] <- round(runs$Dataset$ModelBeta[1], digits = 2)
      } 
    }
  }
  allcombos <- cbind(response = colnames(model.frame(baseline))[1], allcombos)
  combined <- c(combined, combos = list(allcombos))
  
  #If we changed scipen at the start, switch it back to default
  if (exists("current_option")) {
    
    options(scipen = current_option)
    
  }
  
  class(combined) <- c("climwin", "list")
  
  return(combined)
}

#######################################

#'Climate window analysis for randomised data
#'
#'Randomises biological data and carries out a climate window analysis. Used
#'to help determine the chance of obtaining an observed result at random.
#'@param exclude Two values (distance and duration) which allow users
#'  to exclude short-duration long-lag climate windows from analysis (e.g., 
#'  windows with a duration of 10 days which occur over a month ago).
#'  These windows are often considered to be biologically implausible.
#'@param repeats The number of times that data will be randomised and analysed 
#'  for climate windows.
#'@param window Whether randomisations are carried out for a sliding window ("sliding")
#'  or weighted window ("weighted") approach.
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
#'@param stat If window = "sliding"; The aggregate statistic used to analyse the climate data. Can 
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
#'@param k If window = "sliding"; the number of folds used for k-fold cross validation. By default
#'  this value is set to 0, so no cross validation occurs. Value should be a
#'  minimum of 2 for cross validation to occur.
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
#'@param centre A list item containing:
#'  1. The variable used for mean centring (e.g. Year, Site, Individual). 
#'  Please specify the parent environment and variable name (e.g. Biol$Year).
#'  2. Whether the model should include both within-group means and variance ("both"),
#'  only within-group means ("mean"), or only within-group variance ("dev").
#'@param weightfunc If window = "weighted"; 
#'  the distribution to be used for optimisation. Can be 
#'  either a Weibull ("W") or Generalised Extreme Value distribution ("G").
#'@param par If window = "weighted"; the shape, scale and location parameters 
#'  of the Weibull or GEV weight function used as start weight function. 
#'  For Weibull : Shape and scale parameters must be greater than 0, 
#'  while location parameter must be less than or equal to 0. 
#'  For GEV : Scale parameter must be greater than 0.
#'@param control If window = "weighted";
#'  parameters used to determine step size for the optimisation 
#'  function. Please see \code{\link{optim}} for more detail.
#'@param method If window = "weighted"; 
#'  the method used for the optimisation function. Please see 
#'  \code{\link{optim}} for more detail.

#'@param cutoff.day,cutoff.month Redundant parameters. Now replaced by refday.
#'@param furthest,closest Redundant parameters. Now replaced by range.
#'@param thresh Redundant parameter. Now replaced by binary.
#'@param cvk Redundant parameter. Now replaced by k.
#'@return Returns a dataframe containing information on the best climate
#'  window from each randomisation. See \code{\link{MassRand}} as an example.
#'@author Liam D. Bailey and Martijn van de Pol
#' @examples
#'
#'#Simple test example
#'#Create data from a subset of our test dataset
#'#Just use two years
#'biol_data <- Mass[1:2, ]
#'clim_data <- MassClimate[grep(pattern = "1979|1986", x = MassClimate$Date), ]
#'
#'rand <- randwin(repeats = 1, xvar = list(Temp = clim_data$Temp),
#'                cdate = clim_data$Date, 
#'                bdate = biol_data$Date, 
#'                baseline = lm(Mass ~ 1, data = biol_data),
#'                range = c(1, 0), 
#'                type = "relative", stat = "mean", 
#'                func = c("lin"), cmissing = FALSE, cinterval = "day")
#' 
#'\dontrun{
#'
#'# Full working examples
#'
#'## EXAMPLE 1 ##
#'
#'# Test climate windows in randomised data using a sliding window approach.
#' 
#'data(Mass)
#'data(MassClimate)
#' 
#'# Randomise data twice
#'# Note all other parameters are fitted in the same way as the climatewin function.
#' 
#'rand <- randwin(repeats = 2, window = "sliding", 
#'                xvar = list(Temp = MassClimate$Temp), 
#'                cdate = MassClimate$Date, bdate = Mass$Date,
#'                baseline = lm(Mass ~ 1, data = Mass), 
#'                range = c(100, 0),
#'                stat = "mean", func = "lin", type = "absolute", 
#'                refday = c(20, 5),
#'                cmissing = FALSE, cinterval = "day")
#'                 
#'# View output #
#' 
#'head(rand)
#'
#'## EXAMPLE 2 ##
#'
#'# Test climate windows in randomised data using a weighted window approach.
#'   
#'data(Offspring)
#'data(OffspringClimate)
#'
#'# Randomise data twice
#'# Note all other parameters are fitted in the same way as the weightwin function.
#'
#'weightrand <- randwin(repeats = 2, window = "weighted", 
#'                      xvar = list(Temp = OffspringClimate$Temperature), 
#'                      cdate = OffspringClimate$Date,
#'                      bdate = Offspring$Date,
#'                      baseline = glm(Offspring ~ 1, family = poisson, data = Offspring),
#'                      range = c(365, 0), func = "quad",
#'                      type = "relative", weightfunc = "W", cinterval = "day",
#'                      par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
#'                      method = "L-BFGS-B")
#'                    
#'# View output
#'
#'head(weightrand)
#'                           
#'        }
#'
#'@export

devel_randwin <- function(exclude = NA, repeats = 5, window = "sliding", xvar, cdate, bdate, baseline, 
                          stat, range, func, type, refday,
                          cmissing = FALSE, cinterval = "day",
                          spatial = NULL, cohort = NULL,
                          upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"), k = 0,
                          weightfunc = "W", par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)), 
                          method = "L-BFGS-B", cutoff.day = NULL, cutoff.month = NULL,
                          furthest = NULL, closest = NULL, thresh = NULL, cvk = NULL) {
  
  ### Implementing scientific notation can cause problems because years
  ### are converted to characters in scientific notation (e.g. 2000 = "2e+3")
  ### Check options and convert scipen TEMPORARILY if needed.
  if (getOption("scipen") < 0) {
    
    current_option <- getOption("scipen")
    options(scipen = 0)
    
  }
  
  #Create a centre function that over-rides quadratics etc. when centre != NULL
  if (!is.null(centre[[1]])) {
    func = "centre"
  }
  
  if (is.null(cohort)) {
    cohort = lubridate::year(as.Date(bdate, format = "%d/%m/%Y"))
  }
  
  if (!is.null(cvk)) {
    stop("Parameter 'cvk' is now redundant. Please use parameter 'k' instead.")
  }
  
  if (!is.null(thresh)) {
    stop("Parameter 'thresh' is now redundant. Please use parameter 'binary' instead.")
  }
  
  if (type == "variable" || type == "fixed") {
    stop("Parameter 'type' now uses levels 'relative' and 'absolute' rather than 'variable' and 'fixed'.")
  }
  
  if (!is.null(furthest) & !is.null(closest)) {
    stop("furthest and closest are now redundant. Please use parameter 'range' instead.")
  }
  
  if (is.null(names(xvar))) {
    numbers <- seq(1, length(xvar), 1)
    for (xname in 1:length(xvar)) {
      names(xvar)[xname] = paste("climate", numbers[xname])
    }
  }
  
  if (!is.null(cutoff.day) & !is.null(cutoff.month)) {
    stop("cutoff.day and cutoff.month are now redundant. Please use parameter 'refday' instead.")
  }
  
  if (window == "sliding") {
    
    if ((!is.na(upper) || !is.na(lower)) && (cinterval == "week" || cinterval == "month")) {
      
      thresholdQ <- readline("You specified a climate threshold using upper and/or lower and are working at a weekly or monthly scale. 
                             Do you want to apply this threshold before calculating weekly/monthly means (i.e. calculate thresholds for each day)? Y/N")
      
      thresholdQ <- toupper(thresholdQ)
      
      if (thresholdQ != "Y" & thresholdQ != "N") {
        
        thresholdQ <- readline("Please specify yes (Y) or no (N)")
        
      }
      
    }
    
    if (!is.na(upper) && !is.na(lower)) {
      combos       <- expand.grid(list(upper = upper, lower = lower))
      combos       <- combos[which(combos$upper >= combos$lower), ]
      allcombos    <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, gg = c(1:nrow(combos)), binary = binary))
      allcombos    <- cbind(allcombos, combos[allcombos$gg, ], deparse.level = 2)
      binarylevel  <- "two"
      allcombos$gg <- NULL
    } else if (!is.na(upper) && is.na(lower)) {
      allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, upper = upper, lower = lower, binary = binary))
      binarylevel <- "upper"
    } else if (is.na(upper) && !is.na(lower)) {
      allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, upper = upper, lower = lower, binary = binary))
      binarylevel <- "lower"
    } else if (is.na(upper) && is.na(lower)) {
      allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func))
      binarylevel <- "none"
    }
    
  } else if (window == "weighted") {
    
    if (!is.na(upper) && !is.na(lower)) {
      combos       <- expand.grid(list(upper = upper, lower = lower))
      combos       <- combos[which(combos$upper >= combos$lower), ]
      allcombos    <- expand.grid(list(climate = names(xvar), type = type, stat = NA, func = func, gg = c(1:nrow(combos)), binary = binary))
      allcombos    <- cbind(allcombos, combos[allcombos$gg, ], deparse.level = 2)
      binarylevel  <- "two"
      allcombos$gg <- NULL
    } else if (!is.na(upper) && is.na(lower)) {
      allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = NA, func = func, upper = upper, lower = lower, binary = binary))
      binarylevel <- "upper"
    } else if (is.na(upper) && !is.na(lower)) {
      allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = NA, func = func, upper = upper, lower = lower, binary = binary))
      binarylevel <- "lower"
    } else if (is.na(upper) && is.na(lower)) {
      allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = NA, func = func))
      binarylevel <- "none"
    }
    
  }
  
  rownames(allcombos) <- seq(1, nrow(allcombos), 1)
  # message("All combinations to be tested...")
  # message(allcombos)
  
  combined <- list()
  for (combo in 1:nrow(allcombos)) {
    for (r in 1:repeats) {
      message(c("randomization number ", r))
      
      rand.rows <- sample(length(bdate))
      
      bdateNew  <- bdate[rand.rows]
      
      if (is.null(spatial)) {
        
        spatialNew <- NULL
        
      } else {
        
        spatialNew <- list(spatial[[1]][rand.rows], spatial[[2]])
        
      }
      
      if (window == "sliding") {
        
        outputrep <- devel_basewin(exclude = exclude, xvar = xvar[[paste(allcombos[combo, 1])]], cdate = cdate, bdate = bdateNew, 
                                   baseline = baseline, range = range, stat = paste(allcombos[combo, 3]), 
                                   func = paste(allcombos[combo, 4]), type = paste(allcombos[combo, 2]),
                                   refday = refday,
                                   nrandom = repeats, cmissing = cmissing, cinterval = cinterval,
                                   upper = ifelse(binarylevel == "two" || binarylevel == "upper", allcombos$upper[combo], NA),
                                   lower = ifelse(binarylevel == "two" || binarylevel == "lower", allcombos$lower[combo], NA),
                                   binary = paste(allcombos$binary[combo]), centre = centre, k = k, spatial = spatialNew,
                                   cohort = cohort, randwin = TRUE, randwin_thresholdQ = thresholdQ)
        
        outputrep$Repeat <- r
        WeightDist <- sum(as.numeric(cumsum(outputrep$ModWeight) <= 0.95))/nrow(outputrep)
        outputrep <- outputrep[1, ]
        outputrep$WeightDist <- WeightDist
        
        if (r == 1) { 
          outputrand <- outputrep
        } else { 
          outputrand <- rbind(outputrand, outputrep)
        }
        
      } else if (window == "weighted") {
        
        rep <- weightwin(xvar = xvar[[paste(allcombos[combo, 1])]], cdate = cdate, bdate = bdateNew, 
                         baseline = baseline, range = range, func = paste(allcombos[combo, 4]), 
                         type = paste(allcombos[combo, 2]), refday = refday,
                         nrandom = repeats, cinterval = cinterval,
                         centre = centre, spatial = spatialNew,
                         cohort = cohort, weightfunc = weightfunc, par = par, 
                         control = control, method = method)
        
        outputrep <- rep$WeightedOutput
        outputrep$Repeat <- r
        
        if (r == 1) { 
          outputrand <- outputrep 
        } else { 
          outputrand <- rbind(outputrand, outputrep)
        }
      }
      
      rm(outputrep)
      if (r == repeats) {          
        outputrand                   <- as.data.frame(outputrand)
        combined[[combo]]            <- outputrand
      }
    }
  }
  allcombos <- cbind(response = colnames(model.frame(baseline))[1], allcombos)
  combined <- c(combined, combos = list(allcombos))
  
  #If we changed scipen at the start, switch it back to default
  if (exists("current_option")) {
    
    options(scipen = current_option)
    
  }
  
  return(combined)
  
}

################################

devel_basewin <- function(exclude, xvar, cdate, bdate, baseline, range, 
                          type, stat = "mean", func = "lin", refday,
                          cmissing = FALSE, cinterval = "day", nrandom = 0, k = 0,
                          spatial, upper = NA, lower = NA, binary = FALSE, scale = FALSE, centre = list(NULL, "both"),
                          cohort = NULL, randwin = FALSE, randwin_thresholdQ, CPU = 1) {
  
  message("Initialising, please wait...")
  
  options(warn = 0, nwarnings = 1)
  
  ##########################################################################
  
  #### INITIAL CHECKS ####
  
  ## FIXME: THIS WON'T BE NEEDED ONCE WE ADD CHECKS IN ALL PARENT FUNCS
  #Check that climate date data is in the correct date format
  if (all(is.na(as.Date(cdate, format = "%d/%m/%Y")))) {
    
    stop("cdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  #Check that biological date data is in the correct date format
  if (all(is.na(as.Date(bdate, format = "%d/%m/%Y")))) {
    
    stop("bdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  #If the user wants to use slope with log or inverse...
  if (stat == "slope" && func == "log" || stat == "slope" && func == "inv") {
    
    #Return an error...
    stop("stat = slope cannot be used with func = log or inv as negative values may be present")
  }
  
  #If the user has centred the data
  if (!is.null(centre[[1]])) {
    
    #But they haven't specified whether they want within and/or between group...
    if (centre[[2]] != "both" && centre[[2]] != "dev" && centre[[2]] != "mean") {
      
      #Return an error...
      stop("Please set centre to one of 'both', 'dev', or 'mean'. See help file for details.")
    }
  }
  
  ##########################################################################
  
  #### DEALING WITH THRESHOLDS ####
  
  #By default, don't ask a question about how you want to apply the thresholds.
  thresholdQ <- "N"
  
  #If you are not using randwin, then ask the question about how thresholds should be applied.
  if (!randwin) {
    
    #If you have specified an upper or lower value for which a threshold should be applied and you are not working at a daily scale...
    if ((!is.na(upper) || !is.na(lower)) && (cinterval == "week" || cinterval == "month")) {
      
      #Determine whether the user wants to: 1. apply the threshold at the daily level BEFORE estimating monthly/weekly mean (i.e. daily data is binary but monthly/weekly is not)
      #                                     2. apply the threshold AFTER applying monthly/weekly mean (i.e. daily data is non-binary, monthly/weekly is)
      thresholdQ <- readline("You specified a climate threshold using upper and/or lower and are working at a weekly or monthly scale. 
                           Do you want to apply this threshold before calculating weekly/monthly means (i.e. calculate thresholds for each day)? Y/N")
      
      #Convert to upper case for logical conditions
      thresholdQ <- toupper(thresholdQ)
      
      #If they didn't specify a Y/N answer, ask again.
      if (thresholdQ != "Y" & thresholdQ != "N") {
        
        thresholdQ <- readline("Please specify yes (Y) or no (N)")
        
      }
      
    }
    
    #If they are using randwin, make a decision based on earlier specified argument.    
  } else {
    
    if ((!is.na(upper) || !is.na(lower)) && (cinterval == "week" || cinterval == "month")) {
      
      thresholdQ <- randwin_thresholdQ
      
    }
    
  }
  
  ##########################################################################
  
  #### SPATIAL REPLICATION ####
  
  ##FIXME: This is total crap, but should change once we have df input
  #Create a data frame with the biological data and its spatial information.  
  sample.size <- 0
  data <- data.frame(bdate = bdate, spatial = spatial[[1]], cohort = as.factor(cohort))
  
  #For each cohort (usually year, but may also be breeding period)...  
  for (i in unique(data$cohort)) {
    
    #Take a subset of the data for that cohort...
    sub <- subset(data, cohort = i)
    #Relevel spatial...
    sub$spatial <- factor(sub$spatial)
    #Add 1 to sample size for every site in each cohort.
    sample.size <- sample.size + length(levels(sub$spatial))
    
  }
  
  ##########################################################################
  
  #PROCESS DATA INTO CLIMWIN FORMAT
  
  #Duration of searching period is the difference between the two levels or range + 1 (e.g. also including day 0)
  duration  <- (range[1] - range[2]) + 1
  #Determine maximum number of potential windows to fit.
  maxmodno  <- (duration * (duration + 1))/2
  
  #If the user has provided exclude data (i.e. there are certain windows that will not be considered)...
  if (length(exclude) == 2) {
    
    #Remove these excluded windows from the estimate of maximum number of windows.
    maxmodno  <- maxmodno - exclude[1] * (duration - exclude[2] - 1) + (exclude[1] - 1) * exclude[1] / 2
    
  }
  
  #If slope stat is used...
  if (stat == "slope") { 
    
    #Adjust number of models...
    ifelse(is.na(exclude[2]),  maxmodno  <- maxmodno - duration, maxmodno  <- maxmodno - exclude[2] - 1)
    
  }
  
  #Convert date information in to numbers and apply absolute window info (if appropriate)
  #This code creates a new climate dataframe with continuous daynumbers, leap days are not a problem
  converted_dates <- convertdate_devel(bdate = bdate, cdate = cdate, xvar = xvar, 
                                       cinterval = cinterval, type = type, 
                                       refday = refday, cohort = cohort, spatial = spatial, 
                                       binary = binary, upper = upper, lower = lower, thresholdQ = thresholdQ)
  
  clim <- converted_dates$cintno %>% 
    dplyr::group_by(.data$spatial) %>% 
    dplyr::summarise(min_cintno = min(.data$Date),
                     max_cintno = max(.data$Date))
  biol <- converted_dates$bintno %>% 
    dplyr::group_by(.data$spatial) %>% 
    dplyr::summarise(min_bintno = min(.data$Date) - range[1],
                     max_bintno = max(.data$Date) - range[2])
  
  join_intno <- clim %>% 
    dplyr::left_join(biol, by = c("spatial"))
  
  #Check if any are too late once accounting for range
  #i.e. the climate data starts AFTER the first date required for our given range
  toolate <- join_intno %>% 
    dplyr::filter(min_bintno < min_cintno)
  
  #Check if any end too early once accounting for range
  #i.e. the climate data finished before the latest date required for our given range
  tooearly <- join_intno %>% 
    dplyr::filter(max_bintno > max_cintno)
  
  if (nrow(toolate) > 0) {
    stop(paste0("At site ", toolate$spatial, " you do not have enough climate data to search ", range[1], " ", cinterval, "s back. Please adjust the value of range or add additional climate data."))
  }
  
  if (nrow(tooearly) > 0) {
    stop(paste0("At site ", tooearly$spatial, " you need more recent climate data. The most recent climate data is from ", tooearly$max_cintno, " while the most recent biological data is from ", tooearly$max_bintno))
  }
  
  modno     <- 1  #Create a model number variable that will count up during the loop#
  cmatrix   <- matrix(ncol = (duration), nrow = length(bdate))  # matrix that stores the weather data for variable or fixed windows
  
  modlist   <- list()   # dataframes to store ouput
  if (class(baseline)[1] == "lme") {
    
    baseline  <- update(baseline, .~.)
    
  } else {
    
    baseline  <- my_update(baseline, .~.) 
    
  }
  nullmodel <- MuMIn::AICc(baseline)
  modeldat  <- model.frame(baseline)
  
  #If there are any variables that have been scaled (i.e. using scale(x)) we need to change the model data to remove the scale argument.
  #If not, the model.frame output has a column scale(x) but the model during update is looking for x.
  if (any(grepl("scale\\(|\\)", colnames(modeldat)))) {
    
    colnames(modeldat) <- gsub("scale\\(|\\)", "", colnames(modeldat))
    
  }
  
  if (attr(baseline, "class")[1] == "lme") { #If model is fitted using nlme package
    
    if (!is.null(baseline$modelStruct$varStruct) && !is.null(attr(baseline$modelStruct$varStruct, "groups"))) { #If a custom variance structure has been included and it has multiple levels...
      
      modeldat <- cbind(modeldat, attr(baseline$modelStruct$varStruct, "groups")) #Add the variables from this variance structure to the model data used for updating models.
      
      colnames(modeldat)[ncol(modeldat)] <- strsplit(x = as.character(attr(baseline$modelStruct$varStruct, "formula"))[2], split = " | ")[[1]][3] #Make the names equivalent.
      
    }
    
    #If a complex variance structure hasn't been provided...
    
    non_rand <- ncol(modeldat) #Determine the number of non-random variables from the original model data.
    
    modeldat <- cbind(modeldat, baseline$data[, colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")]]) #Include random effects (i.e. those that AREN'T FIXED)
    
    colnames(modeldat)[-(1:non_rand)] <- colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")] #Make sure these columns are named correctly
    
  }
  
  #If using coxph models, adjust naming of variables to deal with frailty terms.
  if (class(baseline)[length(class(baseline))] == "coxph" && grepl("frailty\\(", colnames(modeldat)[ncol(modeldat)])) {
    colnames(modeldat)[ncol(modeldat)] <- gsub("frailty\\(", "", colnames(modeldat)[ncol(modeldat)])
    colnames(modeldat)[ncol(modeldat)] <- gsub("\\)", "", colnames(modeldat)[ncol(modeldat)])
  }
  
  #Rename response variable as yvar (this way the name is standardised and can be easily called)
  colnames(modeldat)[1] <- "yvar"
  
  #If user has provided some variable for mean centring change the func to centre (i.e. you can't do mean centring and quadratic/cubic)
  if (!is.null(centre[[1]])) {
    func <- "centre"
  }
  
  #Determine length of data provided for response variable.
  ifelse(class(baseline)[length(class(baseline))] == "coxph", leng <- length(modeldat$yvar[,1]), leng <- length(modeldat$yvar))
  #If there are NAs present in the biological data, provide an error.
  if (leng != length(bdate)) {
    stop("NA values present in biological response. Please remove NA values")
  }
  
  if (cinterval == "day" || (!is.na(thresholdQ) && thresholdQ == "N")) { #If dealing with daily data OR user chose to apply threshold later...
    
    if (!is.na(upper) && is.na(lower)) { #...and an upper bound is provided...
      if (binary) { #...and we want data to be binary (i.e. it's above the value or it's not)
        converted_dates$xvar$Clim <- ifelse(converted_dates$xvar$Clim > upper, 1, 0) #Then turn climate data into binary data.
      } else { #Otherwise, if binary is not true, simply make all data below the upper limit into 0.
        converted_dates$xvar$Clim <- ifelse(converted_dates$xvar$Clim > upper, converted_dates$xvar$Clim, 0)
      }
    }
    
    if (!is.na(lower) && is.na(upper)) { #If a lower limit has been provided, do the same.
      if (binary) {
        converted_dates$xvar$Clim <- ifelse(converted_dates$xvar$Clim < lower, 1, 0)
      } else {
        converted_dates$xvar$Clim <- ifelse(converted_dates$xvar$Clim < lower, converted_dates$xvar$Clim, 0)
      }
    }
    
    if (!is.na(lower) && !is.na(upper)) { #If both an upper and lower limit are provided, do the same.
      if (binary) {
        converted_dates$xvar$Clim <- ifelse(converted_dates$xvar$Clim > lower && converted_dates$xvar$Clim < upper, 1, 0)
      } else {
        converted_dates$xvar$Clim <- ifelse(converted_dates$xvar$Clim > lower && converted_dates$xvar$Clim < upper, converted_dates$xvar$Clim - lower, 0)
      } 
    }
  }
  
  ## FIXME: Seems inefficient!! Need to look at this more
  ## In fact, can we even remove the cmatrix?!
  for (i in 1:length(bdate)) { #For each biological record we have...
    #Take a row in the empty matrix and add climate data from the correct site and over the full date range chosen by the user.
    cmatrix[i, ] <- converted_dates$xvar[which(converted_dates$cintno$spatial %in% converted_dates$bintno$spatial[i] & converted_dates$cintno$Date %in% (converted_dates$bintno$Date[i] - c(range[2]:range[1]))), 1]  
  }
  
  #Make sure the order is correct, so most recent climate data is in the earliest column
  cmatrix <- as.matrix(cmatrix[, c(ncol(cmatrix):1)])
  
  if (!cmissing & any(is.na(cmatrix))) { #If the user doesn't expect missing climate data BUT there are missing data present...
    
    if (cinterval == "day") { #Where a daily interval is used...
      #...save an object 'missing' with the full dates of all missing data.
      .GlobalEnv$missing <- as.Date(converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
    }
    
    if (cinterval == "month") { #Where a monthly interval is used...
      #...save an object 'missing' with the month and year of all missing data.
      .GlobalEnv$missing <- c(paste("Month:", converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)] - (floor(converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)]/12) * 12),
                                    #lubridate::month(as.Date(converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                    "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)]/12)))
      #lubridate::year(as.Date(converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
    }
    if (cinterval == "week") { #Where weekly data is used...
      #...save an object 'missing' with the week and year of all missing data.
      .GlobalEnv$missing <- c(paste("Week:", converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)] - (floor(converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)]/52) * 52),
                                    #lubridate::week(as.Date(converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                    "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)]/52)))
      #lubridate::year(as.Date(converted_dates$cintno$Date[is.na(converted_dates$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
    }
    
    #Create an error to warn about missing data
    stop(c("Climate data should not contain NA values: ", length(.GlobalEnv$missing),
           " NA value(s) found. Please add missing climate data or set cmissing to `method1` or `method2`.
           See object 'missing' for all missing climate data"))
  }
  
  #If we expect NAs and choose a method to deal with them...
  if (cmissing && any(is.na(cmatrix))) {
    
    message("Missing climate data detected. Please wait while NAs are replaced.")
    
    for (i in which(is.na(cmatrix))) {
      
      #Determine the column and row location...
      if (i %% nrow(cmatrix) == 0) {
        
        col <- i/nrow(cmatrix)
        row <- nrow(cmatrix)
        
      } else {
        
        col <- i%/%nrow(cmatrix) + 1
        row <- i %% nrow(cmatrix)
        
      }
      
      
      #If we are using method1
      if (cmissing == "method1") {
        
        #If we are using a daily interval
        if (cinterval == "day") {
          
          #For the original cdate data extract date information.
          cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"))
          
          #Extract the original biological date
          bioldate <- as.Date(bdate[row], format = "%d/%m/%Y")
          
          #Determine from this on which date data is missing
          missingdate <- bioldate - (col + range[2] - 1)
          
          #If we have spatial replication
          if (!is.null(spatial)) {
            
            cdate_new$spatial <- spatial[[2]]
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)) & cdate_new$spatial %in% siteID)], na.rm = TRUE)
            
          } else {
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)))], na.rm = TRUE)
            
          }
          
        } else if (cinterval == "week" || cinterval == "month") {
          
          if (!is.null(spatial)) {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = converted_dates$cintno$Date,
                                    spatial = converted_dates$cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- converted_dates$bintno$Date[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(converted_dates$xvar$Clim[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)) & cdate_new$spatial %in% siteID)], na.rm = TRUE)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = converted_dates$cintno)
            
            #Extract the biological week number that is missing
            bioldate <- converted_dates$bintno[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            cmatrix[row, col] <- mean(converted_dates$xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)))], na.rm = TRUE)
            
          }
          
        }
        
        #If the record is still an NA, there must be too many NAs. Give an error.
        if (is.na(cmatrix[row, col])) {
          
          stop("Too many consecutive NAs present in the data. Consider using method2 or manually replacing NAs.")
          
        }
        
      } else if (cmissing == "method2") {
        
        if (cinterval == "day") {
          
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
          
          if (!is.null(spatial)) {
            
            cdate_new$spatial <- spatial[[2]]
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% missingdate$Month & cdate_new$Day %in% missingdate$Day & cdate_new$spatial %in% siteID)], na.rm = TRUE)
            
          } else {
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% missingdate$Month & cdate_new$Day %in% missingdate$Day)], na.rm = TRUE)
            
          }
          
        } else if (cinterval == "week" || cinterval == "month") {
          
          if (!is.null(spatial)) {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = converted_dates$cintno$Date,
                                    spatial = converted_dates$cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- converted_dates$bintno$Date[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            #Convert all dates back into year specific values
            if (cinterval == "week") {
              
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
            
            cmatrix[row, col] <- mean(converted_dates$xvar$Clim[which(cdate_new$Date %in% missingdate & cdate_new$spatial %in% siteID)], na.rm = TRUE)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = converted_dates$cintno)
            
            #Extract the biological week number that is missing
            bioldate <- converted_dates$bintno[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            #Convert all dates back into year specific values
            if (cinterval == "week") {
              
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
            
            cmatrix[row, col] <- mean(converted_dates$xvar[which(cdate_new$Date %in% missingdate)], na.rm = TRUE)
            
          }
        }
        
        if (is.na(cmatrix[row, col])) {
          
          stop("There is not enough data to replace missing values using method2. Consider dealing with NA values manually")
          
        }
        
      } else {
        
        stop("cmissing should be method1, method2 or FALSE")
        
      }
    }
  }
  
  #Check to see if the model contains a weight function. If so, incorporate this into the data used for updating the model.
  if ("(weights)" %in% colnames(model.frame(baseline))) {
    
    modeldat$model_weights  <- weights(baseline)
    #baseline <- update(baseline, yvar~., weights = model_weights, data = modeldat)
    
    call <- as.character(getCall(baseline))
    
    weight_name <- call[length(call)]
    
    names(modeldat)[length(names(modeldat))] <- weight_name
    
  }
  
  #If using a mixed model, ensure that maximum likelihood is specified (because we are comparing models with different fixed effects)
  if (!is.null(attr(class(baseline), "package")) && attr(class(baseline), "package") == "lme4" && class(baseline)[1] == "lmerMod" && baseline@resp$REML == 1) {
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, REML = F)
    
  } else if (attr(baseline, "class")[1] == "lme" && baseline$method == "REML") {
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, method = "ML")
    
  } else {
    
    baseline <- update(baseline, yvar ~., data = modeldat)
    
  }
  
  #Save AICc value of baseline model
  #This is used to determine deltaAICc, we only want to estimate it once.
  ## FIXME: Replace with logLik (Issue #27)
  if (inherits(baseline, c("HLfit"))) {
    #For spaMM, use marginal AIC
    baselineAIC <- as.numeric(AIC(baseline, verbose = FALSE)[1])
  } else {
    baselineAIC <- AICc(baseline)
  }
  
  #If there are no variables in the baseline model called climate (i.e. the user has not specified more complex role for climate in the model, such as an interaction or random effects.)
  if (all(!grepl("climate", colnames(modeldat)))) {
    
    #Create a new dummy variable called climate, that is made up all of 1s (unless it's using lme, because this will cause errors).
    if (attr(baseline, "class")[1] == "lme") {
      
      modeldat$climate <- seq(1, nrow(modeldat), 1)
      
    } else {
      
      modeldat$climate <- 1
      
    }
    
    #Update the baseline model to include this new variable in the required format (e.g. linear, quadratic etc.)
    if (func == "lin") {
      modeloutput <- update(baseline, yvar~. + climate, data = modeldat)
    } else if (func == "quad") {
      modeloutput <- update(baseline, yvar~. + climate + I(climate ^ 2), data = modeldat)
    } else if (func == "cub") {
      modeloutput <- update(baseline, yvar~. + climate + I(climate ^ 2) + I(climate ^ 3), data = modeldat)
    } else if (func == "log") {
      modeloutput <- update(baseline, yvar~. + log(climate), data = modeldat)
    } else if (func == "inv") {
      modeloutput <- update(baseline, yvar~. + I(climate ^ -1), data = modeldat)
    } else if (func == "centre") {
      if (centre[[2]] == "both") {
        modeldat$wgdev  <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeloutput <- update(baseline, yvar ~. + wgdev + wgmean, data = modeldat)
      }
      if (centre[[2]] == "mean") {
        modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeloutput <- update(baseline, yvar ~. + wgmean, data = modeldat)
      }
      if (centre[[2]] == "dev") {
        modeldat$wgdev  <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeloutput <- update(baseline, yvar ~. + wgdev, data = modeldat)
      }
    } else {
      stop("Define func")
    }
    
  } else {
    
    #If climate has already been provided, simply update the model with the new term yvar.
    modeloutput <- update(baseline, yvar ~., data = modeldat)
    
    coef_data <- list()
    
  }
  
  #If cross validation has been specified...
  if (k >= 1) {
    modeldat$K <- sample(seq(from = 1, to = length(modeldat$climate), by = 1) %% k + 1)
  }   # create labels k-fold crossvalidation
  
  #Determine all possible combos of days
  all_windows <- expand.grid(WindowOpen = range[2]:range[1],
                             WindowClose = range[2]:range[1])
  #Subset only those where WindowOpen is >= WindowClose
  all_windows <- subset(all_windows, all_windows$WindowClose <= all_windows$WindowOpen)
  #Determine duration of windows
  all_windows$duration <- all_windows$WindowOpen - all_windows$WindowClose
  all_windows$i        <- 1:nrow(all_windows)
  
  #If CPU > 1 we are parallel processing
  if (CPU > 1) {
    
    message("Parallel processing...")
    
    future::plan(future::multisession, workers = CPU)
    
    #Now, loop through every row and run models
    modlist <- furrr::future_pmap(.l = all_windows,
                                  .f = function(i, WindowOpen, WindowClose, duration) {
      
      #Start index is the column in the cmatrix that should be extracted
      #This is the actual number of days in the past - range[2]
      #column 1 in cmatrix is the first day that windows would be built
      start_index <- WindowClose - range[2] + 1
      end_index   <- WindowOpen - range[2] + 1
      
      new_climate <- cmatrix[, start_index:end_index]
      
      if (duration == 0) {
        
        modeldat$climate <- new_climate
        
      } else {
        
        modeldat$climate <- apply(new_climate, 1, FUN = stat)
        
      }
      
      modeloutput <- tryCatch({
        
        eval(getCall(modeloutput))
        
      }, error = function(e) {
        
        baseline
        
      })
      
      ## FIXME: Replace with logLik (Issue #27)
      if (inherits(modeloutput, c("HLfit"))) {
        #For spaMM, use marginal AIC
        ModelAICc <- as.numeric(AIC(modeloutput, verbose = FALSE)[1])
      } else {
        ModelAICc <- AIC(modeloutput)
      }
      
      
      if (inherits(modeloutput, c("lm", "coxph"))) {
        coef_fn <- coef
      } else if (inherits(modeloutput, c("lme4", "HLfit"))) {
        coef_fn <- fixef
      }
      
      #Extract fixed effects coefficients
      coef_output <- coef_fn(modeloutput)
      
      #Filter only intercept and coefs with climate
      coef_output_filter <- coef_output[grepl(names(coef_output), pattern = "Intercept|climate")]
      
      #Add in the deltaAICc
      coef_output_filter <- append(coef_output_filter, values = c("ModelAICc" = ModelAICc))
      
      #Transpose to named matrix
      coef_output_t <- t(coef_output_filter)
      # 
      # 
      # modlist <- data.frame(deltaAICc = ModelAICc - baselineAIC,
      #                       ModelAICc = ModelAICc,
      #                       WindowOpen = WindowOpen,
      #                       WindowClose = WindowClose)
      # 
      # if (class(modeloutput)[1] %in% c("lm", "lmerMod")) {
      #   
      #   mod_summary <- coef(summary(modeloutput))
      #   
      #   modlist$ModelInt <- mod_summary[1, "Estimate"]
      #   
      #   #If a model did not fit a climate term (i.e. climate was all 0)
      #   #Just return NA
      #   if (!"climate" %in% rownames(mod_summary)) {
      #     
      #     modlist$ModelBeta  <- NA
      #     modlist$Std.Error  <- NA
      #     modlist$ModelBetaQ <- NA
      #     modlist$ModelBetaC <- NA
      #     
      #     if (func == "quad") {
      #       
      #       modlist$Std.ErrorQ <- NA
      #       
      #     }
      #     
      #     if (func == "cub") {
      #       
      #       modlist$Std.ErrorQ <- NA
      #       modlist$Std.ErrorC <- NA
      #       
      #     }
      #     
      #   } else {
      #     
      #     if (func == "quad") {
      #       
      #       modlist$ModelBeta  <- mod_summary[nrow(mod_summary) - 1, "Estimate"]
      #       modlist$Std.Error  <- mod_summary[nrow(mod_summary) - 1, "Std. Error"]
      #       modlist$ModelBetaQ <- mod_summary[nrow(mod_summary), "Estimate"]
      #       modlist$Std.ErrorQ <- mod_summary[nrow(mod_summary), "Std. Error"]
      #       modlist$ModelBetaC <- NA
      #       
      #     } else if (func == "cub") {
      #       
      #       modlist$ModelBeta  <- mod_summary[nrow(mod_summary) - 2, "Estimate"]
      #       modlist$Std.Error  <- mod_summary[nrow(mod_summary) - 2, "Std. Error"]
      #       modlist$ModelBetaQ <- mod_summary[nrow(mod_summary) - 1, "Estimate"]
      #       modlist$Std.ErrorQ <- mod_summary[nrow(mod_summary) - 1, "Std. Error"]
      #       modlist$ModelBetaC <- mod_summary[nrow(mod_summary), "Estimate"]
      #       modlist$Std.ErrorC <- mod_summary[nrow(mod_summary), "Std. Error"]
      #       
      #     } else {
      #       
      #       modlist$ModelBeta  <- mod_summary[nrow(mod_summary), "Estimate"]
      #       modlist$Std.Error  <- mod_summary[nrow(mod_summary), "Std. Error"]
      #       modlist$ModelBetaQ <- NA
      #       modlist$ModelBetaC <- NA
      #       
      #     }
      #     
      #   }
      #   
      # }
      
      setTxtProgressBar(pb, i)
      
      return(coef_output_t)
      
    },
    .progress = TRUE,
    .options = furrr::furrr_options(globals = c("modeloutput", "range", "cmatrix", "modeldat", "baseline")))
  
  } else {
   
    #Create the progress bar
    pb <- txtProgressBar(min = 1, max = nrow(all_windows), style = 3, char = "|")
    
    #Now, loop through every row and run models
    modlist <- purrr::pmap(.l = all_windows, .f = function(i, WindowOpen, WindowClose, duration) {
      
      #Start index is the column in the cmatrix that should be extracted
      #This is the actual number of days in the past - range[2]
      #column 1 in cmatrix is the first day that windows would be built
      start_index <- WindowClose - range[2] + 1
      end_index   <- WindowOpen - range[2] + 1
      
      new_climate <- cmatrix[, start_index:end_index]
      
      if (duration == 0) {
        
        modeldat$climate <- new_climate
        
      } else {
        
        modeldat$climate <- apply(new_climate, 1, FUN = stat)
        
      }
      
      modeloutput <- tryCatch({
        
        eval(getCall(modeloutput))
        
      }, error = function(e) {
        
        baseline
        
      })
      
      ## FIXME: Replace with logLik (Issue #27)
      if (inherits(modeloutput, c("HLfit"))) {
        #For spaMM, use marginal AIC
        ModelAICc <- as.numeric(AIC(modeloutput, verbose = FALSE)[1])
      } else {
        ModelAICc <- AIC(modeloutput)
      }
      
      
      if (inherits(modeloutput, c("lm", "coxph"))) {
        coef_fn <- coef
      } else if (inherits(modeloutput, c("lme4", "HLfit"))) {
        coef_fn <- fixef
      }
      
      #Extract fixed effects coefficients
      coef_output <- coef_fn(modeloutput)
      
      #Filter only intercept and coefs with climate
      coef_output_filter <- coef_output[grepl(names(coef_output), pattern = "Intercept|climate")]
      
      #Add in the deltaAICc
      coef_output_filter <- append(coef_output_filter, values = c("ModelAICc" = ModelAICc))
      
      #Transpose to named matrix
      coef_output_t <- t(coef_output_filter)
      # 
      # 
      # modlist <- data.frame(deltaAICc = ModelAICc - baselineAIC,
      #                       ModelAICc = ModelAICc,
      #                       WindowOpen = WindowOpen,
      #                       WindowClose = WindowClose)
      # 
      # if (class(modeloutput)[1] %in% c("lm", "lmerMod")) {
      #   
      #   mod_summary <- coef(summary(modeloutput))
      #   
      #   modlist$ModelInt <- mod_summary[1, "Estimate"]
      #   
      #   #If a model did not fit a climate term (i.e. climate was all 0)
      #   #Just return NA
      #   if (!"climate" %in% rownames(mod_summary)) {
      #     
      #     modlist$ModelBeta  <- NA
      #     modlist$Std.Error  <- NA
      #     modlist$ModelBetaQ <- NA
      #     modlist$ModelBetaC <- NA
      #     
      #     if (func == "quad") {
      #       
      #       modlist$Std.ErrorQ <- NA
      #       
      #     }
      #     
      #     if (func == "cub") {
      #       
      #       modlist$Std.ErrorQ <- NA
      #       modlist$Std.ErrorC <- NA
      #       
      #     }
      #     
      #   } else {
      #     
      #     if (func == "quad") {
      #       
      #       modlist$ModelBeta  <- mod_summary[nrow(mod_summary) - 1, "Estimate"]
      #       modlist$Std.Error  <- mod_summary[nrow(mod_summary) - 1, "Std. Error"]
      #       modlist$ModelBetaQ <- mod_summary[nrow(mod_summary), "Estimate"]
      #       modlist$Std.ErrorQ <- mod_summary[nrow(mod_summary), "Std. Error"]
      #       modlist$ModelBetaC <- NA
      #       
      #     } else if (func == "cub") {
      #       
      #       modlist$ModelBeta  <- mod_summary[nrow(mod_summary) - 2, "Estimate"]
      #       modlist$Std.Error  <- mod_summary[nrow(mod_summary) - 2, "Std. Error"]
      #       modlist$ModelBetaQ <- mod_summary[nrow(mod_summary) - 1, "Estimate"]
      #       modlist$Std.ErrorQ <- mod_summary[nrow(mod_summary) - 1, "Std. Error"]
      #       modlist$ModelBetaC <- mod_summary[nrow(mod_summary), "Estimate"]
      #       modlist$Std.ErrorC <- mod_summary[nrow(mod_summary), "Std. Error"]
      #       
      #     } else {
      #       
      #       modlist$ModelBeta  <- mod_summary[nrow(mod_summary), "Estimate"]
      #       modlist$Std.Error  <- mod_summary[nrow(mod_summary), "Std. Error"]
      #       modlist$ModelBetaQ <- NA
      #       modlist$ModelBetaC <- NA
      #       
      #     }
      #     
      #   }
      #   
      # }
      
      setTxtProgressBar(pb, i)
      
      return(coef_output_t)
      
    })
     
  }
  
  all_windows$i <- NULL
  
  modlist <- cbind(all_windows, do.call(rbind, modlist))
  modlist$deltaAICc <- modlist$ModelAICc - baselineAIC
  modlist <- modlist[order(modlist$deltaAICc), ]
  
  #Save the best model
  start_index <- modlist$WindowClose[1] - range[2] + 1
  end_index   <- modlist$WindowOpen[1] - range[2] + 1
  
  if (start_index == end_index) {
    
    modeldat$climate <- cmatrix[, start_index:end_index]
    
  } else {
    
    modeldat$climate <- apply(cmatrix[, start_index:end_index], 1, FUN = stat)
    
  }
  
  LocalModel <- tryCatch({
    
    update(modeloutput, .~., data = modeldat)
    
  }, error = function(e) {
    
    update(baseline, yvar~., data = modeldat)
    
  })
  
  modlist$Function     <- func
  modlist$Furthest     <- range[1]
  modlist$Closest      <- range[2]
  modlist$Statistics   <- stat
  modlist$Type         <- type
  modlist$K            <- k
  modlist$ModWeight    <- (exp(-0.5 * modlist$deltaAICc)) / sum(exp(-0.5 * modlist$deltaAICc))
  modlist$sample.size  <- sample.size
  
  if (type == "absolute") {
    modlist$Reference.day   <- refday[1]
    modlist$Reference.month <- refday[2]
  }
  
  if (exists("coef_data")) {
    
    modlist <- cbind(modlist, plyr::rbind.fill(coef_data))
    
  }
  
  if (nrandom == 0) {
    if (!is.null(centre[[1]])) {
      LocalData         <- model.frame(LocalModel)
      LocalData$climate <- modeldat$climate
    } else {
      LocalData <- model.frame(LocalModel)
      
      if (attr(LocalModel, "class")[1] == "lme") {
        
        non_rand <- ncol(LocalData)
        
        LocalData <- cbind(LocalData, LocalModel$data[, colnames(LocalModel$fitted)[-which(colnames(LocalModel$fitted) %in% "fixed")]])
        
        colnames(LocalData)[-(1:non_rand)] <- colnames(LocalModel$fitted)[-which(colnames(LocalModel$fitted) %in% "fixed")]
        
      }
      
    }
    
    modlist$Randomised    <- "no"
    modlist               <- as.data.frame(modlist)
    LocalOutput           <- modlist[order(modlist$deltaAICc), ]
    LocalOutput$ModelAICc <- NULL
  }
  
  if (nrandom > 0) {
    modlist$Randomised        <- "yes"
    modlist                   <- as.data.frame(modlist)
    LocalOutputRand           <- modlist[order(modlist$deltaAICc), ]
    LocalOutputRand$ModelAICc <- NULL
  }
  
  if (nrandom == 0) {
    return(list(BestModel = LocalModel, BestModelData = LocalData, Dataset = LocalOutput))
  } else {
    return(LocalOutputRand)
  }
}