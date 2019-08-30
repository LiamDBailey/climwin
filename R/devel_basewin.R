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
                       spatial = NULL, cohort = NULL){
  
  ### Implementing scientific notation can cause problems because years
  ### are converted to characters in scientific notation (e.g. 2000 = "2e+3")
  ### Check options and convert scipen TEMPORARILY if needed.
  if(getOption("scipen") < 0){
    
    current_option <- getOption("scipen")
    options(scipen = 0)
    
  }
  
  #### INITIAL CHECKS ####
  
  if(cmissing != FALSE && cmissing != "method1" && cmissing != "method2"){
    
    stop("cmissing must be FALSE, 'method1' or 'method2'.")
    
  }
  
  if(type != "absolute" && type != "relative"){
    
    stop("type must be either absolute or relative.")
    
  }
  
  if(is.null(cohort) == TRUE){
    cohort = lubridate::year(as.Date(bdate, format = "%d/%m/%Y"))
  }
  
  if(k > 0 && class(baseline)[length(class(baseline))]=="coxph"){
    stop("Sorry, cross-validation is not available yet for coxph models")
  }
  
  #If the baseline model is fitted with nlme and cross validation is requested, return an error.
  if(attr(baseline, "class")[1] == "lme" && k > 0){
    
    stop("Sorry, cross-validation is currently not functioning for nlme models. Consider using lme4 if possible.")
    
  }
  
  #If spatial information is not specified, check that there are no duplicate calendar dates.
  if(is.null(spatial) & length(unique(cdate)) < length(cdate)){
    
    stop("Your cdate variable has repeated date measures. Do you have climate data from multiple sites? If so, you should specify the parameter `spatial`.")
    
  }
  
  #Create a centre function that over-rides quadratics etc. when centre != NULL
  if(is.null(centre[[1]]) == FALSE){
    func = "centre"
  }
  
  #Make xvar a list where the name of list object is the climate variable (e.g. Rain, Temp)
  if (is.list(xvar) == FALSE){
    stop("xvar should be an object of type list")
  }
  
  if (is.null(names(xvar)) == TRUE){
    numbers <- seq(1, length(xvar), 1)
    for (xname in 1:length(xvar)){
      names(xvar)[xname] = paste("climate", numbers[xname])
    }
  }
  
  if (is.na(upper) == FALSE && is.na(lower) == FALSE){
    combos       <- expand.grid(list(upper = upper, lower = lower))
    combos       <- combos[which(combos$upper >= combos$lower), ]
    allcombos    <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, gg = c(1:nrow(combos)), binary = binary))
    allcombos    <- cbind(allcombos, combos[allcombos$gg, ], deparse.level = 2)
    binarylevel  <- "two"
    allcombos$gg <- NULL
  } else if (is.na(upper) == FALSE && is.na(lower) == TRUE){
    allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, upper = upper, lower = lower, binary = binary))
    binarylevel <- "upper"
  } else if (is.na(upper) == TRUE && is.na(lower) == FALSE){
    allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, upper = upper, lower = lower, binary = binary))
    binarylevel <- "lower"
  } else if (is.na(upper) == TRUE && is.na(lower) == TRUE){
    allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func))
    binarylevel <- "none"
  }
  
  rownames(allcombos) <- seq(1, nrow(allcombos), 1)
  # message("All combinations to be tested...")
  # message(allcombos)
  
  combined <- list()
  for (combo in 1:nrow(allcombos)){
    runs <- devel_basewin(exclude = exclude, xvar = xvar[[paste(allcombos[combo, 1])]], cdate = cdate, bdate = bdate, baseline = baseline,
                    range = range, type = paste(allcombos[combo, 2]), refday = refday, stat = paste(allcombos[combo, 3]), func = paste(allcombos[combo, 4]),
                    cmissing = cmissing, cinterval = cinterval, k = k, 
                    upper = ifelse(binarylevel == "two" || binarylevel == "upper", allcombos$upper[combo], NA),
                    lower = ifelse(binarylevel == "two" || binarylevel == "lower", allcombos$lower[combo], NA),
                    binary = paste(allcombos$binary[combo]), centre = centre, cohort = cohort,
                    spatial = spatial)
    
    combined[[combo]]            <- runs
    allcombos$DeltaAICc[combo]   <- round(runs$Dataset$deltaAICc[1], digits = 2)
    allcombos$WindowOpen[combo]  <- runs$Dataset$WindowOpen[1]
    allcombos$WindowClose[combo] <- runs$Dataset$WindowClose[1]
    
    if(any(grepl("climate", model.frame(baseline)))){
      
      if(length(which("lin" == levels(allcombos$func))) >0){
        allcombos$betaL[combo] <- round(runs$Dataset$ModelBeta[1], digits = 2)
      }
      if(allcombos$func[1] == "centre"){
        if(centre[[2]] == "both"){
          allcombos$WithinGrpMean <- round(runs$Dataset$WithinGrpMean[1], digits = 2)
          allcombos$WithinGrpDev  <- round(runs$Dataset$WithinGrpDev[1], digits = 2)
        }
        if(centre[[2]] == "dev"){
          allcombos$WithinGrpDev  <- round(runs$Dataset$WithinGrpDev[1], digits = 2)
        }
        if(centre[[2]] == "mean"){
          allcombos$WithinGrpMean <- round(runs$Dataset$WithinGrpMean[1], digits = 2)
        }
      }
      if(length(which("quad" == levels(allcombos$func))) > 0){
        allcombos$betaL[combo]   <- round(runs$Dataset$ModelBeta[1], digits = 2)
        allcombos$betaQ[combo]   <- round(runs$Dataset$ModelBetaQ[1], digits = 2)
      }
      if(length(which("cub" == levels(allcombos$func))) > 0){
        allcombos$betaL[combo]   <- round(runs$Dataset$ModelBeta[1], digits = 2)
        allcombos$betaQ[combo]   <- round(runs$Dataset$ModelBetaQ[1], digits = 2)
        allcombos$betaC[combo]   <- round(runs$Dataset$ModelBetaC[1], digits = 2)
      }
      if(length(which("inv" == levels(allcombos$func))) > 0){
        allcombos$betaInv[combo] <- round(runs$Dataset$ModelBeta[1], digits = 2)
      }
      if(length(which("log" == levels(allcombos$func))) > 0){
        allcombos$betaLog[combo] <- round(runs$Dataset$ModelBeta[1], digits = 2)
      } 
    }
  }
  allcombos <- cbind(response = colnames(model.frame(baseline))[1], allcombos)
  combined <- c(combined, combos = list(allcombos))
  
  #If we changed scipen at the start, switch it back to default
  if(exists("current_option")){
    
    options(scipen = current_option)
    
  }
  
  return(combined)
}

#######################################



devel_basewin <- function(exclude, xvar, cdate, bdate, baseline, range, 
                    type, stat = "mean", func = "lin", refday,
                    cmissing = FALSE, cinterval = "day", nrandom = 0, k = 0,
                    spatial, upper = NA, lower = NA, binary = FALSE, scale = FALSE, centre = list(NULL, "both"),
                    cohort = NULL, randwin = FALSE, randwin_thresholdQ){
  
  message("Initialising, please wait...")
  
  options(warn = 0, nwarnings = 1)
  
  ##########################################################################
  
  #### INITIAL CHECKS ####
  
  #Check that climate date data is in the correct date format
  if(all(is.na(as.Date(cdate, format = "%d/%m/%Y")))){
    
    stop("cdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  #Check that biological date data is in the correct date format
  if(all(is.na(as.Date(bdate, format = "%d/%m/%Y")))){
    
    stop("bdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  #If the user wants to use slope with log or inverse...
  if (stat == "slope" && func == "log" || stat == "slope" && func == "inv"){
    
    #Return an error...
    stop("stat = slope cannot be used with func = log or inv as negative values may be present")
  }
  
  #If the user has centred the data
  if(!is.null(centre[[1]])){
    
    #But they haven't specified whether they want within and/or between group...
    if(centre[[2]] != "both" && centre[[2]] != "dev" && centre[[2]] != "mean"){
      
      #Return an error...
      stop("Please set centre to one of 'both', 'dev', or 'mean'. See help file for details.")
    }
  }
  
  ##########################################################################
  
  #### DEALING WITH THRESHOLDS ####
  
  #By default, don't ask a question about how you want to apply the thresholds.
  thresholdQ <- "N"
  
  #If you are not using randwin, then ask the question about how thresholds should be applied.
  if(randwin == FALSE){
    
    #If you have specified an upper or lower value for which a threshold should be applied and you are not working at a daily scale...
    if((!is.na(upper) || !is.na(lower)) && (cinterval == "week" || cinterval == "month")){
      
      #Determine whether the user wants to: 1. apply the threshold at the daily level BEFORE estimating monthly/weekly mean (i.e. daily data is binary but monthly/weekly is not)
      #                                     2. apply the threshold AFTER applying monthly/weekly mean (i.e. daily data is non-binary, monthly/weekly is)
      thresholdQ <- readline("You specified a climate threshold using upper and/or lower and are working at a weekly or monthly scale. 
                           Do you want to apply this threshold before calculating weekly/monthly means (i.e. calculate thresholds for each day)? Y/N")
      
      #Convert to upper case for logical conditions
      thresholdQ <- toupper(thresholdQ)
      
      #If they didn't specify a Y/N answer, ask again.
      if(thresholdQ != "Y" & thresholdQ != "N"){
        
        thresholdQ <- readline("Please specify yes (Y) or no (N)")
        
      }
      
    }
    
    #If they are using randwin, make a decision based on earlier specified argument.    
  } else {
    
    if((!is.na(upper) || !is.na(lower)) && (cinterval == "week" || cinterval == "month")){
      
      thresholdQ <- randwin_thresholdQ
      
    }
    
  }
  
  ##########################################################################
  
  #### SPATIAL REPLICATION ####
  
  #If spatial info has been provided...
  if(!is.null(spatial)){
    
    #Create a data frame with the biological data and its spatial information.  
    sample.size <- 0
    data <- data.frame(bdate = bdate, spatial = spatial[[1]], cohort = as.factor(cohort))
    
    #For each cohort (usually year, but may also be breeding period)...  
    for(i in unique(data$cohort)){
      
      #Take a subset of the data for that cohort...
      sub <- subset(data, cohort = i)
      #Relevel spatial...
      sub$spatial <- factor(sub$spatial)
      #Add 1 to sample size for every site in each cohort.
      sample.size <- sample.size + length(levels(sub$spatial))
      
    }
    
    #If spatial is not provided...
  } else if(is.null(spatial)) {
    
    #Sample size is just the length of cohorts (i.e. number of years)
    sample.size <- length(unique(cohort))
    
  }
  
  ##########################################################################
  
  #PROCESS DATA INTO CLIMWIN FORMAT
  
  #Duration of searching period is the difference between the two levels or range + 1 (e.g. also including day 0)
  duration  <- (range[1] - range[2]) + 1
  #Determine maximum number of potential windows to fit.
  maxmodno  <- (duration * (duration + 1))/2
  
  #If the user has provided exclude data (i.e. there are certain windows that will not be considered)...
  if (length(exclude) == 2){
    
    #Remove these excluded windows from the estimate of maximum number of windows.
    maxmodno  <- maxmodno - exclude[1] * (duration - exclude[2] - 1) + (exclude[1] - 1) * exclude[1] / 2
    
  }
  
  #If slope stat is used...
  if (stat == "slope") { 
    
    #Adjust number of models...
    ifelse(is.na(exclude[2]) == TRUE,  maxmodno  <- maxmodno - duration, maxmodno  <- maxmodno - exclude[2] - 1)
    
  }
  
  #Convert date information in to numbers and apply absolute window info (if appropriate)
  #This code creates a new climate dataframe with continuous daynumbers, leap days are not a problem
  cont      <- convertdate(bdate = bdate, cdate = cdate, xvar = xvar, 
                           cinterval = cinterval, type = type, 
                           refday = refday, cohort = cohort, spatial = spatial, 
                           binary = binary, upper = upper, lower = lower, thresholdQ = thresholdQ)   
  
  if(!is.null(spatial)){ #If spatial data is provided...
    
    for(i in unique(spatial[[1]])){ #For each site...
      
      SUB_clim <- subset(cont$cintno, spatial == i) # ...subset the date numbers from climate data...
      SUB_biol <- subset(cont$bintno, spatial == i) # ...subset the date numbers from biological data...
      
      #Check that you have enough data to go back the specified range at EACH SITE
      if ((min(SUB_biol$Date) - range[1]) < min(SUB_clim$Date)){
        
        stop(paste("At site ", i, " you do not have enough climate data to search ", range[1], " ", cinterval, "s back. Please adjust the value of range or add additional climate data.", sep = ""))
        
      }
      
      #Check that you have enough data to start in the specified range at EACH SITE
      if (max(SUB_biol$Date) - range[2] > max(SUB_clim$Date)){
        
        stop(paste("At site ", i, " you need more recent climate data. The most recent climate data is from ", max(SUB_clim$Date), " while the most recent biological data is from ", max(SUB_biol$Date), sep = ""))
        
      }
    }
    
  } else { #If spatial data is not provided.
    
    #Check that you have enough data to go back the specified range
    if ((min(cont$bintno) - range[1]) < min(cont$cintno)){
      stop(paste("You do not have enough climate data to search ", range[1], " ", cinterval, "s before ", min(as.Date(bdate, format = "%d/%m/%Y")), ". Please adjust the value of range or add additional climate data.", sep = ""))
    }
    
    #Check that you have enough data to start in the specified range
    if ((max(cont$bintno) - range[2] - 1) > max(cont$cintno)){
      stop(paste("You need more recent climate data to test over this range. The most recent climate data is from ", max(as.Date(cdate, format = "%d/%m/%Y")), " while the most recent biological data is from ", max(as.Date(bdate, format = "%d/%m/%Y")), sep = ""))
    }
    
  }
  
  modno     <- 1  #Create a model number variable that will count up during the loop#
  cmatrix   <- matrix(ncol = (duration), nrow = length(bdate))  # matrix that stores the weather data for variable or fixed windows
  
  modlist   <- list()   # dataframes to store ouput
  if(class(baseline)[1] == "lme"){
    
    baseline  <- update(baseline, .~.)
    
  } else {
    
    baseline  <- my_update(baseline, .~.) 
    
  }
  nullmodel <- MuMIn::AICc(baseline)
  modeldat  <- model.frame(baseline)
  
  #If there are any variables that have been scaled (i.e. using scale(x)) we need to change the model data to remove the scale argument.
  #If not, the model.frame output has a column scale(x) but the model during update is looking for x.
  if(any(grepl("scale\\(|\\)", colnames(modeldat)))){
    
    colnames(modeldat) <- gsub("scale\\(|\\)", "", colnames(modeldat))
    
  }
  
  if(attr(baseline, "class")[1] == "lme"){ #If model is fitted using nlme package
    
    if(is.null(baseline$modelStruct$varStruct) == FALSE && !is.null(attr(baseline$modelStruct$varStruct, "groups"))){ #If a custom variance structure has been included and it has multiple levels...
      
      modeldat <- cbind(modeldat, attr(baseline$modelStruct$varStruct, "groups")) #Add the variables from this variance structure to the model data used for updating models.
      
      colnames(modeldat)[ncol(modeldat)] <- strsplit(x = as.character(attr(baseline$modelStruct$varStruct, "formula"))[2], split = " | ")[[1]][3] #Make the names equivalent.
      
    }
    
    #If a complex variance structure hasn't been provided...
    
    non_rand <- ncol(modeldat) #Determine the number of non-random variables from the original model data.
    
    modeldat <- cbind(modeldat, baseline$data[, colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")]]) #Include random effects (i.e. those that AREN'T FIXED)
    
    colnames(modeldat)[-(1:non_rand)] <- colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")] #Make sure these columns are named correctly
    
  }
  
  #If using coxph models, adjust naming of variables to deal with frailty terms.
  if(class(baseline)[length(class(baseline))]=="coxph" && grepl("frailty\\(", colnames(modeldat)[ncol(modeldat)])){
    colnames(modeldat)[ncol(modeldat)] <- gsub("frailty\\(", "", colnames(modeldat)[ncol(modeldat)])
    colnames(modeldat)[ncol(modeldat)] <- gsub("\\)", "", colnames(modeldat)[ncol(modeldat)])
  }
  
  #Rename response variable as yvar (this way the name is standardised and can be easily called)
  colnames(modeldat)[1] <- "yvar"
  
  #If user has provided some variable for mean centring change the func to centre (i.e. you can't do mean centring and quadratic/cubic)
  if (is.null(centre[[1]]) == FALSE){
    func <- "centre"
  }
  
  #Determine length of data provided for response variable.
  ifelse(class(baseline)[length(class(baseline))]=="coxph", leng <- length(modeldat$yvar[,1]), leng <- length(modeldat$yvar))
  #If there are NAs present in the biological data, provide an error.
  if (leng != length(bdate)){
    stop("NA values present in biological response. Please remove NA values")
  }
  
  if(cinterval == "day" || (!is.na(thresholdQ) && thresholdQ == "N")){ #If dealing with daily data OR user chose to apply threshold later...
    
    if(is.null(spatial) == FALSE){ #...and spatial information is provided...
      
      if (is.na(upper) == FALSE && is.na(lower) == TRUE){ #...and an upper bound is provided...
        if (binary == TRUE){ #...and we want data to be binary (i.e. it's above the value or it's not)
          cont$xvar$Clim <- ifelse (cont$xvar$Clim > upper, 1, 0) #Then turn climate data into binary data.
        } else { #Otherwise, if binary is not true, simply make all data below the upper limit into 0.
          cont$xvar$Clim <- ifelse (cont$xvar$Clim > upper, cont$xvar$Clim, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == TRUE){ #If a lower limit has been provided, do the same.
        if (binary == TRUE){
          cont$xvar$Clim <- ifelse (cont$xvar$Clim < lower, 1, 0)
        } else {
          cont$xvar$Clim <- ifelse (cont$xvar$Clim < lower, cont$xvar$Clim, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == FALSE){ #If both an upper and lower limit are provided, do the same.
        if (binary == TRUE){
          cont$xvar$Clim <- ifelse (cont$xvar$Clim > lower && cont$xvar$Clim < upper, 1, 0)
        } else {
          cont$xvar$Clim <- ifelse (cont$xvar$Clim > lower && cont$xvar$Clim < upper, cont$xvar$Clim - lower, 0)
        } 
      }
      
    } else { #Do the same with non-spatial data (syntax is just a bit different, but method is the same.)
      
      if (is.na(upper) == FALSE && is.na(lower) == TRUE){
        if (binary == TRUE){
          cont$xvar <- ifelse (cont$xvar > upper, 1, 0)
        } else {
          cont$xvar <- ifelse (cont$xvar > upper, cont$xvar, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == TRUE){
        if (binary == TRUE){
          cont$xvar <- ifelse (cont$xvar < lower, 1, 0)
        } else {
          cont$xvar <- ifelse (cont$xvar < lower, cont$xvar, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == FALSE){
        if (binary == TRUE){
          cont$xvar <- ifelse (cont$xvar > lower & cont$xvar < upper, 1, 0)
        } else {
          cont$xvar <- ifelse (cont$xvar > lower & cont$xvar < upper, cont$xvar - lower, 0)
        } 
      } 
      
    }
    
  }
  
  if(is.null(spatial) == FALSE){ #If spatial information is provided...
    for (i in 1:length(bdate)){ #For each biological record we have...
      #Take a row in the empty matrix and add climate data from the correct site and over the full date range chosen by the user.
      cmatrix[i, ] <- cont$xvar[which(cont$cintno$spatial %in% cont$bintno$spatial[i] & cont$cintno$Date %in% (cont$bintno$Date[i] - c(range[2]:range[1]))), 1]  
    }
  } else { #If no spatial data is provided, do the same but without checking site ID
    for (i in 1:length(bdate)){
      cmatrix[i, ] <- cont$xvar[which(cont$cintno %in% (cont$bintno[i] - c(range[2]:range[1])))]    
    } 
  }
  
  #Make sure the order is correct, so most recent climate data is in the earliest column
  cmatrix <- as.matrix(cmatrix[, c(ncol(cmatrix):1)])
  
  #return(list(cmatrix, cont))
  
  if(cmissing == FALSE & any(is.na(cmatrix))){ #If the user doesn't expect missing climate data BUT there are missing data present...
    if(is.null(spatial) == FALSE){ #And spatial data has been provided...
      
      if (cinterval == "day"){ #Where a daily interval is used...
        #...save an object 'missing' with the full dates of all missing data.
        .GlobalEnv$missing <- as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      
      if (cinterval == "month"){ #Where a monthly interval is used...
        #...save an object 'missing' with the month and year of all missing data.
        .GlobalEnv$missing <- c(paste("Month:", cont$cintno$Date[is.na(cont$xvar$Clim)] - (floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/12) * 12),
                                      #lubridate::month(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/12)))
        #lubridate::year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if (cinterval == "week"){ #Where weekly data is used...
        #...save an object 'missing' with the week and year of all missing data.
        .GlobalEnv$missing <- c(paste("Week:", cont$cintno$Date[is.na(cont$xvar$Clim)] - (floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/52) * 52),
                                      #lubridate::week(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/52)))
        #lubridate::year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
    } else { #If spatial data is not provided.
      
      if (cinterval == "day"){ #Do the same for day (syntax is just a bit differen)
        .GlobalEnv$missing <- as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if (cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", (lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1)) - (floor((lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1))/12)*12),
                                      "Year:", (floor((which(is.na(cont$xvar)) - 1)/12) + lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))))))
      }
      if (cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", cont$cintno[is.na(cont$xvar)] - (floor(cont$cintno[is.na(cont$xvar)]/52) * 52),
                                      #ceiling(((as.numeric((as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y"))) - (floor(which(is.na(cmatrix))/nrow(cmatrix))*7)) - as.numeric(as.Date(paste("01/01/", lubridate::year(as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) + 1) / 7),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cont$cintno[is.na(cont$xvar)]/52)))
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
      
      #Determine the column and row location...
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
            cdate_new <- data.frame(Date = cont$cintno$Date,
                                    spatial = cont$cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno$Date[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(cont$xvar$Clim[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)) & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            cmatrix[row, col] <- mean(cont$xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)))], na.rm = T)
            
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
            cdate_new <- data.frame(Date = cont$cintno$Date,
                                    spatial = cont$cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno$Date[row]
            
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
            
            cmatrix[row, col] <- mean(cont$xvar$Clim[which(cdate_new$Date %in% missingdate & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno[row]
            
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
            
            cmatrix[row, col] <- mean(cont$xvar[which(cdate_new$Date %in% missingdate)], na.rm = T)
            
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
  #   if(class(baseline) == "lm"){
  #     
  #     if(!is.null(weights(baseline))){
  #       
  #       
  #       
  #     }
  #     
  #   }
  #   if (class(baseline)[1] == "glm" && sum(weights(baseline)) == nrow(model.frame(baseline)) || attr(class(baseline), "package") == "lme4" && sum(weights(baseline)) == nrow(model.frame(baseline))){
  #     
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
  
  #If there are no variables in the baseline model called climate (i.e. the user has not specified more complex role for climate in the model, such as an interaction or random effects.)
  if(all(!grepl("climate", colnames(modeldat)))){
    
    #Create a new dummy variable called climate, that is made up all of 1s (unless it's using lme, because this will cause errors).
    if(attr(baseline, "class")[1] == "lme"){
      
      modeldat$climate <- seq(1, nrow(modeldat), 1)
      
    } else {
      
      modeldat$climate <- 1
      
    }
    
    #Update the baseline model to include this new variable in the required format (e.g. linear, quadratic etc.)
    if (func == "lin"){
      modeloutput <- update(baseline, yvar~. + climate, data = modeldat)
    } else if (func == "quad") {
      modeloutput <- update(baseline, yvar~. + climate + I(climate ^ 2), data = modeldat)
    } else if (func == "cub") {
      modeloutput <- update(baseline, yvar~. + climate + I(climate ^ 2) + I(climate ^ 3), data = modeldat)
    } else if (func == "log") {
      modeloutput <- update(baseline, yvar~. + log(climate), data = modeldat)
    } else if (func == "inv") {
      modeloutput <- update (baseline, yvar~. + I(climate ^ -1), data = modeldat)
    } else if (func == "centre"){
      if(centre[[2]] == "both"){
        modeldat$wgdev  <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeloutput <- update (baseline, yvar ~. + wgdev + wgmean, data = modeldat)
      }
      if(centre[[2]] == "mean"){
        modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeloutput <- update (baseline, yvar ~. + wgmean, data = modeldat)
      }
      if(centre[[2]] == "dev"){
        modeldat$wgdev  <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeloutput <- update (baseline, yvar ~. + wgdev, data = modeldat)
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
  if (k >= 1){
    modeldat$K <- sample(seq(from = 1, to = length(modeldat$climate), by = 1) %% k + 1)
  }   # create labels k-fold crossvalidation
  
  #Determine all possible combos of days
  all_windows <- expand.grid(WindowOpen = range[2]:range[1],
                             WindowClose = range[2]:range[1])
  #Subset only those where WindowOpen is >= WindowClose
  all_windows <- subset(all_windows, all_windows$WindowClose <= all_windows$WindowOpen)
  #Determine duration of windows
  all_windows$duration <- all_windows$WindowOpen - all_windows$WindowClose
  
  #Create the progress bar
  pb <- txtProgressBar(min = 1, max = nrow(all_windows), style = 3, char = "|")
  
  #Now, loop through every row and run models
  for(row in 1:nrow(all_windows)){
    
    #Start index is the column in the cmatrix that should be extracted
    #This is the actual number of days in the past - range[2]
    #column 1 in cmatrix is the first day that windows would be built
    start_index <- all_windows$WindowClose[row] - range[2] + 1
    end_index   <- all_windows$WindowOpen[row] - range[2] + 1
    
    if(all_windows$duration[row] == 0){
      
      modeldat$climate <- cmatrix[, start_index:end_index]
      
    } else {
      
      modeldat$climate <- apply(cmatrix[, start_index:end_index], 1, FUN = stat)
      
    }
    
    modeloutput <- tryCatch({
      
      #update(modeloutput, .~., data = modeldat)
      eval(getCall(modeloutput))
      
    }, error = function(e){
      
      #update(baseline, yvar~., data = modeldat)
      modcall <- getCall(baseline)
      modcall$formula <- update.formula(old = modcall$formula, new = yvar ~ .)
      modcall$data <- modeldat
      eval(modcall)
      
    })
    
    modlist$deltaAICc[[row]] <- AICc(modeloutput) - AICc(baseline)
    modlist$ModelAICc[[row]] <- AICc(modeloutput)
    
    modlist$WindowOpen[[row]]  <- all_windows$WindowOpen[row]
    modlist$WindowClose[[row]] <- all_windows$WindowClose[row]
    
    if(class(modeloutput)[1] %in% c("lm", "lmerMod")){
      
      mod_summary <- coef(summary(modeloutput))
      
      modlist$ModelInt[[row]] <- mod_summary[1, "Estimate"]
      
      #If a model did not fit a climate term (i.e. climate was all 0)
      #Just return NA
      if(!"climate" %in% rownames(mod_summary)){
        
        modlist$ModelBeta[[row]]  <- NA
        modlist$Std.Error[[row]]  <- NA
        modlist$ModelBetaQ[[row]] <- NA
        modlist$ModelBetaC[[row]] <- NA
        
        if(func == "quad"){
          
          modlist$Std.ErrorQ[[row]] <- NA
          
        }
        
        if(func == "cub"){
          
          modlist$Std.ErrorQ[[row]] <- NA
          modlist$Std.ErrorC[[row]] <- NA
          
        }
        
      } else {
        
        if(func == "quad"){
          
          modlist$ModelBeta[[row]]  <- mod_summary[nrow(mod_summary) - 1, "Estimate"]
          modlist$Std.Error[[row]]  <- mod_summary[nrow(mod_summary) - 1, "Std. Error"]
          modlist$ModelBetaQ[[row]] <- mod_summary[nrow(mod_summary), "Estimate"]
          modlist$Std.ErrorQ[[row]] <- mod_summary[nrow(mod_summary), "Std. Error"]
          modlist$ModelBetaC[[row]] <- NA
          
        } else if(func == "cub"){
          
          modlist$ModelBeta[[row]]  <- mod_summary[nrow(mod_summary) - 2, "Estimate"]
          modlist$Std.Error[[row]]  <- mod_summary[nrow(mod_summary) - 2, "Std. Error"]
          modlist$ModelBetaQ[[row]] <- mod_summary[nrow(mod_summary) - 1, "Estimate"]
          modlist$Std.ErrorQ[[row]] <- mod_summary[nrow(mod_summary) - 1, "Std. Error"]
          modlist$ModelBetaC[[row]] <- mod_summary[nrow(mod_summary), "Estimate"]
          modlist$Std.ErrorC[[row]] <- mod_summary[nrow(mod_summary), "Std. Error"]
          
        } else {
          
          modlist$ModelBeta[[row]]  <- mod_summary[nrow(mod_summary), "Estimate"]
          modlist$Std.Error[[row]]  <- mod_summary[nrow(mod_summary), "Std. Error"]
          modlist$ModelBetaQ[[row]] <- NA
          modlist$ModelBetaC[[row]] <- NA
          
        }
        
      }
  
    }
    
    setTxtProgressBar(pb, row)
    
  }
  
  modlist <- as.data.frame(modlist)
  modlist <- modlist[order(modlist$deltaAICc), ]
  
  #Save the best model
  start_index <- modlist$WindowClose[1] - range[2] + 1
  end_index   <- modlist$WindowOpen[1] - range[2] + 1
  
  if(start_index == end_index){
    
    modeldat$climate <- cmatrix[, start_index:end_index]
    
  } else {
    
    modeldat$climate <- apply(cmatrix[, start_index:end_index], 1, FUN = stat)
    
  }
  
  LocalModel <- tryCatch({
    
    update(modeloutput, .~., data = modeldat)
    
  }, error = function(e){
    
    update(baseline, yvar~., data = modeldat)
    
  })
  
  # #CREATE A FOR LOOP TO FIT DIFFERENT CLIMATE WINDOWS#
  # for (m in range[2]:range[1]){ #For every day in the given range...
  #   for (n in 1:duration){ #And for each possible window duration...
  #     if (length(exclude) == 2 && m >= exclude[2] && (m-n) >= exclude[2] && n <= exclude[1]){
  #       next #If an exclude term has been provided, skip those windows that are meant to be excluded.
  #     }
  #     if ( (m - n) >= (range[2] - 1)){  # do not use windows that overshoot the closest possible day in window
  #       if (stat != "slope" || n > 1){ #Don't use windows one day long with function slope...
  #         windowopen  <- m - range[2] + 1 #Determine the windowopen time (i.e. the point in the past where the window STARTS)
  #         windowclose <- windowopen - n + 1 #Determine the windowclose time (i.e. the more recent point where the window FINISHES)
  #         
  #         if (stat == "slope"){ #If we are using the slope function
  #           time             <- n:1 #Determine the number of days over which we will calculate slope.
  #           #Determine the slope (i.e. change in climate over time)
  #           modeldat$climate <- apply(cmatrix[, windowopen:windowclose], 1, FUN = function(x) coef(lm(x ~ time))[2])
  #         } else {
  #           #If slopes is not specified, apply the chosen aggregate statistic (e.g. mean, mass) to the window.
  #           ifelse (n == 1, modeldat$climate <- cmatrix[, windowopen:windowclose], 
  #                   modeldat$climate <- apply(cmatrix[, windowopen:windowclose], 1, FUN = stat))
  #         }
  #         
  #         #Stop climwin if there are values <=0 and func is log or inverse.
  #         if (min(modeldat$climate) <= 0 && func == "log" || min(modeldat$climate) <= 0 && func == "inv"){
  #           stop("func = log or inv cannot be used with climate values <= 0. 
  #                Consider adding a constant to climate data to remove these values")
  #         }
  #         
  #         #If using models from nlme and there is an issue where climate has no variance (e.g. short windows where rainfall is all 0)
  #         if(attr(modeloutput, "class")[1] == "lme" && var(modeldat$climate) == 0){
  #           
  #           #skip the fitting of the climate data and just treat it has deltaAICc of 0
  #           #This is necessary as nlme doesn't have a way to deal with rank deficiency (unlike lme4) and will give an error
  #           modeloutput  <- baseline
  #           AICc_cv_avg  <- AICc(baseline)
  #           deltaAICc_cv <- AICc(baseline) - AICc(baseline)
  #           
  #         } else {
  #           
  #           #If mean centring is specified, carry this out on the data from the climate window.
  #           if (is.null(centre[[1]]) == FALSE){
  #             if(centre[[2]] == "both"){
  #               modeldat$wgdev  <- wgdev(modeldat$climate, centre[[1]])
  #               modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
  #               
  #               if(class(baseline)[1] == "coxph"){
  #                 
  #                 modeloutput <- my_update(modeloutput, .~., data = modeldat)
  #                 
  #               } else {
  #                 
  #                 modeloutput <- update(modeloutput, .~., data = modeldat)
  #                 
  #               }
  #               
  #             }
  #             if(centre[[2]] == "mean"){
  #               modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
  #               
  #               if(class(baseline)[1] == "coxph"){
  #                 
  #                 modeloutput <- my_update(modeloutput, .~., data = modeldat)
  #                 
  #               } else {
  #                 
  #                 modeloutput <- update(modeloutput, .~., data = modeldat)
  #                 
  #               }
  #               
  #             }
  #             if(centre[[2]] == "dev"){
  #               modeldat$wgdev  <- wgdev(modeldat$climate, centre[[1]])
  #               
  #               if(class(baseline)[1] == "coxph"){
  #                 
  #                 modeloutput <- my_update(modeloutput, .~., data = modeldat)
  #                 
  #               } else {
  #                 
  #                 modeloutput <- update(modeloutput, .~., data = modeldat)
  #                 
  #               }
  #               
  #             }
  #           } else {
  #             
  #             #Update models with this new climate data (syntax is a bit different for nlme v. other models)
  #             if(attr(modeloutput, "class")[1] == "lme"){
  #               
  #               modeloutput <- tryCatch({
  #                 
  #                 update(modeloutput, .~., data = modeldat); 
  #                 update(modeloutput, .~., data = modeldat)
  #                 
  #               }, error = function(e){
  #                 
  #                 update(baseline, yvar~., data = modeldat)
  #                 
  #               })
  #               
  #               if(all(!colnames(model.frame(modeloutput)) %in% "climate")){
  #                 
  #                 warning("A model from one climate windows failed to converge. This model was replaced with the null model")
  #                 
  #               }
  #               
  #             } else {
  #               
  #               if(class(baseline)[1] == "coxph"){
  #                 
  #                 modeloutput <- my_update(modeloutput, .~., data = modeldat)
  #                 
  #               } else {
  #                 
  #                 modeloutput <- update(modeloutput, .~., data = modeldat)
  #                 
  #               }
  #               
  #             }
  #           }
  #           
  #           # If valid, perform k-fold crossvalidation
  #           if (k >= 1) {      
  #             for (k in 1:k) {
  #               test                     <- subset(modeldat, modeldat$K == k) # Create the test dataset
  #               train                    <- subset(modeldat, modeldat$K != k) # Create the train dataset
  #               baselinecv               <- update(baseline, yvar~., data = train) # Refit the model without climate using the train dataset
  #               modeloutputcv            <- update(modeloutput, yvar~., data = train)  # Refit the model with climate using the train dataset
  #               test$predictions         <- predict(modeloutputcv, newdata = test, allow.new.levels = TRUE, type = "response") # Test the output of the climate model fitted using the test data
  #               test$predictionsbaseline <- predict(baselinecv, newdata = test, allow.new.levels = TRUE, type = "response") # Test the output of the null models fitted using the test data
  #               
  #               num        <- length(test$predictions) # Determine the length of the test dataset
  #               p          <- num - df.residual(modeloutputcv)  # Determine df for the climate model
  #               mse        <- sum((test$predictions - test[, 1]) ^ 2) / num
  #               p_baseline <- num - df.residual(baselinecv)  # Determine df for the baseline model
  #               
  #               #calculate mean standard errors for climate model
  #               #calc mse only works non-categorical yvars, e.g. normal, binary, count data 
  #               mse_baseline <- sum((test$predictionsbaseline - test[, 1]) ^ 2) / num
  #               #calculate mean standard errors for null model
  #               AICc_cv          <- num * log(mse) + (2 * p * (p + 1)) / (num - p - 1)
  #               AICc_cv_baseline <- num * log(mse_baseline) + (2 * p_baseline * (p_baseline + 1)) / (num - p_baseline - 1)
  #               
  #               #Calculate AICc values for climate and baseline models
  #               #rmse_corrected<-sqrt(sum((test$predictions-test[,1])^2)/modeloutputcv$df[1])
  #               ifelse (k == 1, AICc_cvtotal <- AICc_cv, AICc_cvtotal <- AICc_cvtotal + AICc_cv)              
  #               ifelse (k == 1, AICc_cv_basetotal <- AICc_cv_baseline, AICc_cv_basetotal <- AICc_cv_basetotal + AICc_cv_baseline)
  #               #Add up the AICc values for all iterations of crossvalidation
  #             }
  #             
  #             AICc_cv_avg          <- AICc_cvtotal / k # Determine the average AICc value of the climate model from cross validations
  #             AICc_cv_baseline_avg <- AICc_cv_basetotal / k # Determine the average AICc value of the null model from cross validations
  #             deltaAICc_cv         <- AICc_cv_avg - AICc_cv_baseline_avg # Calculate delta AICc
  #             
  #           }
  #           
  #         }
  #         
  #         #Add model parameters to list
  #         if (k > 1){
  #           
  #           modlist$ModelAICc[[modno]]    <- AICc_cv_avg
  #           modlist$deltaAICc[[modno]]    <- deltaAICc_cv
  #           
  #         } else {
  #           
  #           modlist$deltaAICc[[modno]] <- AICc(modeloutput) - AICc(baseline)
  #           modlist$ModelAICc[[modno]] <- AICc(modeloutput)
  #         }
  #         
  #         modlist$WindowOpen[[modno]]  <- m
  #         modlist$WindowClose[[modno]] <- m - n + 1
  #         
  #         #Extract model coefficients (syntax is slightly different depending on the model type e.g. lme4 v. nlme v. lm)
  #         if(any(grepl("climate", colnames(model.frame(baseline))))){
  #           
  #           coefs <- coef(summary(modeloutput))[, 1:2]
  #           
  #           temp.df <- data.frame("Y", t(coefs[-1, 1]), t(coefs[-1, 2]))
  #           
  #           colnames(temp.df) <- c("Custom.mod", gsub("scale\\(|\\)", "", rownames(coefs)[-1]), paste(gsub("scale\\(|\\)", "", rownames(coefs)[-1]), "SE", sep = ""))
  #           
  #           coef_data[[modno]] <- temp.df
  #           
  #         } else {
  #           
  #           if (class(baseline)[length(class(baseline))] == "coxph") {
  #             if (func == "quad"){
  #               modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))-1]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-1]
  #               modlist$ModelBetaQ[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
  #               modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
  #               modlist$ModelBetaC[[modno]] <- NA
  #               modlist$ModelInt[[modno]]   <- 0
  #             } else if (func == "cub"){
  #               modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))-2]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-2]
  #               modlist$ModelBetaQ[[modno]] <- coef(modeloutput)[length(coef(modeloutput))-1]
  #               modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-1]
  #               modlist$ModelBetaC[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
  #               modlist$Std.ErrorC[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
  #               modlist$ModelInt[[modno]]   <- 0
  #             } else if (func == "centre"){
  #               if(centre[[2]] == "both"){
  #                 modlist$WithinGrpMean[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
  #                 modlist$Std.ErrorMean[[modno]] <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
  #                 modlist$WithinGrpDev[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))-1]
  #                 modlist$Std.ErrorDev[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-1]
  #                 modlist$ModelInt[[modno]]      <- 0
  #               }
  #               if(centre[[2]] == "mean"){
  #                 modlist$WithinGrpMean[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
  #                 modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
  #                 modlist$ModelInt[[modno]]      <- 0
  #               }
  #               if(centre[[2]] == "dev"){
  #                 modlist$WithinGrpDev[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))]
  #                 modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
  #                 modlist$ModelInt[[modno]]      <- 0
  #               }
  #             } else {
  #               modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
  #               modlist$ModelBetaQ[[modno]] <- NA
  #               modlist$ModelBetaC[[modno]] <- NA
  #               modlist$ModelInt[[modno]]   <- 0
  #             }
  #           } else if (length(attr(class(modeloutput),"package")) > 0 && attr(class(modeloutput), "package") == "lme4"){            
  #             if (func == "quad"){
  #               
  #               browser(expr = length(fixef(modeloutput)[length(fixef(modeloutput)) - 1]) == 0)
  #               
  #               modlist$ModelBeta[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
  #               modlist$ModelBetaQ[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
  #               modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
  #               modlist$ModelBetaC[[modno]] <- NA
  #               modlist$ModelInt[[modno]]   <- fixef(modeloutput)[1]
  #             } else if (func == "cub"){
  #               modlist$ModelBeta[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 2]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
  #               modlist$ModelBetaQ[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
  #               modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
  #               modlist$ModelBetaC[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
  #               modlist$Std.ErrorC[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
  #               modlist$ModelInt[[modno]]   <- fixef(modeloutput)[1]
  #             } else if (func == "centre"){
  #               if(centre[[2]] == "both"){
  #                 modlist$WithinGrpMean[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
  #                 modlist$Std.ErrorMean[[modno]] <- coef(summary(modeloutput))[, "Std. Error"][2]
  #                 modlist$WithinGrpDev[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
  #                 modlist$Std.ErrorDev[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
  #                 modlist$ModelInt[[modno]]      <- fixef(modeloutput)[1]
  #               }
  #               if(centre[[2]] == "mean"){
  #                 modlist$WithinGrpMean[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
  #                 modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "Std. Error"][2]
  #                 modlist$ModelInt[[modno]]      <- fixef(modeloutput)[1]
  #               }
  #               if(centre[[2]] == "dev"){
  #                 modlist$WithinGrpDev[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
  #                 modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "Std. Error"][2]
  #                 modlist$ModelInt[[modno]]      <- fixef(modeloutput)[1]
  #               }
  #             } else {
  #               
  #               modlist$ModelBeta[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput))]
  #               
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
  #               modlist$ModelBetaQ[[modno]] <- NA
  #               modlist$ModelBetaC[[modno]] <- NA
  #               modlist$ModelInt[[modno]]   <- fixef(modeloutput)[1]
  #             }
  #             
  #           } else if(attr(baseline, "class")[1] == "lme"){
  #             
  #             if (func == "quad"){
  #               modlist$ModelBeta[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std.Error"][2]
  #               modlist$ModelBetaQ[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
  #               modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std.Error"][3]
  #               modlist$ModelBetaC[[modno]] <- NA
  #               modlist$ModelInt[[modno]]   <- fixef(modeloutput)[1]
  #             } else if (func == "cub"){
  #               modlist$ModelBeta[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 2]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std.Error"][2]
  #               modlist$ModelBetaQ[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
  #               modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std.Error"][3]
  #               modlist$ModelBetaC[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
  #               modlist$Std.ErrorC[[modno]]  <- coef(summary(modeloutput))[, "Std.Error"][3]
  #               modlist$ModelInt[[modno]]   <- fixef(modeloutput)[1]
  #             } else if (func == "centre"){
  #               if(centre[[2]] == "both"){
  #                 modlist$WithinGrpMean[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
  #                 modlist$Std.ErrorMean[[modno]] <- coef(summary(modeloutput))[, "Std.Error"][2]
  #                 modlist$WithinGrpDev[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
  #                 modlist$Std.ErrorDev[[modno]]  <- coef(summary(modeloutput))[, "Std.Error"][3]
  #                 modlist$ModelInt[[modno]]      <- fixef(modeloutput)[1]
  #               }
  #               if(centre[[2]] == "mean"){
  #                 modlist$WithinGrpMean[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
  #                 modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "Std.Error"][2]
  #                 modlist$ModelInt[[modno]]      <- fixef(modeloutput)[1]
  #               }
  #               if(centre[[2]] == "dev"){
  #                 modlist$WithinGrpDev[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
  #                 modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "Std.Error"][2]
  #                 modlist$ModelInt[[modno]]      <- fixef(modeloutput)[1]
  #               }
  #             } else {
  #               
  #               modlist$ModelBeta[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput))]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std.Error"][2]
  #               modlist$ModelBetaQ[[modno]] <- NA
  #               modlist$ModelBetaC[[modno]] <- NA
  #               modlist$ModelInt[[modno]]   <- fixef(modeloutput)[1]
  #             }
  #             
  #           } else {
  #             if (func == "quad"){
  #               modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput)) - 1]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
  #               modlist$ModelBetaQ[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
  #               modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
  #               modlist$ModelBetaC[[modno]] <- NA
  #               modlist$ModelInt[[modno]]   <- coef(modeloutput)[1]
  #             } else if (func == "cub"){
  #               modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput)) - 2]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
  #               modlist$ModelBetaQ[[modno]] <- coef(modeloutput)[length(coef(modeloutput)) - 1]
  #               modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
  #               modlist$ModelBetaC[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
  #               modlist$Std.ErrorC[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][4]
  #               modlist$ModelInt[[modno]]   <- coef(modeloutput)[1]
  #             } else if (func == "centre"){
  #               if(centre[[2]] == "both"){
  #                 modlist$WithinGrpMean[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
  #                 modlist$Std.ErrorMean[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
  #                 modlist$WithinGrpDev[[modno]]  <- coef(modeloutput)[length(coef(modeloutput)) - 1]
  #                 modlist$Std.ErrorDev[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
  #                 modlist$ModelInt[[modno]]      <- coef(modeloutput)[1]
  #               }
  #               if(centre[[2]] == "mean"){
  #                 modlist$WithinGrpMean[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
  #                 modlist$Std.ErrorMean[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
  #                 modlist$ModelInt[[modno]]      <- coef(modeloutput)[1]
  #               }
  #               if(centre[[2]] == "dev"){
  #                 modlist$WithinGrpDev[[modno]]  <- coef(modeloutput)[length(coef(modeloutput)) - 1]
  #                 modlist$Std.ErrorDev[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
  #                 modlist$ModelInt[[modno]]      <- coef(modeloutput)[1]
  #               }
  #             } else {
  #               modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))]
  #               modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
  #               modlist$ModelBetaQ[[modno]] <- NA
  #               modlist$ModelBetaC[[modno]] <- NA
  #               modlist$ModelInt[[modno]]   <- coef(modeloutput)[1]
  #             }
  #           }
  #         }
  #         modno <- modno + 1        #Increase modno#
  #       }
  #     }
  #   }  
  #   #Fill progress bar
  #   setTxtProgressBar(pb, modno - 1)
  # }
  
  #Save the best model output
  # m <- (modlist$WindowOpen[modlist$ModelAICc %in% min(modlist$ModelAICc)])
  # n <- (modlist$WindowOpen[modlist$ModelAICc %in% min(modlist$ModelAICc)]) - (modlist$WindowClose[modlist$ModelAICc %in% min(modlist$ModelAICc)]) + 1
  # windowopen  <- m[1] - range[2] + 1
  # windowclose <- windowopen - n[1] + 1
  # if (stat == "slope"){
  #   time      <- n[1]:1
  #   modeldat$climate <- apply(cmatrix[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
  # } else {
  #   ifelse (windowopen - windowclose == 0, 
  #           modeldat$climate <- cmatrix[, windowclose:windowopen], 
  #           modeldat$climate <- apply(cmatrix[, windowclose:windowopen], 1, FUN = stat))
  # }
  # 
  # if (!is.null(centre[[1]])){
  #   if (centre[[2]] == "both"){
  #     modeldat$WGdev   <- wgdev(modeldat$climate, centre[[1]])
  #     modeldat$WGmean  <- wgmean(modeldat$climate, centre[[1]])
  #     
  #     if(class(baseline)[1] == "coxph"){
  #       
  #       LocalModel <- my_update(modeloutput, .~., data = modeldat)
  #       
  #     } else {
  #       
  #       LocalModel <- update(modeloutput, .~., data = modeldat)
  #       
  #     }
  #     
  #   }
  #   if (centre[[2]] == "dev"){
  #     modeldat$WGdev   <- wgdev(modeldat$climate, centre[[1]])
  #     
  #     if(class(baseline)[1] == "coxph"){
  #       
  #       LocalModel <- my_update(modeloutput, .~., data = modeldat)
  #       
  #     } else {
  #       
  #       LocalModel <- update(modeloutput, .~., data = modeldat)
  #       
  #     }
  #     
  #   }
  #   if (centre[[2]] == "mean"){
  #     modeldat$WGmean  <- wgmean(modeldat$climate, centre[[1]])
  #     
  #     if(class(baseline)[1] == "coxph"){
  #       
  #       LocalModel <- my_update(modeloutput, .~., data = modeldat)
  #       
  #     } else {
  #       
  #       LocalModel <- update(modeloutput, .~., data = modeldat)
  #       
  #     }
  #     
  #   }
  #   modlist$Function <- "centre"
  # } else {
  #   
  #   if(class(baseline)[1] == "coxph"){
  #     
  #     LocalModel <- my_update(modeloutput, .~., data = modeldat)
  #     
  #   } else {
  #     
  #     LocalModel <- update(modeloutput, .~., data = modeldat)
  #     
  #   }
  #   
  #   modlist$Function <- func
  # }
  
  modlist$Function     <- func
  modlist$Furthest     <- range[1]
  modlist$Closest      <- range[2]
  modlist$Statistics   <- stat
  modlist$Type         <- type
  modlist$K            <- k
  modlist$ModWeight    <- (exp(-0.5 * modlist$deltaAICc)) / sum(exp(-0.5 * modlist$deltaAICc))
  modlist$sample.size  <- sample.size
  
  if (type == "absolute"){
    modlist$Reference.day   <- refday[1]
    modlist$Reference.month <- refday[2]
  }
  
  if(exists("coef_data")){
    
    modlist <- cbind(modlist, plyr::rbind.fill(coef_data))
    
  }
  
  if (nrandom == 0){
    if (is.null(centre[[1]]) == FALSE){
      LocalData         <- model.frame(LocalModel)
      LocalData$climate <- modeldat$climate
    } else {
      LocalData <- model.frame(LocalModel)
      
      if(attr(LocalModel, "class")[1] == "lme"){
        
        non_rand <- ncol(LocalData)
        
        LocalData <- cbind(LocalData, LocalModel$data[, colnames(LocalModel$fitted)[-which(colnames(LocalModel$fitted) %in% "fixed")]])
        
        colnames(LocalData)[-(1:non_rand)] <- colnames(LocalModel$fitted)[-which(colnames(LocalModel$fitted) %in% "fixed")]
        
      }
      
    }
    
    modlist$Randomised    <- "no"
    modlist               <- as.data.frame(modlist)
    LocalOutput           <- modlist[order(modlist$ModelAICc), ]
    LocalOutput$ModelAICc <-NULL
  }
  
  if (nrandom > 0){
    modlist$Randomised        <- "yes"
    modlist                   <- as.data.frame(modlist)
    LocalOutputRand           <- modlist[order(modlist$ModelAICc), ]
    LocalOutputRand$ModelAICc <- NULL
  }
  
  if (nrandom == 0){
    return(list(BestModel = LocalModel, BestModelData = LocalData, Dataset = LocalOutput))
  } else {
    return(LocalOutputRand)
  }
}