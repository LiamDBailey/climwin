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

slidingwin <- function(exclude = NA, xvar, cdate, bdate, baseline, 
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
    runs <- basewin(exclude = exclude, xvar = xvar[[paste(allcombos[combo, 1])]], cdate = cdate, bdate = bdate, baseline = baseline,
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
