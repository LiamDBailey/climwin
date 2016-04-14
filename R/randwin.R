#'Climate window analysis for randomised data
#'
#'Will randomise biological data and carry out a climate window analysis. Used
#'to help determine the chance of obtaining an observed result at random.
#'@param exclude Two values (distance and duration) which allow users
#'  to exclude short-duration long-lag climate windows from analysis (e.g., 
#'  windows with a duration of 10 days which occur over a month ago).
#'  These windows are often considered to be biologically implausible.
#'@param repeats The number of times that data will be randomised and analysed 
#'  for climate windows.
#'@param window Whether randomisations are carried out for a sliding window ("Sliding")
#'  or weighted window ("Weighted") approach.
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
#'@param cmissing TRUE or FALSE, determines what should be done if there are 
#'  missing climate data. If FALSE, the function will not run if missing 
#'  climate data is encountered. If TRUE, any records affected by missing 
#'  climate data will be removed from climate window analysis.
#'@param cinterval The resolution at which climate window analysis will be 
#'  conducted. May be days ("day"), weeks ("week"), or months ("month"). Note the units
#'  of parameters 'furthest' and 'closest' will differ depending on the choice
#'  of cinterval.
#'@param k The number of folds used for k-fold cross validation. By default
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
#'  only within-group means ("mean"), or only within-group variance ("var").
#'@param cutoff.day,cutoff.month Redundant parameters. Now replaced by refday.
#'@param furthest,closest Redundant parameters. Now replaced by range.
#'@param thresh Redundant parameter. Now replaced by binary.
#'@param cvk Redundant parameter. Now replaced by k.
#'@return Will return a dataframe containing information on all fitted climate
#'  windows. See \code{\link{MassRand}} as an example.
#'@author Liam D. Bailey and Martijn van de Pol
#' @examples
#' \dontrun{
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
#'rand <- randwin(repeats = 2, window = "Sliding", 
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
#'weightrand <- weightwin(repeats = 2, window = "Weighted", 
#'                        xvar = list(Temp = OffspringClimate$Temperature), 
#'                        cdate = OffspringClimate$Date,
#'                        bdate = Offspring$Date,
#'                        baseline = glm(Offspring ~ 1, family = poisson, data = Offspring),
#'                        range = c(365, 0), func = "quad",
#'                        type = "relative", weightfunc = "W", cinterval = "day",
#'                        par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
#'                        method = "L-BFGS-B")
#'                    
#'# View output
#'
#'head(weightrand)
#'                           
#'        }
#'
#'@export

randwin <- function(exclude = NA, repeats = 5, window = "Sliding", xvar, cdate, bdate, baseline, 
                    stat, range, func, type, refday,
                    cmissing = FALSE, cinterval = "day",
                    upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"), k = 0,
                    weightfunc = "W", par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)), 
                    method = "L-BFGS-B", cutoff.day = NULL, cutoff.month = NULL,
                    furthest = NULL, closest = NULL, thresh = NULL, cvk = NULL,
                    spatial = NULL, cohort = NULL){
  
  #Create a centre function that over-rides quadratics etc. when centre != NULL
  if(is.null(centre[[1]]) == FALSE){
    func = "centre"
  }
  
  if(is.null(cvk) == FALSE){
    stop("Parameter 'cvk' is now redundant. Please use parameter 'k' instead.")
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
  
  if (is.null(names(xvar)) == TRUE){
    numbers <- seq(1, length(xvar), 1)
    for (xname in 1:length(xvar)){
      names(xvar)[xname] = paste("climate", numbers[xname])
    }
  }
  
  if(is.null(cutoff.day) == FALSE & is.null(cutoff.month) == FALSE){
    stop("cutoff.day and cutoff.month are now redundant. Please use parameter 'refday' instead.")
  }
  
  if(window == "Sliding"){
    
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
    
  } else if(window == "Weighted"){
    
    if (is.na(upper) == FALSE && is.na(lower) == FALSE){
      combos       <- expand.grid(list(upper = upper, lower = lower))
      combos       <- combos[which(combos$upper >= combos$lower), ]
      allcombos    <- expand.grid(list(climate = names(xvar), type = type, stat = NA, func = func, gg = c(1:nrow(combos)), binary = binary))
      allcombos    <- cbind(allcombos, combos[allcombos$gg, ], deparse.level = 2)
      binarylevel  <- "two"
      allcombos$gg <- NULL
    } else if (is.na(upper) == FALSE && is.na(lower) == TRUE){
      allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = NA, func = func, upper = upper, lower = lower, binary = binary))
      binarylevel <- "upper"
    } else if (is.na(upper) == TRUE && is.na(lower) == FALSE){
      allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = NA, func = func, upper = upper, lower = lower, binary = binary))
      binarylevel <- "lower"
    } else if (is.na(upper) == TRUE && is.na(lower) == TRUE){
      allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = NA, func = func))
      binarylevel <- "none"
    }
    
  }
  
  rownames(allcombos) <- seq(1, nrow(allcombos), 1)
  print("All combinations to be tested...")
  print(allcombos)
  
  combined <- list()
  for (combo in 1:nrow(allcombos)){
    for (r in 1:repeats){
      print (c("randomization number ", r))
      bdateNew  <- sample(bdate)
      
      if(window == "Sliding"){
        
        outputrep <- basewin(exclude = exclude, xvar = xvar[[paste(allcombos[combo, 1])]], cdate = cdate, bdate = bdateNew, 
                             baseline = baseline, range = range, stat = paste(allcombos[combo, 3]), 
                             func = paste(allcombos[combo, 4]), type = paste(allcombos[combo, 2]),
                             refday = refday,
                             nrandom = repeats, cmissing = cmissing, cinterval = cinterval,
                             upper = ifelse(binarylevel == "two" || binarylevel == "upper", allcombos$upper[combo], NA),
                             lower = ifelse(binarylevel == "two" || binarylevel == "lower", allcombos$lower[combo], NA),
                             binary = paste(allcombos$binary[combo]), centre = centre, k = k, spatial = spatial,
                             cohort = cohort)
        
        outputrep$Repeat <- r
        WeightDist <- sum(as.numeric(cumsum(outputrep$ModWeight) <= 0.95))/nrow(outputrep)
        outputrep <- outputrep[1, ]
        outputrep$WeightDist <- WeightDist
        
        if(r == 1){ 
          outputrand <- outputrep
        } else { 
          outputrand <- rbind(outputrand, outputrep)
        }
        
      } else if(window == "Weighted"){
        
        rep <- weightwin(xvar = xvar[[paste(allcombos[combo, 1])]], cdate = cdate, bdate = bdateNew, 
                         baseline = baseline, range = range, func = paste(allcombos[combo, 4]), 
                         type = paste(allcombos[combo, 2]), refday = refday,
                         nrandom = repeats, cinterval = cinterval,
                         centre = centre, spatial = spatial,
                         cohort = cohort, weightfunc = weightfunc, par = par, 
                         control = control, method = method)
        
        outputrep <- rep$WeightedOutput
        outputrep$Repeat <- r
        
        if(r == 1){ 
          outputrand <- outputrep 
        } else { 
          outputrand <- rbind(outputrand, outputrep)
        }
      }

      rm(outputrep)
      if(r == repeats){          
          outputrand                   <- as.data.frame(outputrand)
          combined[[combo]]            <- outputrand
        }
      }
    }
  allcombos <- cbind(response = colnames(model.frame(baseline))[1], allcombos)
  combined <- c(combined, combos = list(allcombos))
  return(combined)
  }