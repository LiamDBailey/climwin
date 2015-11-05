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
#'@param xvar A list object containing all climate variables of interest. 
#'  Please specify the parent environment and variable name (e.g. Climate$Temp).
#'@param cdate The climate date variable (dd/mm/yyyy). Please specify the parent
#'  environment and variable name (e.g. Climate$Date).
#'@param bdate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for testing correlation. 
#'  Currently known to support lm, glm, lmer and glmer objects.
#'@param furthest The furthest number of time intervals (set by cinterval) back 
#'  from the cutoff date or biological record that will be included in the 
#'  climate window search.
#'@param closest The closest number of time intervals (set by cinterval) back 
#'  from the cutoff date or biological record that will be included in the 
#'  climate window search.
#'@param stat The aggregate statistic used to analyse the climate data. Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format function(x)
#'  (...). See FUN in \code{\link{apply}} for more detail.
#'@param func The functions used to fit the climate variable. Can be linear 
#'  ("lin"), quadratic ("quad"), cubic ("cub"), inverse ("inv") or log ("log").
#'@param type fixed or variable, whether you wish the climate window to be 
#'  variable (i.e. the number of days before each biological record is 
#'  measured) or fixed (i.e. number of days before a set point in time).
#'@param cutoff.day,cutoff.month If type is "fixed", the day and month of the 
#'  year from which the fixed window analysis will start.
#'@param cmissing TRUE or FALSE, determines what should be done if there are 
#'  missing climate data. If FALSE, the function will not run if missing 
#'  climate data is encountered. If TRUE, any records affected by missing 
#'  climate data will be removed from climate window analysis.
#'@param cinterval The resolution at which climate window analysis will be 
#'  conducted. May be days ("day"), weeks ("week"), or months ("month"). Note the units
#'  of parameters 'furthest' and 'closest' will differ depending on the choice
#'  of cinterval.
#'@param upper Cut-off values used to determine growing degree days or positive 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, climatewin will instead calculate an 
#'  optimal climate zone.
#'@param lower Cut-off values used to determine chill days or negative 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, climatewin will instead calculate an 
#'  optimal climate zone.
#'@param thresh TRUE or FALSE. Determines whether to use values of upper and
#'  lower to calculate binary climate data (thresh = TRUE), or to use for
#'  growing degree days (thresh = FALSE).
#'@param centre Variable used for mean centring (e.g. Year, Site, Individual).
#'  Please specify the parent environment and variable name (e.g. Biol$Year).
#'@return Will return a dataframe containing information on all fitted climate
#'  windows. See \code{\link{MassRand}} as an example.
#'@author Liam D. Bailey and Martijn van de Pol
#' @examples
#' \dontrun{
#'# Test climate windows for random data using Mass dataset
#' 
#'data(Mass)
#'data(MassClimate)
#' 
#'# Randomise data twice
#'# Note all other parameters are fitted in the same way as the climatewin function.
#' 
#'rand <- randwin(repeats = 2, xvar = list(Temp = MassClimate$Temp), 
#'                cdate = MassClimate$Date, bdate = Mass$Date,
#'                baseline = lm(Mass ~ 1, data = Mass), 
#'                furthest = 100, closest = 0,
#'                stat = "mean", func = "lin", type = "fixed", 
#'                cutoff.day = 20, cutoff.month = 5,
#'                cmissing = FALSE, cinterval = "day")
#'                 
#'# View output #
#' 
#'head(rand)    
#'        }
#'
#'@export

randwin <- function(exclude = NA, repeats = 1, xvar, cdate, bdate, baseline, 
                    stat, limits, func, type, refday,
                    cmissing = FALSE, cinterval = "day",
                    upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"), k = 0,
                    cutoff.day = NULL, cutoff.month = NULL,
                    furthest = NULL, closest = NULL, thresh = NULL, cvk = NULL){
  
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
    stop("furthest and closest are now redundant. Please use parameter 'limits' instead.")
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
  print("All combinations to be tested...")
  print(allcombos)
  
  combined <- list()
  for (combo in 1:nrow(allcombos)){
    for (r in 1:repeats){
      print (c("randomization number ", r))
      bdateNew        <- sample(bdate)
      outputrep <- basewin(exclude = exclude, xvar = xvar[[paste(allcombos[combo, 1])]], cdate = cdate, bdate = bdateNew, 
                           baseline = baseline, limits = limits, stat = paste(allcombos[combo, 3]), 
                           func = paste(allcombos[combo, 4]), type = paste(allcombos[combo, 2]),
                           refday = refday,
                           nrandom = repeats, cmissing = cmissing, cinterval = cinterval,
                           upper = ifelse(binarylevel == "two" || binarylevel == "upper", allcombos$upper[combo], NA),
                           lower = ifelse(binarylevel == "two" || binarylevel == "lower", allcombos$lower[combo], NA),
                           binary = paste(allcombos$binary[combo]), centre = centre, k = k)
      
      outputrep$Repeat <- r
      
      if(r == 1){ 
        outputrand <- outputrep 
      } else { 
        outputrand <- rbind(outputrand, outputrep)
      }
      rm(outputrep)
      if(r == repeats){
        outputrand                   <- as.data.frame(outputrand)
        combined[[combo]]            <- outputrand
        allcombos$Type               <- outputrand$Type[1]
        allcombos$AIC[combo]         <- round(outputrand$deltaAICc[1], digits = 2)
        allcombos$WindowOpen[combo]  <- outputrand$WindowOpen[1]
        allcombos$WindowClose[combo] <- outputrand$WindowClose[1]
        if(length(which("lin" == levels(allcombos$func))) >0){
          allcombos$betaL[combo] <- round(outputrand$ModelBeta[1], digits = 2)
        }
        if(allcombos$func[1] == "centre"){
          if(centre[[2]] == "both"){
            allcombos$WithinGrpMean <- round(outputrand$WithinGrpMean[1], digits = 2)
            allcombos$WithinGrpDev  <- round(outputrand$WithinGrpDev[1], digits = 2)
          }
          if(centre[[2]] == "dev"){
            allcombos$WithinGrpDev  <- round(outputrand$WithinGrpDev[1], digits = 2)
          }
          if(centre[[2]] == "mean"){
            allcombos$WithinGrpMean <- round(outputrand$WithinGrpMean[1], digits = 2)
          }
        }
        if(length(which("quad" == levels(allcombos$func))) > 0){
          allcombos$betaL[combo]   <- round(outputrand$ModelBeta[1], digits = 2)
          allcombos$betaQ[combo]   <- round(outputrand$ModelBetaQ[1], digits = 2)
        }
        if(length(which("cub" == levels(allcombos$func))) > 0){
          allcombos$betaL[combo]   <- round(outputrand$ModelBeta[1], digits = 2)
          allcombos$betaQ[combo]   <- round(outputrand$ModelBetaQ[1], digits = 2)
          allcombos$betaC[combo]   <- round(outputrand$ModelBetaC[1], digits = 2)
        }
        if(length(which("inv" == levels(allcombos$func))) > 0){
          allcombos$betaInv[combo] <- round(outputrand$ModelBeta[1], digits = 2)
        }
        if(length(which("log" == levels(allcombos$func))) > 0){
          allcombos$betaLog[combo] <- round(outputrand$ModelBeta[1], digits = 2)
        }
      }
    }
  }
  allcombos <- cbind(response = colnames(model.frame(baseline))[1], allcombos)
  combined <- c(combined, combos = list(allcombos))
  return(combined)
} 