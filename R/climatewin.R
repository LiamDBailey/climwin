#'  Test for a climate windows in data.
#'  
#'  Finds the time period when a biological variable is most strongly affected 
#'  by climate. Note that climate data and biological data should be loaded as 
#'  two seperate objects. Both objects should contain a date column to designate
#'  when the data were recorded (dd/mm/yyyy) and a response variable column.
#'  @param Xvars A list containing all climate variables of interest. Please specify the parent 
#'    environment and variable name (e.g. Climate$Temp).
#'  @param CDate The climate date variable (dd/mm/yyyy). Please specify the 
#'    parent environment and variable name (e.g. Climate$Date).
#'  @param BDate The biological date variable (dd/mm/yyyy). Please specify the 
#'    parent environment and variable name (e.g. Biol$Date).
#'  @param baseline The baseline model structure used for model testing. 
#'    Currently known to support lm, glm, lmer and glmer objects.
#'  @param furthest The furthest number of time intervals (set by CINTERVAL) 
#'    back from the cutoff date or biological record that will be included in
#'    the climate window search.
#'  @param closest The closest number of time intervals (set by CINTERVAL) back 
#'    from the cutoff date or biological record that will be included in the
#'    climate window search.
#'  @param STATS The aggregate statistics used to analyse the climate data. Can 
#'    currently use basic R statistics (e.g. mean, min), as well as slope. 
#'    Additional aggregate statistics can be created using the format 
#'    function(x) (...). See FUN in \code{\link{apply}} for more detail.
#'  @param FUNCS The functions used to fit the climate variable. Can be linear 
#'    ("L"), quadratic ("Q"), cubic ("C"), inverse ("I") or log ("LOG").
#'  @param FIXED TRUE or FALSE, whether you wish the climate window to be 
#'    variable (i.e. the number of days before each biological record is 
#'    measured) or fixed (i.e. number of days before a set point in time).
#'  @param cutoff.day,cutoff.month If FIXED is TRUE, the day and month of the 
#'    year from which the fixed window analysis will start.
#'  @param nrandom Used in conjunction with \code{\link{randwin}}. Not to be 
#'    changed manually.
#'  @param CMISSING TRUE or FALSE, determines what should be done if there are 
#'    missing climate data. If FALSE, the function will not run if missing 
#'    climate data is encountered. If TRUE, any records affected by missing 
#'    climate data will be removed from climate window analysis.
#'  @param CINTERVAL The resolution at which climate window analysis will be 
#'    conducted. May be days ("D"), weeks ("W"), or months ("M"). Note the units
#'    of parameters 'furthest' and 'closest' will differ depending on the choice
#'    of CINTERVAL.
#'  @param CVK The number of folds used for the k-fold cross validation. By default
#'    this value is set to 0, so no cross validation occurs. Value should be a
#'    minimum of 2 for cross validation to occur.
#'  @param uppers Cut-off values used to determine growing degree days or positive 
#'    climate thresholds (determined by parameter thresh). Note that when values
#'    of lowers and uppers are both provided, climatewin will instead calculate a 
#'    optimal climate zone (?).
#'  @param lowers Cut-off values used to determine chill days or negative 
#'    climate thresholds (determined by parameter thresh). Note that when values
#'    of lowers and uppers are both provided, climatewin will instead calculate a 
#'    optimal climate zone (?).
#'  @param thresh TRUE or FALSE. Determines whether to use values of uppers and
#'    lowers to calculate binary climate data (thresh = TRUE), or to use for
#'    growing degree days (thresh = FALSE).  
#'  @return Will return a list containing three objects:
#'    
#'    \itemize{ \item BestModel, a model object. The strongest climate window 
#'    model based on AICc. \item BestModelData, a dataframe with the data used 
#'    to fit the strongest climate window model. \item WindowOutput, a dataframe
#'    with information on all fitted climate windows. Ordered using AICc model
#'    strength, with lowest AICc value first. See \code{\link{MassOutput}} as an
#'    example. }
#'  @author Liam D. Bailey and Martijn van de Pol
#'  @importFrom MuMIn AICc
#'  @import lme4
#'  @import stats  
#'  @examples
#'  \dontrun{
#'  ##EXAMPLE 1## 
#'  
#'  # Test for a variable climate window using datasets "Offspring"
#'  # and "OffspringClimate".
#'  
#'  # Load data.
#'  
#'  data(Offspring) 
#'  data(OffspringClimate)
#'  
#'  # Test both linear and quadratic functions with climate variable temperature
#'  
#'  OffspringWin <- climatewin(Xvars = list(Temp = OffspringClimate$Temperature), 
#'                             CDate = OffspringClimate$Date, 
#'                             BDate = Offspring$Date, 
#'                             baseline = glm(Offspring ~ 1, data = Offspring, family = poisson),
#'                             furthest = 150, closest = 0, 
#'                             FIXED = FALSE, STATS = "mean", 
#'                             FUNCS = c("L", "Q"), CMISSING = FALSE, CINTERVAL = "D")
#'  
#'  # View output testing linear relationship of temperature
#'  
#'  head(OffspringWin[[1]]$Dataset) 
#'  summary(OffspringWin[[1]]$BestModel)
#'  
#'  ##EXAMPLE 2##
#'  
#'  # Test for a fixed climate window using datasets 'Mass' and 'MassClimate'.
#'  
#'  # Load data.
#'  
#'  data(Mass)
#'  data(MassClimate)
#'  
#'  # Test a fixed window, starting 20 May
#'  # Test for climate windows between 100 and 0 days ago (furthest = 100, closest = 0)
#'  # Fit a linear term for the mean climate (FUNC = "L")
#'  # Test at the resolution of days (CINTERVAL = "D")
#'  # Test using both mean and max aggregate statistics
#'  
#'  MassWin <- climatewin(Xvars = list(Temp = MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date,
#'                        baseline = lm(Mass ~ 1, data = Mass),
#'                        furthest = 100, closest = 0,
#'                        STATS = c("mean", "max"), FUNCS = "L",
#'                        FIXED = TRUE, cutoff.day = 20, cutoff.month = 5,
#'                        nrandom = 0, CMISSING = FALSE, CINTERVAL = "D")
#'  
#'  # View output for mean temperature
#'  
#'  head(MassWin[[1]]$Dataset)
#'  summary(MassWin[[1]]$BestModel)
#'  }
#'  
#'  @export

#LAST EDITED: 08/07/2015
#EDITED BY: LIAM
#NOTES: Integrated cross validation into main climatewin function

climatewin <- function(Xvars, CDate, BDate, baseline, furthest, closest, 
                       FIXED, cutoff.day, cutoff.month, STATS = "mean", FUNCS = "L",
                       CMISSING = FALSE, CINTERVAL = "D",  nrandom = 0, CVK = 0,
                       uppers = NA, lowers = NA, thresh = FALSE){
  
  #Make Xvars a list where the name of list object is the climate variable (e.g. Rain, Temp)
  
  if(is.null(names(Xvars)) == TRUE){
    numbers <- seq(1, length(Xvars), 1)
    for(Xname in 1:length(Xvars)){
      names(Xvars)[Xname] = paste("climate", numbers[Xname])
    }
  }
  
  if(is.na(uppers) == FALSE && is.na(lowers) == FALSE){
    combos <- expand.grid(list(uppers = uppers, lowers = lowers))
    combos <- combos[which(combos$uppers >= combos$lowers), ]
    allcombos <- expand.grid(list(Climate = names(Xvars), Stat = STATS, Func = FUNCS, gg = c(1:nrow(combos)), Thresh = thresh))
    allcombos <- cbind(allcombos, combos[allcombos$gg, ], deparse.level = 2)
    allcombos$gg <- NULL
    threshlevel <- "two"
  } else if(is.na(uppers) == FALSE && is.na(lowers) == TRUE){
    allcombos <- expand.grid(list(Climate = names(Xvars), Stat = STATS, Func = FUNCS, uppers = uppers, Thresh = thresh))
    threshlevel <- "upper"
  } else if(is.na(uppers) == TRUE && is.na(lowers) == FALSE){
    allcombos <- expand.grid(list(Climate = names(Xvars), Stat = STATS, Func = FUNCS, lowers = lowers, Thresh = thresh))
    threshlevel <- "lower"
  } else if(is.na(uppers) == TRUE && is.na(lowers) == TRUE){
    allcombos <- expand.grid(list(Climate = names(Xvars), Stat = STATS, Func = FUNCS))
    threshlevel <- "none"
  }
  
  combined <- list()
  for(combo in 1:nrow(allcombos)){
    runs <- basewin(Xvar = Xvars[[paste(allcombos[combo, 1])]], CDate = CDate, BDate = BDate, baseline = baseline,
                    furthest = furthest, closest = closest, FIXED = FIXED, cutoff.day = cutoff.day,
                    cutoff.month = cutoff.month, STAT = paste(allcombos[combo, 2]), FUNC = paste(allcombos[combo, 3]),
                    CMISSING = CMISSING, CINTERVAL = CINTERVAL, CVK = CVK, 
                    upper = ifelse(threshlevel == "two" || threshlevel == "upper", allcombos$uppers[combo], NA),
                    lower = ifelse(threshlevel == "two" || threshlevel == "lower", allcombos$lowers[combo], NA),
                    thresh = paste(allcombos$Thresh[combo]))
    combined[[combo]] <- runs
  }
  combined <- c(combined, combos = list(allcombos))
  return(combined)
}
