#'  Test for a climate window
#'  
#'  Finds the time period when a biological variable is most strongly affected 
#'  by climate. Note that climate data and biological data should be loaded as 
#'  two seperate objects. Both objects should contain a date column to designate
#'  when the data were recorded (dd/mm/yyyy) and a response variable column.
#'  @param Xvar The climate variable of interest. Please specify the parent 
#'    environment and variable name (e.g. Climate$Temp).
#'  @param CDate The climate date variable (dd/mm/yyyy). Please specify the 
#'    parent environment and variable name (e.g. Climate$Date).
#'  @param BDate The biological date variable (dd/mm/yyyy). Please specify the 
#'    parent environment and variable name (e.g. Biol$Date).
#'  @param baseline The baseline model structure used for testing correlation. 
#'    Currently known to support lm, glm, lmer and glmer objects.
#'  @param furthest The furthest number of time intervals (set by CINTERVAL) 
#'    back from the cutoff date or biological record that you want to include in
#'    your climate window search.
#'  @param closest The closest number of time intervals (set by CINTERVAL) back 
#'    from the cutoff date or biological record that you want to include in your
#'    climate window search.
#'  @param STAT The aggregate statistic used to analyse the climate data. Can 
#'    currently use basic R statistics (e.g. mean, min), as well as slope. 
#'    Additional aggregate statistics can be created using the format 
#'    function(x) (...). See FUN in \code{\link{apply}} for more detail.
#'  @param FUNC The function used to fit the climate variable. Can be linear 
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
#'  OffspringWin <- climatewin(Xvar = OffspringClimate$Temperature, 
#'                             CDate = OffspringClimate$Date, 
#'                             BDate = Offspring$Date, 
#'                             baseline = glm(Offspring$Offspring ~ 1, family = poisson),
#'                             furthest = 150, closest = 0, 
#'                             FIXED = FALSE, STAT = "mean", 
#'                             FUNC = "L", CMISSING = FALSE, CINTERVAL = "D")
#'  
#'  # View output.
#'  
#'  head(OffspringWin[[3]]) 
#'  OffspringWin[[1]]
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
#'  
#'  MassWin <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
#'                        baseline = lm(Mass$Mass ~ 1),
#'                        furthest = 100, closest = 0,
#'                        STAT = "mean", FUNC = "L",
#'                        FIXED = TRUE, cutoff.day = 20, cutoff.month = 5,
#'                        nrandom = 0, CMISSING = FALSE, CINTERVAL = "D")
#'  
#'  # View output
#'  
#'  head(MassWin[[3]])
#'  MassWin[[1]]
#'  }
#'  
#'  @export

#LAST EDITED: 17/02/2015
#EDITED BY: LIAM
#NOTES: Tidy up code
# 04/02/2015 Martijn fixed FUNC="I" option
# renamed Modelwindowstart->WIndowOpen & ModelwindowEnd->WIndowClose

# when FIXED=TRUE and season encompasses two calender years (e.g. oct 2013 to Jan 2014) 
# then cutoff.day and cutoff.month should chosen to be larger then the closest Biol$Date

climatewin <- function(Xvar, CDate, BDate, baseline, furthest, closest, 
                       FIXED, cutoff.day, cutoff.month, STAT = "mean", FUNC = "L",
                       CMISSING = FALSE, CINTERVAL = "D",  nrandom = 0){
  
  print("Initialising, please wait...")
  pb        <- SetProgressBar(furthest, closest, STAT)    # Calculate the number of models to run 
  cont      <- DateConverter(BDate = BDate, CDate = CDate, Xvar = Xvar, 
                             CINTERVAL = CINTERVAL, FIXED = FIXED, 
                             cutoff.day = cutoff.day, cutoff.month = cutoff.month)   # create new climate dataframe with continuous daynumbers, leap days are not a problem
  MODNO     <- 1  #Create a model number variable that will count up during the loop#
  duration  <- (furthest - closest) + 1
  CMatrix   <- matrix(ncol = (duration), nrow = length(BDate))  # matrix that stores the weather data for variable or fixed windows
  MODLIST   <- list()   # dataframes to store ouput
  baseline  <- update(baseline, .~.)
  nullmodel <- AICc(baseline)
  
  modeldat      <- model.frame(baseline)
  modeldat$Yvar <- modeldat[, 1]
  
  if(length(modeldat$Yvar) != length(BDate)){
    stop("NA values present in biological response. Please remove NA values")
  }
  
  for (i in 1:length(BDate)){
   for (j in closest:furthest){
     k <- j - closest + 1
     CMatrix[i, k] <- cont$Xvar[which(cont$CIntNo == cont$BIntNo[i] - j)]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
   }
  }

  if (CMISSING == FALSE && length(which(is.na(CMatrix))) > 0){
    .GlobalEnv$Missing <- as.Date(cont$CIntNo[is.na(cont$Xvar)], origin = min(as.Date(CDate, format = "%d/%m/%Y")) - 1)
    stop(c("Climate data should not contain NA values: ", length(.GlobalEnv$Missing),
           " NA value(s) found. Please add missing climate data or set CMISSING=TRUE.
           See object Missing for all missing climate data"))
  }
  
  if (CMISSING == TRUE && length(which(is.na(CMatrix))) > 0){
    modeldat      <- modeldat[complete.cases(CMatrix), ]
    baseline      <- update(baseline, Yvar~., data = modeldat)
    CMatrix       <- CMatrix[complete.cases(CMatrix), ]
  }
  
  modeldat           <- model.frame(baseline)
  modeldat$temporary <- matrix(ncol = 1, nrow = nrow(CMatrix), seq(from = 1, to = nrow(CMatrix), by = 1))
  
  if (FUNC == "L"){
    modeloutput <- update(baseline, .~. + temporary, data = modeldat)
  } else if (FUNC == "Q") {
    modeloutput <- update(baseline, .~. + temporary + I(temporary ^ 2), data = modeldat)
  } else if (FUNC == "C") {
    modeloutput <- update(baseline, .~. + temporary + I(temporary ^ 2) + I(temporary ^ 3), data = modeldat)
  } else if (FUNC == "LOG") {
    modeloutput <- update(baseline, .~. + log(temporary), data = modeldat)
  } else if (FUNC == "I") {
    modeloutput <- update (baseline, .~. + I(temporary ^ -1), data = modeldat)
  } else {
    print("DEFINE FUNC")
  }

  #CREATE A FOR LOOP TO FIT DIFFERENT CLIMATE WINDOWS#
  for (m in closest:furthest){
    for (n in 1:duration){
      if ( (m - n) >= (closest - 1)){  # do not use windows that overshoot the closest possible day in window   
        if (STAT != "slope" || n > 1){
          windowopen  <- m - closest + 1
          windowclose <- windowopen - n + 1
          if (STAT == "slope"){ 
              time               <- seq(1, n, 1)
              modeldat$temporary <- apply(CMatrix[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
          } else { 
            ifelse (n == 1, modeldat$temporary <- CMatrix[, windowclose:windowopen], modeldat$temporary <- apply(CMatrix[, windowclose:windowopen], 1, FUN = STAT))
          }
          # run the model
          modeloutput <- update(modeloutput, .~.) 
          #Add model parameters to list#
          MODLIST$ModelAICc[[MODNO]]   <- AICc(modeloutput)
          MODLIST$WindowOpen[[MODNO]]  <- m
          MODLIST$WindowClose[[MODNO]] <- m - n + 1
          
          if(length(attr(class(modeloutput),"package")) > 0 && attr(class(modeloutput), "package") == "lme4"){
            MODLIST$ModelBeta[[MODNO]] <- fixef(modeloutput)[2]
            MODLIST$ModelInt[[MODNO]]  <- fixef(modeloutput)[1]
            
            if (FUNC == "Q"){
              MODLIST$ModelBetaQ[[MODNO]] <- fixef(modeloutput)[3]
            }   
            
            if (FUNC == "C"){
              MODLIST$ModelBetaQ[[MODNO]] <- fixef(modeloutput)[3]    
              MODLIST$ModelBetaC[[MODNO]] <- fixef(modeloutput)[4]
            }
          } else {
          MODLIST$ModelBeta[[MODNO]]   <- coef(modeloutput)[2]   # add one coef if quadratic
          MODLIST$ModelInt[[MODNO]]    <- coef(modeloutput)[1]   # add one coef if quadratic
          
          if (FUNC == "Q"){
            MODLIST$ModelBetaQ[[MODNO]] <- coef(modeloutput)[3]
          }   
          
          if (FUNC == "C"){
            MODLIST$ModelBetaQ[[MODNO]] <- coef(modeloutput)[3]    
            MODLIST$ModelBetaC[[MODNO]] <- coef(modeloutput)[4]
          }
          }
          MODNO <- MODNO + 1        #Increase ModNo#
        }
      }
    }  
    #Fill progress bar
    setTxtProgressBar(pb, MODNO - 1)
  }
  #Save the best model output
  m <- (MODLIST$WindowOpen[MODLIST$ModelAICc %in% min(MODLIST$ModelAICc)])
  n <- (MODLIST$WindowOpen[MODLIST$ModelAICc %in% min(MODLIST$ModelAICc)]) - (MODLIST$WindowClose[MODLIST$ModelAICc %in% min(MODLIST$ModelAICc)]) + 1
  windowopen  <- m[1] - closest + 1
  windowclose <- windowopen - n[1] + 1
  if(STAT == "slope"){
    time      <- seq(1, n[1], 1)
    modeldat$temporary <- apply(CMatrix[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
  } else {
    ifelse (windowopen - windowclose == 0, modeldat$temporary <- CMatrix[, windowclose:windowopen], modeldat$temporary <- apply (CMatrix[, windowclose:windowopen], 1, FUN = STAT))
  }
  LocalModel           <- update(modeloutput, .~.)
  MODLIST$baselineAICc <- AICc(baseline)
  MODLIST$furthest     <- furthest
  MODLIST$closest      <- closest
  MODLIST$Statistics   <- STAT
  MODLIST$Function     <- FUNC
  MODLIST$FIXED        <- FIXED
  
  if (FIXED == TRUE){
    MODLIST$cutoff.day   <- cutoff.day
    MODLIST$cutoff.month <- cutoff.month
  }
  
  if (nrandom == 0){
    LocalData           <- model.frame(LocalModel)
    MODLIST$Randomised  <- "no"
    MODLIST             <- as.data.frame(MODLIST)
    LocalOutput         <- MODLIST[order(MODLIST$ModelAICc), ]
  }
  
  if (nrandom > 0){
    MODLIST$Randomised <- "yes"
    MODLIST            <- as.data.frame (MODLIST)
    LocalOutputRand    <- MODLIST[order(MODLIST$ModelAICc), ]
    }
  
  if (nrandom == 0){
    return(list(BestModel = LocalModel, BestModelData = LocalData, WindowOutput = LocalOutput))
  } else {
    return(LocalOutputRand)
  }
}