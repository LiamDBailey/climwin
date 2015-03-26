#'Fit a single climate window
#'
#'Fit a single climate window with a known start and end time.
#'@param Xvar The climate variable of interest. Please specify the parent 
#'  environment and variable name (e.g. Climate$Temp).
#'@param CDate The climate date variable (dd/mm/yyyy). Please specify the parent
#'  environment and variable name (e.g. Climate$Date).
#'@param BDate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for testing correlation. 
#'  Currently known to support lm, glm, lmer and glmer objects.
#'@param furthest The start day of the single window.
#'@param closest The end day of the single window.
#'@param STAT The aggregate statistic used to analyse the climate data. Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format function(x)
#'  (...). See FUN in \code{\link{apply}} for more detail.
#'@param FUNC The function used to fit the climate variable in the model. Can be
#'  linear ("L"), quadratic ("Q"), cubic ("C"), inverse ("I") or log ("LOG").
#'@param FIXED TRUE or FALSE, whether you wish the climate window to be variable
#'  (i.e. the number of days before each biological record is measured) or fixed
#'  (i.e. number of days before a set point in time).
#'@param cutoff.day,cutoff.month If FIXED is TRUE, the day and month of the year
#'  from which the fixed window analysis will start.
#'@param CMISSING TRUE or FALSE, determines what should be done if there are 
#'  missing climate data. If FALSE, the function will not run if missing climate
#'  data is encountered. If TRUE, any records affected by missing climate data 
#'  will be removed from climate window analysis.
#'@param CINTERVAL The resolution at which climate window analysis will be 
#'  conducted. May be days ("D"), weeks ("W"), or months ("M"). Note the units 
#'  of parameters 'furthest' and 'closest' will differ depending on the choice 
#'  of CINTERVAL.
#'@return Will return a list containing two objects:
#'  
#'  \itemize{ \item SingleBestModel, a model object of the fitted climate window
#'  model.
#'  
#'  \item SingleBestModelData, a dataframe with the biological and climate data
#'  used to fit the climate window model.}
#'  
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'\dontrun{
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
#'single <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
#'                    baseline = lm(Mass ~ 1, data = Mass), furthest = 72, closest = 15,
#'                    STAT = "mean", FUNC = "L",
#'                    FIXED = TRUE, cutoff.day = 20, cutoff.month = 5,
#'                    CMISSING = FALSE, CINTERVAL = "D")
#'                
#'##View data##
#'single[[1]]
#'head(single[[2]])
#'}
#'
#'@importFrom MuMIn AICc
#'@importFrom lubridate year
#'@importFrom lubridate month
#'@export
              
#LAST EDITED: 19/02/2015
#EDITED BY: LIAM
#NOTES: TIDY CODE

#Plotting part needs fixing

singlewin <- function(Xvar, CDate, BDate, baseline, 
                      furthest, closest, STAT, FUNC, 
                      FIXED, cutoff.day, cutoff.month, 
                      CMISSING = FALSE, CINTERVAL = "D"){
  
  baseline  <- update(baseline, .~.)
  nullmodel <- AICc(baseline)  
  duration  <- (furthest-closest) + 1
  MODLIST   <- list()   # dataframes to store ouput
  CMatrix   <- matrix(ncol = (duration), nrow = length(BDate))  # matrix that stores the weather data for variable or fixed windows
  # create new climate dataframe with continuous daynumbers, leap days are not a problem
  
  BDate <- as.Date(BDate, format = "%d/%m/%Y")
  CDate2 <- seq(min(as.Date(CDate, format = "%d/%m/%Y")), max(as.Date(CDate, format = "%d/%m/%Y")), "days") # Convert the date variables into the R date format
  CDate <- as.Date(CDate, format = "%d/%m/%Y") #Convert the date variables into the R date format#
  
  Xvar <- Xvar[match(CDate2, CDate)]
  CIntNo     <- as.numeric(CDate2) - min(as.numeric(CDate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to CIntNo 1
  RealBIntNo <- as.numeric(BDate) - min(as.numeric(CDate2)) + 1
  
  if (length(CIntNo) != length(unique(CIntNo))){
    stop ("There are duplicate dayrecords in climate data")
  }
  
  if (CINTERVAL != "D" && CINTERVAL != "W" && CINTERVAL != "M"){
    stop("CINTERVAL should be either D, W or M")
  }
  
  if (CINTERVAL == "D"){  
    if (FIXED == TRUE){   
      BIntNo            <- as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(BDate), sep = "-"), 
                                              format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1 
      wrongyear         <- which(BIntNo<RealBIntNo)
      BIntNo[wrongyear] <- (as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(BDate[wrongyear]) + 1), sep = "-"), 
                                               format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1)
    } else {
      BIntNo <- RealBIntNo
    }
  } else if (CINTERVAL == "W"){
    CIntNo     <- ceiling((as.numeric(CDate) - min(as.numeric(CDate)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to CIntNo 1
    RealBIntNo <- ceiling((as.numeric(BDate) - min(as.numeric(CDate)) + 1) / 7)
    NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar)
    NewClim2   <- melt(NewClim, id = "CIntNo")
    NewClim3   <- cast(NewClim2, CIntNo ~ variable, mean)
    CIntNo     <- NewClim3$CIntNo
    Xvar       <- NewClim3$Xvar
    if(FIXED == TRUE){ 
      BIntNo            <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(BDate), sep = "-"), 
                                                       format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1) / 7) 
      wrongyear         <- which(BIntNo<RealBIntNo)
      BIntNo[wrongyear] <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(BDate[wrongyear]) + 1), sep = "-"), 
                                                       format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1) / 7)  
    } else {
      BIntNo <- RealBIntNo
    }
  } else if (CINTERVAL == "M"){
    Cmonth     <- month(CDate)
    Cyear      <- year(CDate) - min(year(CDate))
    CIntNo     <- Cmonth + 12 * Cyear
    RealBIntNo <- month(BDate) + 12 * (year(BDate) - min(year(CDate)))
    NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar)
    NewClim2   <- melt(NewClim, id = "CIntNo")
    NewClim3   <- cast(NewClim2, CIntNo ~ variable, mean)
    CIntNo     <- NewClim3$CIntNo
    Xvar       <- NewClim3$Xvar
    if (FIXED == TRUE){ 
      BIntNo            <- cutoff.month + 12 * (year(BDate) - min(year(CDate)))
      wrongyear         <- which(BIntNo < RealBIntNo)
      BIntNo[wrongyear] <- cutoff.month + 12 * (year(BDate[wrongyear]) + 1 - min(year(CDate)))
    } else {
      BIntNo <- RealBIntNo
    }
  }
  
  for (i in 1:length(BDate)){
    for (j in closest:furthest){
      k <- j - closest + 1
      CMatrix[i, k] <- Xvar[which(CIntNo == BIntNo[i] - j)]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  }
  
  if (CMISSING == FALSE && length(which(is.na(CMatrix))) > 0){
    .GlobalEnv$Missing <- as.Date(cont$CIntNo[is.na(cont$Xvar)], origin = min(as.Date(CDate, format = "%d/%m/%Y")) - 1)
    stop(c("Climate data should not contain NA values: ", length(.GlobalEnv$Missing),
           " NA value(s) found. Please add missing climate data or set CMISSING=TRUE.
           See object Missing for all missing climate data"))
  }
  
  if (CMISSING == TRUE && length(which(is.na(CMatrix))) > 0){ 
    modeldat      <- model.frame(baseline)
    modeldat$Yvar <- modeldat[, 1]
    modeldat      <- modeldat[complete.cases(CMatrix), ]
    baseline      <- update(baseline, Yvar~., data = modeldat)
    CMatrix       <- CMatrix[complete.cases(CMatrix), ]
  }  
  
  
  modeldat           <- model.frame(baseline)
  modeldat$temporary <- matrix(ncol = 1, nrow = nrow(CMatrix), seq(from = 1, to = nrow(CMatrix), by = 1))
  
  #.GlobalEnv$temporary <- matrix(ncol = 1, nrow = nrow(CMatrix), seq(from=1,to=nrow(CMatrix), by=1))
  
  if (FUNC == "L"){
    modeloutput <- update(baseline, .~. + temporary, data = modeldat)
  } else if(FUNC == "Q"){
    modeloutput <- update(baseline, .~. + temporary + I(temporary^2), data = modeldat)
  } else if(FUNC == "C"){
    modeloutput <- update(baseline, .~. + temporary + I(temporary^2) + I(temporary^3), data = modeldat)
  } else if(FUNC == "LOG"){
    modeloutput <- update(baseline, .~. + log(temporary), data = modeldat)
  } else if(FUNC == "I"){
    modeloutput <- update(baseline, .~. + I(temporary ^ -1), data = modeldat)
  } else {
    print("DEFINE FUNC")
  }
  #CREATE A FOR LOOP TO FIT DIFFERENT CLIMATE WINDOWS#
  MODNO <- 1  #Create a model number variable that will count up during the loop#
  m     <- closest
  n     <- duration
  if ( (m-n) >= (closest - 1)){  # do not use windows that overshoot the closest possible day in window   
    if (STAT != "slope" || n > 1){
      if (STAT == "slope"){
        time               <- seq(1, n, 1)
        modeldat$temporary <- apply(CMatrix, 1, FUN = function(x) coef(lm(x ~ time))[2])
      } else {
        ifelse (n == 1, modeldat$temporary <- CMatrix, modeldat$temporary <- apply(CMatrix, 1, FUN = STAT))
      }    
      # run the model
      modeloutput <- update(modeloutput, .~.) 
      #Add model parameters to list#
      MODLIST$ModelAICc[[MODNO]]    <- AICc(modeloutput)
      MODLIST$WindowOpen[[MODNO]]   <- m
      MODLIST$WindowClose[[MODNO]]  <- m - n + 1
      MODLIST$ModelBeta[[MODNO]]    <- coef(modeloutput)[2]   # add one coef if quadratic
      if (FUNC == "Q" ){
        MODLIST$ModelBetaQ[[MODNO]] <- coef(modeloutput)[3]
      }   
      if(FUNC == "C" ){
        MODLIST$ModelBetaQ[[MODNO]] <- coef(modeloutput)[3]    
        MODLIST$ModelBetaC[[MODNO]] <- coef(modeloutput)[4]
      }   
      MODNO <- MODNO + 1        #Increase ModNo#
    }
  }
  #Save the best model output
  if (STAT == "slope"){
    time               <- seq(1, n, 1)
    modeldat$temporary <- apply(CMatrix, 1, FUN = function(x) coef(lm(x ~ time))[2])
  } else {
    ifelse(n == 1, modeldat$temporary <- CMatrix, modeldat$temporary <- apply(CMatrix, 1, FUN = STAT))
  }
  LocalBestModel      <- update(modeloutput, .~.)
  LocalData           <- model.frame(LocalBestModel)
  return(list(SingleBestModel = LocalBestModel, SingleBestModelData = LocalData))
}