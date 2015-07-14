#'Fit a single climate window
#'
#'Fit a single climate window with a known start and end time.
#'@param Xvar The climate variable of interest. Please specify the parent 
#'  environment and variable name (e.g. Climate$Temp).
#'@param Cdate The climate date variable (dd/mm/yyyy). Please specify the parent
#'  environment and variable name (e.g. Climate$Date).
#'@param Bdate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for testing correlation. 
#'  Currently known to support lm, glm, lmer and glmer objects.
#'@param furthest The start day of the single window.
#'@param closest The end day of the single window.
#'@param stat The aggregate statistic used to analyse the climate data. Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format function(x)
#'  (...). See FUN in \code{\link{apply}} for more detail.
#'@param func The function used to fit the climate variable in the model. Can be
#'  linear ("lin"), quadratic ("quad"), cubic ("cub"), inverse ("inv") or log ("log").
#'@param type fixed or variable, whether you wish the climate window to be variable
#'  (i.e. the number of days before each biological record is measured) or fixed
#'  (i.e. number of days before a set point in time).
#'@param cutoff.day,cutoff.month If type is "fixed", the day and month of the year
#'  from which the fixed window analysis will start.
#'@param Cmissing TRUE or FALSE, determines what should be done if there are 
#'  missing climate data. If FALSE, the function will not run if missing climate
#'  data is encountered. If TRUE, any records affected by missing climate data 
#'  will be removed from climate window analysis.
#'@param Cinterval The resolution at which climate window analysis will be 
#'  conducted. May be days ("day"), weeks ("week"), or months ("month"). Note the units 
#'  of parameters 'furthest' and 'closest' will differ depending on the choice 
#'  of Cinterval.
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
#'single <- singlewin(Xvar = MassClimate$Temp, Cdate = MassClimate$Date, Bdate = Mass$Date,
#'                    baseline = lm(Mass ~ 1, data = Mass), furthest = 72, closest = 15,
#'                    stat = "mean", func = "lin",
#'                    type = "fixed", cutoff.day = 20, cutoff.month = 5,
#'                    Cmissing = FALSE, Cinterval = "day")
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
              
#LAST EDITED: 19/02/2015
#EDITED BY: LIAM
#NOTES: TIDY CODE

#Plotting part needs fixing

singlewin <- function(Xvar, Cdate, Bdate, baseline, 
                      furthest, closest, stat, func, 
                      type, cutoff.day, cutoff.month, 
                      Cmissing = FALSE, Cinterval = "day"){
  
  baseline  <- update(baseline, .~.)
  nullmodel <- AICc(baseline)  
  duration  <- (furthest-closest) + 1
  MODLIST   <- list()   # dataframes to store ouput
  CMatrix   <- matrix(ncol = (duration), nrow = length(Bdate))
  
  modeldat      <- model.frame(baseline)
  modeldat$Yvar <- modeldat[, 1]
  
  if(length(modeldat$Yvar) != length(Bdate)){
    stop("NA values present in biological response. Please remove NA values")
  }
  
  Bdate  <- as.Date(Bdate, format = "%d/%m/%Y")
  Cdate2 <- seq(min(as.Date(Cdate, format = "%d/%m/%Y")), max(as.Date(Cdate, format = "%d/%m/%Y")), "days") # Convert the date variables into the R date format
  Cdate  <- as.Date(Cdate, format = "%d/%m/%Y") #Convert the date variables into the R date format#
  
  Xvar       <- Xvar[match(Cdate2, Cdate)]
  CIntNo     <- as.numeric(Cdate2) - min(as.numeric(Cdate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to CIntNo 1
  RealBIntNo <- as.numeric(Bdate) - min(as.numeric(Cdate2)) + 1
  
  if (length(CIntNo) != length(unique(CIntNo))){
    stop ("There are duplicate dayrecords in climate data")
  }
  
  if (Cinterval != "day" && Cinterval != "week" && Cinterval != "month"){
    stop("Cinterval should be either D, W or M")
  }
  
  if (Cinterval == "day"){  
    if (type == "fixed"){   
      BIntNo            <- as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(Bdate), sep = "-"), 
                                              format = "%d-%m-%Y")) - min(as.numeric(Cdate2)) + 1 
      wrongyear         <- which(BIntNo<RealBIntNo)
      BIntNo[wrongyear] <- (as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(Bdate[wrongyear]) + 1), sep = "-"), 
                                               format = "%d-%m-%Y")) - min(as.numeric(Cdate2)) + 1)
    } else {
      BIntNo <- RealBIntNo
    }
  } else if (Cinterval == "week"){
    CIntNo     <- ceiling((as.numeric(Cdate2) - min(as.numeric(Cdate2)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to CIntNo 1
    RealBIntNo <- ceiling((as.numeric(Bdate) - min(as.numeric(Cdate2)) + 1) / 7)
    NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar)
    NewClim2   <- melt(NewClim, id = "CIntNo")
    NewClim3   <- cast(NewClim2, CIntNo ~ variable, mean)
    CIntNo     <- NewClim3$CIntNo
    Xvar       <- NewClim3$Xvar
    if(type == "fixed"){ 
      BIntNo            <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(Bdate), sep = "-"), 
                                                       format = "%d-%m-%Y")) - min(as.numeric(Cdate2)) + 1) / 7) 
      wrongyear         <- which(BIntNo<RealBIntNo)
      BIntNo[wrongyear] <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(Bdate[wrongyear]) + 1), sep = "-"), 
                                                       format = "%d-%m-%Y")) - min(as.numeric(Cdate2)) + 1) / 7)  
    } else {
      BIntNo <- RealBIntNo
    }
  } else if (Cinterval == "month"){
    Cmonth     <- month(Cdate2)
    Cyear      <- year(Cdate2) - min(year(Cdate2))
    CIntNo     <- Cmonth + 12 * Cyear
    RealBIntNo <- month(Bdate) + 12 * (year(Bdate) - min(year(Cdate2)))
    NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar)
    NewClim2   <- melt(NewClim, id = "CIntNo")
    NewClim3   <- cast(NewClim2, CIntNo ~ variable, mean)
    CIntNo     <- NewClim3$CIntNo
    Xvar       <- NewClim3$Xvar
    if (type == "fixed"){ 
      BIntNo            <- cutoff.month + 12 * (year(Bdate) - min(year(Cdate2)))
      wrongyear         <- which(BIntNo < RealBIntNo)
      BIntNo[wrongyear] <- cutoff.month + 12 * (year(Bdate[wrongyear]) + 1 - min(year(Cdate2)))
    } else {
      BIntNo <- RealBIntNo
    }
  }
  
  for (i in 1:length(Bdate)){
    for (j in closest:furthest){
      k <- j - closest + 1
      CMatrix[i, k] <- Xvar[which(CIntNo == BIntNo[i] - j)]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  }
  
  if (Cmissing == FALSE && length(which(is.na(CMatrix))) > 0){
    .GlobalEnv$Missing <- as.Date(CIntNo[is.na(Xvar)], origin = min(as.Date(Cdate2, format = "%d/%m/%Y")) - 1)
    stop(c("Climate data should not contain NA values: ", length(.GlobalEnv$Missing),
           " NA value(s) found. Please add missing climate data or set Cmissing=TRUE.
           See object Missing for all missing climate data"))
  }
  
  if (Cmissing == TRUE && length(which(is.na(CMatrix))) > 0){
    modeldat      <- modeldat[complete.cases(CMatrix), ]
    baseline      <- update(baseline, Yvar~., data = modeldat)
    CMatrix       <- CMatrix[complete.cases(CMatrix), ]
  }  
  
  
  modeldat           <- model.frame(baseline)
  modeldat$climate <- matrix(ncol = 1, nrow = nrow(CMatrix), seq(from = 1, to = nrow(CMatrix), by = 1))
  
  if (func == "lin"){
    modeloutput <- update(baseline, .~. + climate, data = modeldat)
  } else if(func == "quad"){
    modeloutput <- update(baseline, .~. + climate + I(climate^2), data = modeldat)
  } else if(func == "cub"){
    modeloutput <- update(baseline, .~. + climate + I(climate^2) + I(climate^3), data = modeldat)
  } else if(func == "log"){
    modeloutput <- update(baseline, .~. + log(climate), data = modeldat)
  } else if(func == "inv"){
    modeloutput <- update(baseline, .~. + I(climate ^ -1), data = modeldat)
  } else {
    print("Define func")
  }
  #CREATE A FOR LOOP TO FIT DIFFERENT CLIMATE WINDOWS#
  MODNO <- 1  #Create a model number variable that will count up during the loop#
  m     <- closest
  n     <- duration
  if ( (m-n) >= (closest - 1)){  # do not use windows that overshoot the closest possible day in window   
    if (stat != "slope" || n > 1){
      if (stat == "slope"){
        time               <- seq(1, n, 1)
        modeldat$climate <- apply(CMatrix, 1, FUN = function(x) coef(lm(x ~ time))[2])
      } else {
        ifelse (n == 1, modeldat$climate <- CMatrix, modeldat$climate <- apply(CMatrix, 1, FUN = stat))
      }    
      # run the model
      modeloutput <- update(modeloutput, .~.) 
      #Add model parameters to list#
      MODLIST$ModelAICc[[MODNO]]    <- AICc(modeloutput)
      MODLIST$WindowOpen[[MODNO]]   <- m
      MODLIST$WindowClose[[MODNO]]  <- m - n + 1
      MODLIST$ModelBeta[[MODNO]]    <- coef(modeloutput)[2]   # add one coef if quadratic
      if (func == "quad" ){
        MODLIST$ModelBetaQ[[MODNO]] <- coef(modeloutput)[3]
      }   
      if(func == "cub" ){
        MODLIST$ModelBetaQ[[MODNO]] <- coef(modeloutput)[3]    
        MODLIST$ModelBetaC[[MODNO]] <- coef(modeloutput)[4]
      }   
      MODNO <- MODNO + 1        #Increase ModNo#
    }
  }
  #Save the best model output
  if (stat == "slope"){
    time               <- seq(1, n, 1)
    modeldat$climate <- apply(CMatrix, 1, FUN = function(x) coef(lm(x ~ time))[2])
  } else {
    ifelse(n == 1, modeldat$climate <- CMatrix, modeldat$climate <- apply(CMatrix, 1, FUN = stat))
  }
  LocalBestModel      <- update(modeloutput, .~.)
  LocalData           <- model.frame(LocalBestModel)
  return(list(BestModel = LocalBestModel, BestModelData = LocalData))
}