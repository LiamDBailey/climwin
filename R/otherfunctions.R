#Basewin function that is combined with manywin to test multiple climate window characteristics
basewin <- function(Xvar, CDate, BDate, baseline, furthest, closest, 
                    FIXED, cutoff.day, cutoff.month, STAT = "mean", FUNC = "L",
                    CMISSING = FALSE, CINTERVAL = "D",  nrandom = 0, CVK = 0,
                    upper = NA, lower = NA, thresh = FALSE){
  print("Initialising, please wait...")
  
  if(STAT == "slope" & FUNC == "LOG" || STAT == "slope" & FUNC == "I"){
    stop("STAT = slope cannot be used with FUNC = LOG or I as negative values may be present")
  }
  
  if(CINTERVAL == "D"){
    if((min(as.Date(BDate, format = "%d/%m/%Y")) - furthest) < min(as.Date(CDate, format = "%d/%m/%Y"))){
      stop("You do not have enough climate data to search that far back. Please adjust the value of furthest or add additional climate data.")
    }
  }
  
  if(CINTERVAL == "W"){
    if((min(as.Date(BDate, format = "%d/%m/%Y")) - lubridate::weeks(furthest)) < min(as.Date(CDate, format = "%d/%m/%Y"))){
      stop("You do not have enough climate data to search that far back. Please adjust the value of furthest or add additional climate data.")
    }
  }
  
  if(CINTERVAL == "M"){
    if((min(as.Date(BDate, format = "%d/%m/%Y")) - months(furthest)) < min(as.Date(CDate, format = "%d/%m/%Y"))){
      stop("You do not have enough climate data to search that far back. Please adjust the value of furthest or add additional climate data.")
    }
  }
  
  duration  <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2
  cont      <- DateConverter(BDate = BDate, CDate = CDate, Xvar = Xvar, 
                             CINTERVAL = CINTERVAL, FIXED = FIXED, 
                             cutoff.day = cutoff.day, cutoff.month = cutoff.month)   # create new climate dataframe with continuous daynumbers, leap days are not a problem
  MODNO     <- 1  #Create a model number variable that will count up during the loop#
  CMatrix   <- matrix(ncol = (duration), nrow = length(BDate))  # matrix that stores the weather data for variable or fixed windows
  MODLIST   <- list()   # dataframes to store ouput
  baseline  <- update(baseline, .~.)
  nullmodel <- AICc(baseline)
  
  modeldat      <- model.frame(baseline)
  modeldat$Yvar <- modeldat[, 1]
  
  if(length(modeldat$Yvar) != length(BDate)){
    stop("NA values present in biological response. Please remove NA values")
  }
  
  if(is.na(upper) == FALSE && is.na(lower) == TRUE){
    if(thresh == TRUE){
      cont$Xvar <- ifelse(cont$Xvar > upper, 1, 0)
    } else {
      cont$Xvar <- ifelse(cont$Xvar > upper, cont$Xvar, 0)
    }
  }
  
  
  if(is.na(lower) == FALSE && is.na(upper) == TRUE){
    if(thresh == TRUE){
      cont$Xvar <- ifelse(cont$Xvar < lower, 1, 0)
    } else {
      cont$Xvar <- ifelse(cont$Xvar < lower, cont$Xvar, 0)
    }
  }
  
  if(is.na(lower) == FALSE && is.na(upper) == FALSE){
    if(thresh == TRUE){
      cont$Xvar <- ifelse(cont$Xvar > lower & cont$Xvar < upper, 1, 0)
    } else {
      cont$Xvar <- ifelse(cont$Xvar > lower & cont$Xvar < upper, cont$Xvar - lower, 0)
    } 
  }  
  
  for (i in 1:length(BDate)){
    for (j in closest:furthest){
      k <- j - closest + 1
      CMatrix[i, k] <- cont$Xvar[which(cont$CIntNo == cont$BIntNo[i] - j)]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  }
  
  if (CMISSING == FALSE && length(which(is.na(CMatrix))) > 0){
    if(CINTERVAL == "D"){
      .GlobalEnv$Missing <- as.Date(cont$CIntNo[is.na(cont$Xvar)], origin = min(as.Date(CDate, format = "%d/%m/%Y")) - 1)
    }
    if(CINTERVAL == "M"){
      .GlobalEnv$Missing <- c(paste("Month:", month(as.Date(cont$CIntNo[is.na(cont$Xvar)], origin = min(as.Date(CDate, format = "%d/%m/%Y")) - 1)),
                                    "Year:", year(as.Date(cont$CIntNo[is.na(cont$Xvar)], origin = min(as.Date(CDate, format = "%d/%m/%Y")) - 1))))
    }
    if(CINTERVAL == "W"){
      .GlobalEnv$Missing <- c(paste("Week:", month(as.Date(cont$CIntNo[is.na(cont$Xvar)], origin = min(as.Date(CDate, format = "%d/%m/%Y")) - 1)),
                                    "Year:", year(as.Date(cont$CIntNo[is.na(cont$Xvar)], origin = min(as.Date(CDate, format = "%d/%m/%Y")) - 1))))
    }
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
  modeldat$Yvar      <- modeldat[, 1]
  modeldat$climate <- matrix(ncol = 1, nrow = nrow(CMatrix), seq(from = 1, to = nrow(CMatrix), by = 1))
  
  if(is.null(weights(baseline)) == FALSE){
    modeldat$modweights <- weights(baseline)
    baseline <- update(baseline, .~., weights = modweights, data = modeldat)
  }
  
  if (CVK>1){
    modeldat$K <- sample(seq(from = 1, to = length(modeldat$climate), by = 1) %% CVK + 1)
  }   # create labels K-fold crossvalidation
  
  if (FUNC == "L"){
    modeloutput <- update(baseline, .~. + climate, data = modeldat)
  } else if (FUNC == "Q") {
    modeloutput <- update(baseline, .~. + climate + I(climate ^ 2), data = modeldat)
  } else if (FUNC == "C") {
    modeloutput <- update(baseline, .~. + climate + I(climate ^ 2) + I(climate ^ 3), data = modeldat)
  } else if (FUNC == "LOG") {
    modeloutput <- update(baseline, .~. + log(climate), data = modeldat)
  } else if (FUNC == "I") {
    modeloutput <- update (baseline, .~. + I(climate ^ -1), data = modeldat)
  } else {
    print("DEFINE FUNC")
  }
  
  pb <- txtProgressBar(min = 0, max = MaxMODNO, style = 3, char = "|")
  
  #CREATE A FOR LOOP TO FIT DIFFERENT CLIMATE WINDOWS#
  for (m in closest:furthest){
    for (n in 1:duration){
      if ( (m - n) >= (closest - 1)){  # do not use windows that overshoot the closest possible day in window   
        if (STAT != "slope" || n > 1){
          windowopen  <- m - closest + 1
          windowclose <- windowopen - n + 1
          if (STAT == "slope"){ 
            time               <- seq(1, n, 1)
            modeldat$climate <- apply(CMatrix[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
          } else { 
            ifelse (n == 1, modeldat$climate <- CMatrix[, windowclose:windowopen], modeldat$climate <- apply(CMatrix[, windowclose:windowopen], 1, FUN = STAT))
          }
          if(min(modeldat$climate) <= 0 & FUNC == "LOG" || min(modeldat$climate) <= 0 & FUNC == "I"){
            stop("FUNC = LOG or I cannot be used with climate values >= 0. 
                 Consider adding a constant to climate data to remove these values")
          }
          modeloutput <- update(modeloutput, .~.)
          
          # If valid, perform k-fold crossvalidation
          if (CVK>1) {      
            for (k in 1:CVK) {
              test           <- subset(modeldat, modeldat$K == k) # Create the test dataset
              train          <- subset(modeldat, modeldat$K != k) # Create the train dataset
              modeloutputcv <- update(modeloutput, Yvar~., data = train)  # Refit the model with climate using the train dataset
              baselinecv    <- update(baseline, Yvar~., data = train) # Refit the model without climate using the train dataset
              test$predictions <- predict(modeloutputcv, newdata = test, allow.new.levels = TRUE) # Test the output of the climate model fitted using the test data
              test$predictionsbaseline <- predict(baselinecv, newdata = test, allow.new.levels = TRUE) # Test the output of the null models fitted using the test data
              N <- length(test$predictions) # Determine the length of the test dataset
              P <- N - df.residual(modeloutputcv)  # Determine df for the climate model
              P_baseline <- N - df.residual(baselinecv)  # Determine df for the baseline model
              mse <- sum((test$predictions - test[, 1]) ^ 2) / N
              #calculate mean standard errors for climate model
              #calc mse only works non-categorical Yvars, e.g. normal, binary, count data 
              mse_baseline <- sum((test$predictionsbaseline - test[, 1]) ^ 2) / N
              #calculate mean standard errors for null model
              AICc_cv <- N * log(mse) + (2 * P * (P + 1)) / (N - P - 1)
              AICc_cv_baseline <- N * log(mse_baseline) + (2 * P_baseline * (P_baseline + 1)) / (N - P_baseline - 1)
              #Calculate AICc values for climate and baseline models
              #rmse_corrected<-sqrt(sum((test$predictions-test[,1])^2)/modeloutputcv$df[1])
              ifelse(k == 1, AICc_cvtotal <- AICc_cv, AICc_cvtotal <- AICc_cvtotal + AICc_cv)              
              ifelse(k == 1, AICc_cv_basetotal <- AICc_cv_baseline, AICc_cv_basetotal <- AICc_cv_basetotal + AICc_cv_baseline)
              #Add up the AICc values for all iterations of crossvalidation
            }
            AICc_cv_avg <- AICc_cvtotal / CVK # Determine the average AICc value of the climate model from cross validations
            AICc_cv_baseline_avg <- AICc_cv_basetotal / CVK # Determine the average AICc value of the null model from cross validations
            deltaAICc_cv <- AICc_cv_avg - AICc_cv_baseline_avg # Calculate delta AICc
          }
          
          #Add model parameters to list
          if(CVK > 1){
            MODLIST$ModelAICc[[MODNO]]    <- AICc_cv_avg
            MODLIST$deltaAICc[[MODNO]]    <- deltaAICc_cv
            MODLIST$baselineAICc[[MODNO]] <- AICc_cv_baseline_avg
          } else {
            MODLIST$ModelAICc[[MODNO]]   <- AICc(modeloutput)
            MODLIST$baselineAICc <- AICc(baseline) 
            #WORK OUT A WAY TO REMOVE THIS FROM LOOP
          }
          
          MODLIST$WindowOpen[[MODNO]]  <- m
          MODLIST$WindowClose[[MODNO]] <- m - n + 1
          
          if(length(attr(class(modeloutput),"package")) > 0 && attr(class(modeloutput), "package") == "lme4"){
            MODLIST$ModelBeta[[MODNO]] <- fixef(modeloutput)[length(fixef(modeloutput))]
            MODLIST$ModelBetaQ[[MODNO]] <- NA
            MODLIST$ModelBetaC[[MODNO]] <- NA
            MODLIST$ModelInt[[MODNO]]  <- fixef(modeloutput)[1]
            
            if (FUNC == "Q"){
              MODLIST$ModelBeta[[MODNO]] <- fixef(modeloutput)[length(fixef(modeloutput))-1]
              MODLIST$ModelBetaQ[[MODNO]] <- fixef(modeloutput)[length(fixef(modeloutput))]
              MODLIST$ModelBetaC[[MODNO]] <- NA
            }   
            
            if (FUNC == "C"){
              MODLIST$ModelBeta[[MODNO]] <- fixef(modeloutput)[length(fixef(modeloutput))-2]
              MODLIST$ModelBetaQ[[MODNO]] <- fixef(modeloutput)[length(fixef(modeloutput))-1]    
              MODLIST$ModelBetaC[[MODNO]] <- fixef(modeloutput)[length(fixef(modeloutput))]
            }
          } else {
            MODLIST$ModelBeta[[MODNO]]   <- coef(modeloutput)[length(coef(modeloutput))]
            MODLIST$ModelBetaQ[[MODNO]] <- NA
            MODLIST$ModelBetaC[[MODNO]] <- NA
            MODLIST$ModelInt[[MODNO]]    <- coef(modeloutput)[1]
            
            if (FUNC == "Q"){
              MODLIST$ModelBeta[[MODNO]]   <- coef(modeloutput)[length(coef(modeloutput))-1]
              MODLIST$ModelBetaQ[[MODNO]] <- coef(modeloutput)[length(coef(modeloutput))]
              MODLIST$ModelBetaC[[MODNO]] <- NA
            }   
            
            if (FUNC == "C"){
              MODLIST$ModelBeta[[MODNO]]   <- coef(modeloutput)[length(coef(modeloutput))-2]
              MODLIST$ModelBetaQ[[MODNO]] <- coef(modeloutput)[length(coef(modeloutput))-1]    
              MODLIST$ModelBetaC[[MODNO]] <- coef(modeloutput)[length(coef(modeloutput))]
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
    modeldat$climate <- apply(CMatrix[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
  } else {
    ifelse (windowopen - windowclose == 0, modeldat$climate <- CMatrix[, windowclose:windowopen], modeldat$climate <- apply(CMatrix[, windowclose:windowopen], 1, FUN = STAT))
  }
  LocalModel           <- update(modeloutput, .~.)
  MODLIST$furthest     <- furthest
  MODLIST$closest      <- closest
  MODLIST$Statistics   <- STAT
  MODLIST$Function     <- FUNC
  MODLIST$FIXED        <- FIXED
  MODLIST$K            <- CVK
  
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
    return(list(BestModel = LocalModel, BestModelData = LocalData, Dataset = LocalOutput))
  } else {
    return(LocalOutputRand)
  }
  }

#Function to convert dates into day/week/month number
DateConverter <- function(BDate, CDate, Xvar, Xvar2 = NULL, CINTERVAL, FIXED, 
                          cutoff.day, cutoff.month, cross = FALSE){
  
  BDate  <- as.Date(BDate, format = "%d/%m/%Y") # Convert the date variables into the R date format
  CDate2 <- seq(min(as.Date(CDate, format = "%d/%m/%Y")), max(as.Date(CDate, format = "%d/%m/%Y")), "days") # Convert the date variables into the R date format
  CDate  <- as.Date(CDate, format = "%d/%m/%Y")
  
  if(min(CDate) > min(BDate)){
    stop("Climate data does not cover all years of biological data. Please increase range of climate data")
  }
  
  Xvar       <- Xvar[match(CDate2, CDate)]
  if(is.null(Xvar2) == FALSE){
    Xvar2 <- Xvar2[match(CDate2, CDate)]
  }
  CIntNo     <- as.numeric(CDate2) - min(as.numeric(CDate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to CIntNo 1
  RealBIntNo <- as.numeric(BDate) - min(as.numeric(CDate2)) + 1
  
  if (length(CIntNo) != length(unique(CIntNo))){
    stop ("There are duplicate dayrecords in climate data")
  }
  
  if (CINTERVAL != "D" && CINTERVAL != "W" && CINTERVAL != "M"){
    stop("CINTERVAL should be either D, W or M")
  }
  
  if(cross == FALSE){
    if (CINTERVAL == "D"){  
      if (FIXED == TRUE){   
        BIntNo            <- as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(BDate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1 
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- (as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(BDate[wrongyear]) + 1), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1)
      } else {
        BIntNo <- RealBIntNo
      }
    } else if (CINTERVAL == "W"){
      CIntNo     <- ceiling((as.numeric(CDate2) - min(as.numeric(CDate2)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to CIntNo 1
      RealBIntNo <- ceiling((as.numeric(BDate) - min(as.numeric(CDate2)) + 1) / 7)
      NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar)
      NewClim2   <- melt(NewClim, id = "CIntNo")
      NewClim3   <- cast(NewClim2, CIntNo~variable, mean)
      CIntNo     <- NewClim3$CIntNo
      Xvar       <- NewClim3$Xvar
      if (FIXED == TRUE){ 
        BIntNo            <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(BDate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate2)) + 1) / 7) 
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(BDate[wrongyear]) + 1), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate2)) + 1) / 7)
      } else {
        BIntNo <- RealBIntNo
      }
    } else if (CINTERVAL == "M"){ 
      Cmonth     <- month(CDate2)
      Cyear      <- year(CDate2) - min(year(CDate2))
      CIntNo     <- Cmonth + 12 * Cyear
      RealBIntNo <- month(BDate) + 12 * (year(BDate) - min(year(CDate2)))
      NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar)
      NewClim2   <- melt(NewClim, id = "CIntNo")
      NewClim3   <- cast(NewClim2, CIntNo~variable, mean)
      CIntNo     <- NewClim3$CIntNo
      Xvar       <- NewClim3$Xvar
      if (FIXED == TRUE){ 
        BIntNo            <- cutoff.month + 12 * (year(BDate) - min(year(CDate2)))
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- cutoff.month + 12 * (year(BDate[wrongyear]) + 1 - min(year(CDate2)))
      } else {
        BIntNo <- RealBIntNo
      }
    }
  } else {
    if (CINTERVAL == "D"){  
      if (FIXED == TRUE){   
        BIntNo            <- as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(BDate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate2)) + 1 
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- (as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(BDate[wrongyear]) + 1), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate2)) + 1)
      } else {
        BIntNo <- RealBIntNo
      }    
    } else if (CINTERVAL == "W"){
      CIntNo     <- ceiling((as.numeric(CDate2) - min(as.numeric(CDate2)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to CIntNo 1
      RealBIntNo <- ceiling((as.numeric(BDate) - min(as.numeric(CDate2)) + 1) / 7)
      NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar, "Xvar2" = Xvar2)
      NewClim2   <- melt(NewClim, id = "CIntNo")
      NewClim3   <- cast(NewClim2, CIntNo~variable, mean)
      CIntNo     <- NewClim3$CIntNo
      Xvar       <- NewClim3$Xvar
      Xvar2      <- NewClim3$Xvar2
      if (FIXED == TRUE){ 
        BIntNo            <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(BDate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate2)) + 1) / 7) 
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(BDate[wrongyear]) + 1), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate2)) + 1) / 7)
      } else {
        BIntNo <- RealBIntNo
      }
    } else if (CINTERVAL == "M"){ 
      Cmonth     <- month(CDate2)
      Cyear      <- year(CDate2) - min(year(CDate2))
      CIntNo     <- Cmonth + 12 * Cyear
      RealBIntNo <- month(BDate) + 12 * (year(BDate) - min(year(CDate2)))
      NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar, "Xvar2" = Xvar2)
      NewClim2   <- melt(NewClim, id = "CIntNo")
      NewClim3   <- cast(NewClim2, CIntNo ~ variable, mean)
      CIntNo     <- NewClim3$CIntNo
      Xvar       <- NewClim3$Xvar
      Xvar2      <- NewClim3$Xvar2
      if (FIXED == TRUE){ 
        BIntNo            <- cutoff.month + 12 * (year(BDate) - min(year(CDate2)))
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- cutoff.month + 12 * (year(BDate[wrongyear]) + 1 - min(year(CDate2)))
      } else {
        BIntNo <- RealBIntNo
      }
    }
  }
  return(list(CIntNo = CIntNo, BIntNo = BIntNo, Xvar = Xvar, Xvar2 = Xvar2))
}


# define a function that returns the AICc or -2LogLikelihood of model using Generalized Extreme Value (GEV) weight function
ModelLogLikelihoodG <- function(par = par, modeloutput = modeloutput, 
                                duration = duration, CMatrix = CMatrix, 
                                nullmodel = nullmodel, funcenv = funcenv){
  
  j                     <- seq(-10, 10, by = (2 * 10 / duration))  # value of 10 is chosen arbitrarily but seems to be suitable
  weight                <- dgev(j[1:duration], loc = par[3], scale = par[2], shape = par[1], log = FALSE)   # calculate weights using the GEV probability distribution function
  weight[is.na(weight)] <- 0 # the GEV function produces "NA" for some values of j if the parameter constraint on kappa, lambda and mu is not satisfied. We put such values to zero.
  
  if (sum(weight) == 0){
    weight <- weight + 1
  }

  weight                                <- weight / sum(weight) 
  funcenv$modeldat$climate            <- apply(CMatrix, 1, FUN = function(x) {sum(x*weight)})    # calculate weighted mean from weather data
  modeloutput                           <- update(modeloutput, .~., data = funcenv$modeldat)   # rerun regression model using new weather index
  deltaAICc                             <- AICc(modeloutput) - nullmodel
  funcenv$DAICc[[funcenv$MODNO]]        <- deltaAICc
  funcenv$par_shape[[funcenv$MODNO]]    <- par[1]
  funcenv$par_scale[[funcenv$MODNO]]    <- par[2]
  funcenv$par_location[[funcenv$MODNO]] <- par[3]
  
  # plot the weight function and corresponding weather index being evaluated
  par(mfrow = c(3, 2))
  plot((weight / sum(weight)), type = "l", ylab = "weight", xlab = "timestep (e.g. days)", main = "Output of current weighted window being tested")
  plot(as.numeric(funcenv$par_shape), type = "l", ylab = "shape parameter", xlab = "convergence step", main = "GEV parameter values being tested")
  plot(as.numeric(funcenv$DAICc), type = "l", ylab = expression(paste(Delta, "AICc")), xlab = "convergence step")
  plot(as.numeric(funcenv$par_scale), type = "l", ylab = "scale parameter", xlab = "convergence step")
  plot(funcenv$modeldat$climate[1:(3 * duration)], type = "s", ylab = "weighted mean of climate", xlab = "timestep (e.g. days)")
  plot(as.numeric(funcenv$par_location), type = "l", ylab = "location parameter", xlab = "convergence step")
  
  funcenv$MODNO <- funcenv$MODNO + 1
  return(deltaAICc)  # returns deltaAICc as optim() minimizes! 
}


# define a function that returns the AICc or -2LogLikelihood of model using Weibull weight function
ModelLogLikelihoodW <- function(par = par,  modeloutput = modeloutput, duration = duration, 
                                CMatrix = CMatrix, nullmodel = nullmodel, funcenv = funcenv){
  
  j                     <- seq(1:duration) / duration # rescale j to interval [0,1]
  weight                <- weibull3(x = j, shape = par[1], scale = par[2], location = par[3])  # calculate weights using the Weibull probability distribution function
  weight[is.na(weight)] <- 0
  
  if (sum(weight) == 0){
    weight <- weight + 1
  }
  
  weight                                <- weight / sum(weight) 
  funcenv$modeldat$climate            <- apply(CMatrix, 1, FUN = function(x) {sum(x*weight)})    # calculate weighted mean from weather data
  modeloutput                           <- update(modeloutput, .~., data = funcenv$modeldat)   # rerun regression model using new weather index
  deltaAICc                             <- AICc(modeloutput) - nullmodel
  funcenv$DAICc[[funcenv$MODNO]]        <- deltaAICc
  funcenv$par_shape[[funcenv$MODNO]]    <- par[1]
  funcenv$par_scale[[funcenv$MODNO]]    <- par[2]
  funcenv$par_location[[funcenv$MODNO]] <- par[3]
  

  # plot the weight function and corresponding weather index being evaluated
  par(mfrow = c(3, 2))
  plot((weight/sum(weight)), type = "l", ylab = "weight", xlab = "time step (e.g days)", main = "Output of current weighted window being tested")
  plot(as.numeric(funcenv$par_shape), type = "l", ylab = "shape parameter", xlab = "convergence step", main = "Weibull parameter values being tested")
  plot(as.numeric(funcenv$DAICc), type = "l", ylab = expression(paste(Delta, "AICc")), xlab = "convergence step")
  plot(as.numeric(funcenv$par_scale), type = "l", ylab = "scale parameter", xlab = "convergence step")
  plot(funcenv$modeldat$climate[1:(duration)], type = "s", ylab = "weighted mean of weather", xlab = "time step (e.g days)")
  plot(as.numeric(funcenv$par_location), type = "l", ylab = "location parameter", xlab = "convergence step")
  
  funcenv$MODNO <- funcenv$MODNO + 1
  return(deltaAICc)  # returns deltaAICc as optim() minimizes! 
}


PlotWeight <- function(Dataset = .GlobalEnv$WeightedOutput){
  ifelse (Dataset$WeightFunction == "W", WF <- "Weibull", WF <- "GEV")
  plot(Dataset$Weight, type = "l", ylab = "weight", xlab = "time step (e.g days)", 
       main = c(paste("Weight function of best", WF, "window"), 
                paste("shape=", Dataset$par_shape), paste("scale=", Dataset$par_scale), paste("location=", Dataset$par_loc)))
}



weibull3<-function(x, shape,scale,location){
  shape / scale * ((x - location) / scale) ^ (shape - 1) * exp( - ((x - location) / scale) ^ shape)
}