#############################################################################################################

#climatewin is now redundant, will transfer straight to slidingwin with message
climatewin <- function(exclude = NA, xvar, cdate, bdate, baseline, 
                       type, refday, stat = "mean", func = "lin", range, 
                       cmissing = FALSE, cinterval = "day", k = 0,
                       upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"),
                       spatial = NULL, cutoff.day = NULL, cutoff.month = NULL, 
                       furthest = NULL, closest = NULL,
                       thresh = NULL, cvk = NULL, cohort = NULL){
  
  print("PLEASE NOTE: Function 'climatewin' is being made redundant. Please use 'slidingwin' as an alternative")
  
  slidingwin(exclude = exclude, xvar = xvar, cdate = cdate, bdate = bdate, baseline = baseline, 
             type = type, refday = refday, stat = stat, func = func, range = range, 
             cmissing = cmissing, cinterval = cinterval, k = k,
             upper = upper, lower = lower, binary = binary, centre = centre,
             spatial = spatial, cutoff.day = cutoff.day, cutoff.month = cutoff.month, 
             furthest = furthest, closest = closest,
             thresh = thresh, cvk = cvk, cohort = cohort)
  
}

###########################################################################################################

#Basewin function that is combined with manywin to test multiple climate window characteristics
basewin <- function(exclude, xvar, cdate, bdate, baseline, range, 
                    type, stat = "mean", func = "lin", refday,
                    cmissing = FALSE, cinterval = "day",  nrandom = 0, k = 0,
                    spatial, upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"),
                    cohort = NULL){
  
  print("Initialising, please wait...")
  
  if(is.null(spatial) == FALSE){
    
    if(is.null(cohort) == FALSE){
      
      sample.size <- 0
      data <- data.frame(bdate = bdate, spatial = as.factor(spatial[[1]]), cohort = as.factor(cohort))
      
      for(i in levels(as.factor(data$cohort))){
        
        sub <- subset(data, cohort = i)
        sub$spatial <- factor(sub$spatial)
        sample.size <- sample.size + length(levels(sub$spatial))
        
      }
      
    } else if(is.null(cohort) == TRUE){
      
      sample.size <- 0
      data <- data.frame(bdate = bdate, spatial = as.factor(spatial[[1]]))
      data$Year <- lubridate::year(as.Date(data$bdate, format = "%d/%m/%Y"))
      
      for(i in levels(as.factor(data$Year))){
        
        sub <- subset(data, data$Year == i)
        sub$spatial <- factor(sub$spatial)
        sample.size <- sample.size + length(levels(sub$spatial))        
        
      }
      
    }
    
  } else if(is.null(spatial) == TRUE) {
    
    if(is.null(cohort) == FALSE){
      sample.size <- length(levels(as.factor(cohort)))
    } else if(is.null(cohort) == TRUE){
      sample.size <- length(levels(as.factor(lubridate::year(as.Date(bdate, format = "%d/%m/%Y")))))
    }  
  }
  
  if(is.null(centre[[1]]) == FALSE){
    func = "centre"
    if(centre[[2]] != "both" & centre[[2]] != "dev" & centre[[2]] != "mean"){
      stop("Please set centre to one of 'both', 'dev', or 'mean'. See help file for details.")
    }
  }
  
  if (stat == "slope" & func == "log" || stat == "slope" & func == "inv"){
    stop("stat = slope cannot be used with func = log or inv as negative values may be present")
  }
  
  duration  <- (range[1] - range[2]) + 1
  maxmodno  <- (duration * (duration + 1))/2
  if (length(exclude) == 2 ){ maxmodno  <- maxmodno- exclude[1]*(duration-exclude[2]-1)+(exclude[1]-1)*exclude[1]/2 }
  if (stat == "slope") { 
    ifelse(is.na(exclude[2])==TRUE,  maxmodno  <- maxmodno - duration, maxmodno  <- maxmodno-exclude[2]-1)
  } 
  cont      <- convertdate(bdate = bdate, cdate = cdate, xvar = xvar, 
                           cinterval = cinterval, type = type, 
                           refday = refday, cohort = cohort, spatial = spatial)   # create new climate dataframe with continuous daynumbers, leap days are not a problem
  
  if(is.null(spatial) == FALSE){
    
    if (cinterval == "day"){
      if ( (min(cont$bintno$Date) - range[1]) < min(cont$cintno$Date)){
        stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additioNAl climate data.")
      }
    }
    
    if (cinterval == "week"){
      if ( (min(cont$bintno$Date) - range[1] * 7) < min(cont$cintno$Date)){
        stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additioNAl climate data.")
      }
    }
    
    if (cinterval == "month"){
      if ( (as.numeric(min(as.Date(bdate, format = "%d/%m/%Y")) - months(range[1])) - (as.numeric(min(as.Date(cdate, format = "%d/%m/%Y"))))) <= 0){
        stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additioNAl climate data.")
      }
    }
    
    if (max(cont$bintno$Date) > max(cont$cintno$Date)){
      if (type == "absolute"){
        stop("You need more recent biological data. This error may be caused by your choice of refday")
      } else {
        stop("You need more recent biological data")
      }
    }
    
  } else {
    
    if (cinterval == "day"){
      if ( (min(cont$bintno) - range[1]) < min(cont$cintno)){
        stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additioNAl climate data.")
      }
    }
    
    if (cinterval == "week"){
      if ( (min(cont$bintno) - range[1] * 7) < min(cont$cintno)){
        stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additioNAl climate data.")
      }
    }
    
    if (cinterval == "month"){
      if ( (as.numeric(min(as.Date(bdate, format = "%d/%m/%Y")) - months(range[1])) - (as.numeric(min(as.Date(cdate, format = "%d/%m/%Y"))))) <= 0){
        stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additioNAl climate data.")
      }
    }
    
    if (max(cont$bintno) > max(cont$cintno)){
      if (type == "absolute"){
        stop("You need more recent biological data. This error may be caused by your choice of refday")
      } else {
        stop("You need more recent biological data")
      }
    }
    
  }
  
  modno     <- 1  #Create a model number variable that will count up during the loop#
  cmatrix   <- matrix(ncol = (duration), nrow = length(bdate))  # matrix that stores the weather data for variable or fixed windows
  modlist   <- list()   # dataframes to store ouput
  baseline  <- update(baseline, .~.)
  nullmodel <- AICc(baseline)
  modeldat  <- model.frame(baseline)
  
  if(class(baseline)[length(class(baseline))]=="coxph" & grepl("frailty\\(", colnames(modeldat)[ncol(modeldat)])){
    colnames(modeldat)[ncol(modeldat)] <- gsub("frailty\\(", "", colnames(modeldat)[ncol(modeldat)])
    colnames(modeldat)[ncol(modeldat)] <- gsub("\\)", "", colnames(modeldat)[ncol(modeldat)])
  }
  
  modeldat$yvar <- modeldat[, 1]
  
  if (is.null(centre[[1]]) == FALSE){
    func <- "centre"
  }
  
  ifelse(class(baseline)[length(class(baseline))]=="coxph", leng<-length(modeldat$yvar[,1]), leng<-length(modeldat$yvar))
  if (leng != length(bdate)){
      stop("NA values present in biological response. Please remove NA values")
  }

  if(is.null(spatial) == FALSE){
    
    if (is.na(upper) == FALSE && is.na(lower) == TRUE){
      if (binary == TRUE){
        cont$xvar$Clim <- ifelse (cont$xvar$Clim > upper, 1, 0)
      } else {
        cont$xvar$Clim <- ifelse (cont$xvar$Clim > upper, cont$xvar$Clim, 0)
      }
    }
    
    if (is.na(lower) == FALSE && is.na(upper) == TRUE){
      if (binary == TRUE){
        cont$xvar$Clim <- ifelse (cont$xvar$Clim < lower, 1, 0)
      } else {
        cont$xvar$Clim <- ifelse (cont$xvar$Clim < lower, cont$xvar$Clim, 0)
      }
    }
    
    if (is.na(lower) == FALSE && is.na(upper) == FALSE){
      if (binary == TRUE){
        cont$xvar$Clim <- ifelse (cont$xvar$Clim > lower & cont$xvar$Clim < upper, 1, 0)
      } else {
        cont$xvar$Clim <- ifelse (cont$xvar$Clim > lower & cont$xvar$Clim < upper, cont$xvar$Clim - lower, 0)
      } 
    }
    
  } else {
    
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
  
  if(is.null(spatial) == FALSE){
    for (i in 1:length(bdate)){
      cmatrix[i, ] <- cont$xvar[which(cont$cintno$spatial %in% cont$bintno$spatial[i] & cont$cintno$Date %in% (cont$bintno$Date[i] - c(range[2]:range[1]))), 1]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  } else {
    for (i in 1:length(bdate)){
      cmatrix[i, ] <- cont$xvar[which(cont$cintno %in% (cont$bintno[i] - c(range[2]:range[1])))]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    } 
  }
  cmatrix <- as.matrix(cmatrix[, c(ncol(cmatrix):1)])
  
  if(cmissing == FALSE && length(which(is.na(cmatrix))) > 0){
    if(is.null(spatial) == FALSE){
      
      if (cinterval == "day"){
        .GlobalEnv$missing <- as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if (cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", lubridate::month(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if (cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", lubridate::month(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
    } else {
      
      if (cinterval == "day"){
        .GlobalEnv$missing <- as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if (cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", lubridate::month(as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if (cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", lubridate::month(as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
    }

    stop(c("Climate data should not contain NA values: ", length(.GlobalEnv$missing),
           " NA value(s) found. Please add missing climate data or set cmissing=TRUE.
           See object missing for all missing climate data"))
  }
  
  if (cmissing == TRUE && length(which(is.na(cmatrix))) > 0){
    modeldat <- modeldat[complete.cases(cmatrix), ]
    baseline <- update(baseline, yvar~., data = modeldat)
    cmatrix  <- cmatrix[complete.cases(cmatrix), ]
  }

  modeldat$climate <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
  
  if (is.null(weights(baseline)) == FALSE){
    if (class(baseline)[1] == "glm" & sum(weights(baseline)) == nrow(model.frame(baseline)) || attr(class(baseline), "package") == "lme4" & sum(weights(baseline)) == nrow(model.frame(baseline))){
    } else {
     modeldat$modweights <- weights(baseline)
     baseline <- update(baseline, .~., weights = modeldat$modweights, data = modeldat)
    }
  }
  
  if (k > 1){
    modeldat$K <- sample(seq(from = 1, to = length(modeldat$climate), by = 1) %% k + 1)
  }   # create labels k-fold crossvalidation

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
      print("Define func")
    }
  
  pb <- txtProgressBar(min = 0, max = maxmodno, style = 3, char = "|")
  
  #CREATE A FOR LOOP TO FIT DIFFERENT CLIMATE WINDOWS#
  for (m in range[2]:range[1]){
    for (n in 1:duration){
        if (length(exclude) == 2 && m >= exclude[2] & (m-n) >= exclude[2] & n <= exclude[1]){
          next
        }
      if ( (m - n) >= (range[2] - 1)){  # do not use windows that overshoot the closest possible day in window
        if (stat != "slope" || n > 1){
          windowopen  <- m - range[2] + 1
          windowclose <- windowopen - n + 1
          if (stat == "slope"){ 
            time             <- seq(1, n, 1)
            modeldat$climate <- apply(cmatrix[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
          } else { 
            ifelse (n == 1, modeldat$climate <- cmatrix[, windowclose:windowopen], 
                    modeldat$climate <- apply(cmatrix[, windowclose:windowopen], 1, FUN = stat))
          }
          if (min(modeldat$climate) <= 0 & func == "log" || min(modeldat$climate) <= 0 & func == "inv"){
            stop("func = log or inv cannot be used with climate values <= 0. 
                 Consider adding a constant to climate data to remove these values")
          }
          
          if (is.null(centre[[1]]) == FALSE){
            if(centre[[2]] == "both"){
              modeldat$wgdev  <- wgdev(modeldat$climate, centre[[1]])
              modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
              modeloutput     <- update(modeloutput, .~., data = modeldat)
            }
            if(centre[[2]] == "mean"){
              modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
              modeloutput     <- update(modeloutput, .~., data = modeldat)
            }
            if(centre[[2]] == "dev"){
              modeldat$wgdev  <- wgdev(modeldat$climate, centre[[1]])
              modeloutput     <- update(modeloutput, .~., data = modeldat)
            }
          } else {
            modeloutput <- my_update(modeloutput, .~., data = modeldat)
          }
          
          # If valid, perform k-fold crossvalidation
          if (k > 1) {      
            for (k in 1:k) {
              test                     <- subset(modeldat, modeldat$K == k) # Create the test dataset
              train                    <- subset(modeldat, modeldat$K != k) # Create the train dataset
              baselinecv               <- update(baseline, yvar~., data = train) # Refit the model without climate using the train dataset
              modeloutputcv            <- update(modeloutput, yvar~., data = train)  # Refit the model with climate using the train dataset
              test$predictions         <- predict(modeloutputcv, newdata = test, allow.new.levels = TRUE) # Test the output of the climate model fitted using the test data
              test$predictionsbaseline <- predict(baselinecv, newdata = test, allow.new.levels = TRUE) # Test the output of the null models fitted using the test data
              
              num        <- length(test$predictions) # Determine the length of the test dataset
              p          <- num - df.residual(modeloutputcv)  # Determine df for the climate model
              mse        <- sum((test$predictions - test[, 1]) ^ 2) / num
              p_baseline <- num - df.residual(baselinecv)  # Determine df for the baseline model
              #calculate mean standard errors for climate model
              #calc mse only works non-categorical yvars, e.g. normal, binary, count data 
              mse_baseline <- sum((test$predictionsbaseline - test[, 1]) ^ 2) / num
              #calculate mean standard errors for null model
              AICc_cv          <- num * log(mse) + (2 * p * (p + 1)) / (num - p - 1)
              AICc_cv_baseline <- num * log(mse_baseline) + (2 * p_baseline * (p_baseline + 1)) / (num - p_baseline - 1)
              #Calculate AICc values for climate and baseline models
              #rmse_corrected<-sqrt(sum((test$predictions-test[,1])^2)/modeloutputcv$df[1])
              ifelse (k == 1, AICc_cvtotal <- AICc_cv, AICc_cvtotal <- AICc_cvtotal + AICc_cv)              
              ifelse (k == 1, AICc_cv_basetotal <- AICc_cv_baseline, AICc_cv_basetotal <- AICc_cv_basetotal + AICc_cv_baseline)
              #Add up the AICc values for all iterations of crossvalidation
            }
            AICc_cv_avg          <- AICc_cvtotal / k # Determine the average AICc value of the climate model from cross validations
            AICc_cv_baseline_avg <- AICc_cv_basetotal / k # Determine the average AICc value of the null model from cross validations
            deltaAICc_cv         <- AICc_cv_avg - AICc_cv_baseline_avg # Calculate delta AICc
          }
          
          #Add model parameters to list
          if (k > 1){
            modlist$ModelAICc[[modno]]    <- AICc_cv_avg
            modlist$deltaAICc[[modno]]    <- deltaAICc_cv
          } else {
            modlist$deltaAICc[[modno]] <- AICc(modeloutput) - AICc(baseline)
            modlist$ModelAICc[[modno]] <- AICc(modeloutput)
          }
       
          modlist$WindowOpen[[modno]]  <- m
          modlist$WindowClose[[modno]] <- m - n + 1
          
          if (class(baseline)[length(class(baseline))]=="coxph") {
            if (func == "quad"){
              modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))-1]
              modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-1]
              modlist$ModelBetaQ[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
              modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
              modlist$ModelBetaC[[modno]] <- NA
              modlist$ModelInt[[modno]]   <- 0
            } else if (func == "cub"){
              modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))-2]
              modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-2]
              modlist$ModelBetaQ[[modno]] <- coef(modeloutput)[length(coef(modeloutput))-1]
              modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-1]
              modlist$ModelBetaC[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
              modlist$Std.ErrorC[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
              modlist$ModelInt[[modno]]   <- 0
            } else if (func == "centre"){
              if(centre[[2]] == "both"){
                modlist$WithinGrpMean[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.ErrorMean[[modno]] <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
                modlist$WithinGrpDev[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))-1]
                modlist$Std.ErrorDev[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-1]
                modlist$ModelInt[[modno]]      <- 0
              }
              if(centre[[2]] == "mean"){
                modlist$WithinGrpMean[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
                modlist$ModelInt[[modno]]      <- 0
              }
              if(centre[[2]] == "dev"){
                modlist$WithinGrpDev[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
                modlist$ModelInt[[modno]]      <- 0
              }
            } else {
              modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))]
              modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
              modlist$ModelBetaQ[[modno]] <- NA
              modlist$ModelBetaC[[modno]] <- NA
              modlist$ModelInt[[modno]]   <- 0
            }
          } 
          else if (length(attr(class(modeloutput),"package")) > 0 && attr(class(modeloutput), "package") == "lme4"){            
            if (func == "quad"){
              modlist$ModelBeta[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
              modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
              modlist$ModelBetaQ[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
              modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
              modlist$ModelBetaC[[modno]] <- NA
              modlist$ModelInt[[modno]]   <- fixef(modeloutput)[1]
            } else if (func == "cub"){
              modlist$ModelBeta[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 2]
              modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
              modlist$ModelBetaQ[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
              modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
              modlist$ModelBetaC[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
              modlist$Std.ErrorC[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
              modlist$ModelInt[[modno]]   <- fixef(modeloutput)[1]
            } else if (func == "centre"){
              if(centre[[2]] == "both"){
                modlist$WithinGrpMean[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
                modlist$Std.ErrorMean[[modno]] <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$WithinGrpDev[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                modlist$Std.ErrorDev[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
                modlist$ModelInt[[modno]]      <- fixef(modeloutput)[1]
              }
              if(centre[[2]] == "mean"){
                modlist$WithinGrpMean[[modno]] <- fixef(modeloutput)[length(fixef(modeloutput))]
                modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelInt[[modno]]      <- fixef(modeloutput)[1]
              }
              if(centre[[2]] == "dev"){
                modlist$WithinGrpDev[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                modlist$Std.Error[[modno]]     <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelInt[[modno]]      <- fixef(modeloutput)[1]
              }
            } else {
              modlist$ModelBeta[[modno]]  <- fixef(modeloutput)[length(fixef(modeloutput))]
              modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
              modlist$ModelBetaQ[[modno]] <- NA
              modlist$ModelBetaC[[modno]] <- NA
              modlist$ModelInt[[modno]]   <- fixef(modeloutput)[1]
            }
          } else {
            if (func == "quad"){
              modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput)) - 1]
              modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
              modlist$ModelBetaQ[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
              modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
              modlist$ModelBetaC[[modno]] <- NA
              modlist$ModelInt[[modno]]   <- coef(modeloutput)[1]
            } else if (func == "cub"){
              modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput)) - 2]
              modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
              modlist$ModelBetaQ[[modno]] <- coef(modeloutput)[length(coef(modeloutput)) - 1]
              modlist$Std.ErrorQ[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
              modlist$ModelBetaC[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
              modlist$Std.ErrorC[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][4]
              modlist$ModelInt[[modno]]   <- coef(modeloutput)[1]
            } else if (func == "centre"){
              if(centre[[2]] == "both"){
                modlist$WithinGrpMean[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.ErrorMean[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$WithinGrpDev[[modno]]  <- coef(modeloutput)[length(coef(modeloutput)) - 1]
                modlist$Std.ErrorDev[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][3]
                modlist$ModelInt[[modno]]      <- coef(modeloutput)[1]
              }
              if(centre[[2]] == "mean"){
                modlist$WithinGrpMean[[modno]] <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.ErrorMean[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelInt[[modno]]      <- coef(modeloutput)[1]
              }
              if(centre[[2]] == "dev"){
                modlist$WithinGrpDev[[modno]]  <- coef(modeloutput)[length(coef(modeloutput)) - 1]
                modlist$Std.ErrorDev[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelInt[[modno]]      <- coef(modeloutput)[1]
              }
            } else {
              modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))]
              modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
              modlist$ModelBetaQ[[modno]] <- NA
              modlist$ModelBetaC[[modno]] <- NA
              modlist$ModelInt[[modno]]   <- coef(modeloutput)[1]
            }
          }
          modno <- modno + 1        #Increase modno#
        }
      }
    }  
    #Fill progress bar
    setTxtProgressBar(pb, modno - 1)
  }
  
  #Save the best model output
  m <- (modlist$WindowOpen[modlist$ModelAICc %in% min(modlist$ModelAICc)])
  n <- (modlist$WindowOpen[modlist$ModelAICc %in% min(modlist$ModelAICc)]) - (modlist$WindowClose[modlist$ModelAICc %in% min(modlist$ModelAICc)]) + 1
  windowopen  <- m[1] - range[2] + 1
  windowclose <- windowopen - n[1] + 1
  if (stat == "slope"){
    time      <- seq(1, n[1], 1)
    modeldat$climate <- apply(cmatrix[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
  } else {
    ifelse (windowopen - windowclose == 0, 
            modeldat$climate <- cmatrix[, windowclose:windowopen], 
            modeldat$climate <- apply(cmatrix[, windowclose:windowopen], 1, FUN = stat))
  }
  
  if (is.null(centre[[1]]) == FALSE){
    if (centre[[2]] == "both"){
        modeldat$WGdev   <- wgdev(modeldat$climate, centre[[1]])
        modeldat$WGmean  <- wgmean(modeldat$climate, centre[[1]])
        LocalModel       <- update(modeloutput, .~., data = modeldat)
    }
    if (centre[[2]] == "dev"){
      modeldat$WGdev   <- wgdev(modeldat$climate, centre[[1]])
      LocalModel       <- update(modeloutput, .~., data = modeldat)
    }
    if (centre[[2]] == "mean"){
      modeldat$WGmean  <- wgmean(modeldat$climate, centre[[1]])
      LocalModel       <- update(modeloutput, .~., data = modeldat)
    }
    modlist$Function <- "centre"
  } else {
    LocalModel       <- update(modeloutput, .~., data = modeldat)
    modlist$Function <- func
  }
  
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
  
  if (nrandom == 0){
    if (is.null(centre[[1]]) == FALSE){
      LocalData         <- model.frame(LocalModel)
      LocalData$climate <- modeldat$climate
    } else {
      LocalData <- model.frame(LocalModel)
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

##################################################################################

#Function to convert dates into day/week/month number
convertdate <- function(bdate, cdate, xvar, xvar2 = NULL, cinterval, type, 
                        refday, cross = FALSE, cohort, spatial){
  
  bdate  <- as.Date(bdate, format = "%d/%m/%Y") # Convert the date variables into the R date format
  if(is.null(spatial) == FALSE) {
    SUB.DATE <- list()
    NUM <- 1
    for(i in levels(as.factor(spatial[[2]]))){
      SUB <- cdate[which(spatial[[2]] == i)]
      SUB.DATE[[NUM]] <- data.frame(Date = seq(min(as.Date(SUB, format = "%d/%m/%Y")), max(as.Date(SUB, format = "%d/%m/%Y")), "days"),
                                    spatial = i)
      if (length(SUB.DATE[[NUM]]$Date) != length(unique(SUB.DATE[[NUM]]$Date))){
        stop ("There are duplicate dayrecords in climate data")
      }
      NUM <- NUM + 1
    }
    spatialcdate <- plyr::rbind.fill(SUB.DATE)
    cdate2       <- spatialcdate$Date
    cintno       <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to cintno 1
    realbintno   <- as.numeric(bdate) - min(as.numeric(cdate2)) + 1
  } else {
    cdate2     <- seq(min(as.Date(cdate, format = "%d/%m/%Y")), max(as.Date(cdate, format = "%d/%m/%Y")), "days")
    cintno     <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to cintno 1
    realbintno <- as.numeric(bdate) - min(as.numeric(cdate2)) + 1
    if (length(cintno) != length(unique(cintno))){
      stop ("There are duplicate dayrecords in climate data")
    }
  }
  cdate  <- as.Date(cdate, format = "%d/%m/%Y")
  
  if(is.null(spatial) == FALSE){
    for(i in levels(as.factor(spatial[[2]]))){
      SUB <- cdate[which(spatial[[2]] == i)]
      if (min(SUB) > min(bdate) | max(SUB) < max(bdate)){
        stop("Climate data does not cover all years of biological data. Please increase range of climate data")
      }
    }
  } else if (min(cdate) > min(bdate) | max(cdate) < max(bdate)){
    stop("Climate data does not cover all years of biological data. Please increase range of climate data")
  }

  
  if (is.null(xvar2) == FALSE){
    if(is.null(spatial) == FALSE){
      xvar2      <- data.frame(Clim = xvar2, spatial = spatial[[2]])
      cdatetemp  <- data.frame(Date = cdate, spatial = spatial[[2]])
      split.list <- list()
      NUM <- 1
      for(i in levels(xvar2$spatial)){
        SUB <- subset(xvar2, spatial == i)
        SUBcdate  <- subset(cdatetemp, spatial == i)
        SUBcdate2 <- subset(spatialcdate, spatial == i)
        rownames(SUB) <- seq(1, nrow(SUB), 1)
        rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
        NewClim    <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)]
        Newspatial <- rep(i, times = length(NewClim))
        split.list[[NUM]] <- data.frame(NewClim, Newspatial)
        NUM <- NUM + 1
      }
      xvar2    <- (plyr::rbind.fill(split.list))$NewClim
      climspatial <- (plyr::rbind.fill(split.list))$Newspatial
    } else {
      xvar2    <- xvar2[match(cdate2, cdate)]
    }
  }
  

  if(is.null(spatial) == FALSE){
    xvar       <- data.frame(Clim = xvar, spatial = spatial[[2]])
    cdate      <- data.frame(Date = cdate, spatial = spatial[[2]])
    split.list <- list()
    NUM <- 1
    for(i in levels(xvar$spatial)){
      SUB <- subset(xvar, spatial == i)
      SUBcdate  <- subset(cdate, spatial == i)
      SUBcdate2 <- subset(spatialcdate, spatial == i)
      rownames(SUB) <- seq(1, nrow(SUB), 1)
      rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
      NewClim    <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)]
      Newspatial <- rep(i, times = length(NewClim))
      split.list[[NUM]] <- data.frame(NewClim, Newspatial)
      NUM <- NUM + 1
    }
    xvar    <- (plyr::rbind.fill(split.list))$NewClim
    climspatial <- (plyr::rbind.fill(split.list))$Newspatial
  } else {
    xvar    <- xvar[match(cdate2, cdate)]
  }
  
  
  
  if (cinterval != "day" && cinterval != "week" && cinterval != "month"){
    stop("cinterval should be either day, week or month")
  }
  
  if (cross == FALSE){
    if (cinterval == "day"){  
      if (type == "absolute"){
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
          }
        } else {
          bintno            <- as.numeric(as.Date(paste(refday[1], refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1 
        }
      } else {
        bintno <- realbintno
      }
    } else if (cinterval == "week"){
      cintno      <- ceiling((as.numeric(cdate2) - min(as.numeric(cdate2)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to cintno 1
      realbintno  <- ceiling((as.numeric(bdate) - min(as.numeric(cdate2)) + 1) / 7)
      if(is.null(spatial) == FALSE){
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial)
        newclim2    <- melt(newclim, id = c("cintno", "spatial"))
        newclim3    <- cast(newclim2, cintno + spatial ~ variable, mean)
        newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ]
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        climspatial <- newclim3$spatial
      } else {
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar)
        newclim2    <- melt(newclim, id = "cintno")
        newclim3    <- cast(newclim2, cintno ~ variable, mean)
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
      }
      if (type == "absolute"){
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- ceiling((as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1)/7)
          }
        } else {
          bintno <- ceiling((as.numeric(as.Date(paste(refday[1], refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1)/7) 
        }
      } else {
        bintno <- realbintno
      }
    } else if (cinterval == "month"){ 
      cmonth     <- lubridate::month(cdate2)
      cyear      <- year(cdate2) - min(year(cdate2))
      cintno     <- cmonth + 12 * cyear
      realbintno <- lubridate::month(bdate) + 12 * (year(bdate) - min(year(cdate2)))
      if(is.null(spatial) == FALSE){
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial)
        newclim2    <- melt(newclim, id = c("cintno", "spatial"))
        newclim3    <- cast(newclim2, cintno + spatial ~ variable, mean)
        newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ]
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        climspatial <- newclim3$spatial
      } else {
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar)
        newclim2   <- melt(newclim, id = "cintno")
        newclim3   <- cast(newclim2, cintno ~ variable, mean)
        cintno     <- newclim3$cintno
        xvar       <- newclim3$xvar 
      }
      if (type == "absolute"){
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- refday[2] + 12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
          }
        } else {
          bintno            <- refday[2] + 12 * (year(bdate) - min(year(cdate2)))
        }
      } else {
        bintno <- realbintno
      }
    }
  } else {
    if (cinterval == "day"){  
      if (type == "absolute"){   
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
          }
        } else {
          bintno <- as.numeric(as.Date(paste(refday[1], refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1 
        }
      } else {
        bintno <- realbintno
      }    
    } else if (cinterval == "week"){
      cintno     <- ceiling((as.numeric(cdate2) - min(as.numeric(cdate2)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to cintno 1
      realbintno <- ceiling((as.numeric(bdate) - min(as.numeric(cdate2)) + 1) / 7)
      if(is.null(spatial) == FALSE){
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2, "spatial" = climspatial)
        newclim2    <- melt(newclim, id = c("cintno", "spatial"))
        newclim3    <- cast(newclim2, cintno + spatial ~ variable, mean)
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        xvar2       <- newclim3$xvar2
        climspatial <- newclim3$spatial
      } else {
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2)
        newclim2   <- melt(newclim, id = "cintno")
        newclim3   <- cast(newclim2, cintno ~ variable, mean)
        cintno     <- newclim3$cintno
        xvar       <- newclim3$xvar
        xvar2      <- newclim3$xvar2 
      }
      if (type == "absolute"){ 
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- ceiling((as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1)/7)
          }
        } else {
          bintno <- ceiling((as.numeric(as.Date(paste(refday[1], refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1)/7) 
        }
      } else {
        bintno <- realbintno
      }
    } else if (cinterval == "month"){ 
      cmonth     <- lubridate::month(cdate2)
      cyear      <- year(cdate2) - min(year(cdate2))
      cintno     <- cmonth + 12 * cyear
      realbintno <- lubridate::month(bdate) + 12 * (year(bdate) - min(year(cdate2)))
      if(is.null(spatial) == FALSE){
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2, "spatial" = climspatial)
        newclim2    <- melt(newclim, id = c("cintno", "spatial"))
        newclim3    <- cast(newclim2, cintno + spatial ~ variable, mean)
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        xvar2       <- newclim3$xvar2
        climspatial <- newclim3$spatial
      } else {
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2)
        newclim2   <- melt(newclim, id = "cintno")
        newclim3   <- cast(newclim2, cintno ~ variable, mean)
        cintno     <- newclim3$cintno
        xvar       <- newclim3$xvar
        xvar2      <- newclim3$xvar2 
      }
      if (type == "absolute"){ 
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- refday[2] + 12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
          }
        } else {
          bintno            <- refday[2] + 12 * (year(bdate) - min(year(cdate2)))
        }
      } else {
        bintno <- realbintno
      }
    }
  }
  if(is.null(spatial) == FALSE){
    if(is.null(xvar2) == FALSE){
      return(list(cintno = data.frame(Date = cintno, spatial = climspatial),
                  bintno = data.frame(Date = bintno, spatial = spatial[[1]]),
                  xvar = data.frame(Clim = xvar, spatial = climspatial), 
                  xvar2 = data.frame(Clim = xvar2, spatial = climspatial)))
    } else {
      return(list(cintno = data.frame(Date = cintno, spatial = climspatial),
                  bintno = data.frame(Date = bintno, spatial = spatial[[1]]),
                  xvar = data.frame(Clim = xvar, spatial = climspatial)))
    }
  } else {
    if(is.null(xvar2) == FALSE){
      return(list(cintno = cintno, bintno = bintno, xvar = xvar, xvar2 = xvar2))
    } else {
      return(list(cintno = cintno, bintno = bintno, xvar = xvar)) 
    }
  }
}

##############################################################################################################################

# define a function that returns the AICc or -2LogLikelihood of model using Generalized Extreme Value (GEV) weight function
modloglik_G <- function(par = par, modeloutput = modeloutput, 
                        duration = duration, cmatrix = cmatrix, 
                        nullmodel = nullmodel, funcenv = funcenv){
  
  j                     <- seq(-10, 10, by = (2 * 10 / duration))  # value of 10 is chosen arbitrarily but seems to be suitable
  weight                <- dgev(j[1:duration], loc = par[3], scale = par[2], shape = par[1], log = FALSE)   # calculate weights using the GEV probability distribution function
  weight[is.na(weight)] <- 0 # the GEV function produces "NA" for some values of j if the parameter constraint on kappa, lambda and mu is not satisfied. We put such values to zero.
  
  if (sum(weight) == 0){
    weight <- weight + 1
  }
  
  weight                                <- weight / sum(weight) 
  print(weight)
  funcenv$modeldat$climate              <- apply(cmatrix, 1, FUN = function(x) {sum(x*weight)})    # calculate weighted mean from weather data
  modeloutput                           <- update(modeloutput, .~., data = funcenv$modeldat)   # rerun regression model using new weather index
  deltaAICc                             <- AICc(modeloutput) - nullmodel
  funcenv$DAICc[[funcenv$modno]]        <- deltaAICc
  funcenv$par_shape[[funcenv$modno]]    <- par[1]
  funcenv$par_scale[[funcenv$modno]]    <- par[2]
  funcenv$par_location[[funcenv$modno]] <- par[3]
  
  # plot the weight function and corresponding weather index being evaluated
  par(mfrow = c(3, 2))
  plot( (weight / sum(weight)), type = "l", ylab = "weight", xlab = "timestep (e.g. days)", main = "Output of current weighted window being tested")
  plot(as.numeric(funcenv$par_shape), type = "l", ylab = "shape parameter", xlab = "convergence step", main = "GEV parameter values being tested")
  plot(as.numeric(funcenv$DAICc), type = "l", ylab = expression(paste(Delta, "AICc")), xlab = "convergence step")
  plot(as.numeric(funcenv$par_scale), type = "l", ylab = "scale parameter", xlab = "convergence step")
  plot(funcenv$modeldat$climate[1:(3 * duration)], type = "s", ylab = "weighted mean of climate", xlab = "timestep (e.g. days)")
  plot(as.numeric(funcenv$par_location), type = "l", ylab = "location parameter", xlab = "convergence step")
  
  funcenv$modno <- funcenv$modno + 1
  return(deltaAICc)  # returns deltaAICc as optim() minimizes! 
}


# define a function that returns the AICc or -2LogLikelihood of model using Weibull weight function
modloglik_W <- function(par = par,  modeloutput = modeloutput, duration = duration, 
                        cmatrix = cmatrix, modeldat = modeldat, nullmodel =  nullmodel, funcenv = funcenv){
  
  j                     <- seq(1:duration) / duration # rescale j to interval [0,1]
  weight                <- weibull3(x = j, shape = par[1], scale = par[2], location = par[3])  # calculate weights using the Weibull probability distribution function
  weight[is.na(weight)] <- 0
  
  if (sum(weight) == 0){
    weight <- weight + 1
  }
  
  weight                                <- weight / sum(weight)
  funcenv$modeldat$climate              <- apply(cmatrix, 1, FUN = function(x) {sum(x*weight)})    # calculate weighted mean from weather data
  modeloutput                           <- update(modeloutput, .~., data = funcenv$modeldat)   # rerun regression model using new weather index
  deltaAICc                             <- AICc(modeloutput) - nullmodel
  funcenv$DAICc[[funcenv$modno]]        <- deltaAICc
  funcenv$par_shape[[funcenv$modno]]    <- par[1]
  funcenv$par_scale[[funcenv$modno]]    <- par[2]
  funcenv$par_location[[funcenv$modno]] <- par[3]

  # plot the weight function and corresponding weather index being evaluated
  par(mfrow = c(3, 2))
  plot((weight/sum(weight)), type = "l", ylab = "weight", xlab = "time step (e.g days)", main = "Output of current weighted window being tested")
  plot(as.numeric(funcenv$par_shape), type = "l", ylab = "shape parameter", xlab = "convergence step", main = "Weibull parameter values being tested")
  plot(as.numeric(funcenv$DAICc), type = "l", ylab = expression(paste(Delta, "AICc")), xlab = "convergence step")
  plot(as.numeric(funcenv$par_scale), type = "l", ylab = "scale parameter", xlab = "convergence step")
  plot(funcenv$modeldat$climate[1:(duration)], type = "s", ylab = "weighted mean of weather", xlab = "time step (e.g days)")
  plot(as.numeric(funcenv$par_location), type = "l", ylab = "location parameter", xlab = "convergence step")
  
  funcenv$modno <- funcenv$modno + 1
  return(deltaAICc)  # returns deltaAICc as optim() minimizes! 
}

##################################################################################

weibull3<-function(x, shape,scale,location){
  shape / scale * ((x - location) / scale) ^ (shape - 1) * exp( - ((x - location) / scale) ^ shape)
}

##################################################################################

my_update <- function(mod, formula = NULL, data = NULL) {
  call <- getCall(mod)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }
  term <- terms(mod)
  if (is.null(term)) {
    stop("Model object does not support updating (no terms)", call. = FALSE)
  }
  
  if (!is.null(data)) call$data <- data
  if (!is.null(formula)) call$formula <- update.formula(call$formula, formula)
  env <- attr(term, ".Environment")
  
  eval(call, env, parent.frame())
}

##################################################################################

#Function to determine within group mean and deviance for centring

wgdev <- function(covar, groupvar) {
  a            <- unique(factor(groupvar))
  groups       <- length(a)
  temp         <- rep(NA, groups)
  observations <- length(covar)
  groupmean    <- rep(NA, observations)
  groupdev     <- rep(NA, observations)
  
  for (i in 1:groups){
    b       <- which(groupvar == a[i])
    temp[i] <- mean(covar[b], na.rm=TRUE)
  }
  
  for (j in 1:observations){
    c            <- which(a == groupvar[j])
    groupmean[j] <- temp[c]
    groupdev[j]  <- covar[j] - groupmean[j]
  }
  return(groupdev)
}

wgmean <- function(covar, groupvar){
  a            <- unique(factor(groupvar))
  groups       <- length(a)
  observations <- length(covar)
  temp         <- rep(NA, groups)
  groupmean    <- rep(NA, observations)
  groupdev     <- rep(NA, observations)
  
  for (i in 1:groups){
    b       <- which(groupvar == a[i])
    temp[i] <- mean(covar[b], na.rm=TRUE)
  }
  
  for (j in 1:observations){
    c            <- which(a == groupvar[j])
    groupmean[j] <- temp[c]
    groupdev[j]  <- covar[j] - groupmean[j]
  }
  groupmean[which(is.nan(groupmean)==TRUE)]<-NA
  return(groupmean)
}

##################################################################################

skim <- function(winoutput, duration, cutoff) {
  winoutput$Duration <- winoutput$WindowOpen - winoutput$WindowClose
  winoutput$Filter   <- winoutput$WindowOpen * 0
  winoutput$Filter[which(winoutput$WindowOpen >= cutoff &  winoutput$WindowClose >= cutoff & winoutput$Duration < duration)] <- 1
  winoutput<-subset(winoutput, winoutput$Filter == 0)
  return(winoutput)
}

##################################################################################

merge_results <- function(dataset1, dataset2){
  
  new_combos <- rbind.fill(dataset1$combos, dataset2$combos)
  rownames(new_combos) <- seq(length = nrow(new_combos))
  
  dataset1[[length(dataset1)]] <- NULL
  dataset2[[length(dataset2)]] <- NULL
  
  new_dataset <- c(dataset1, dataset2)
  new_dataset$combos <- new_combos
  
  return(new_dataset)
  
}

##################################################################################

circle <- function(centre = c(0,0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- centre[1] + r * cos(tt)
  yy <- centre[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

##################################################################################

#Function to temporarily adjust global R options (used in pvalue to enforce scientific notation)

withOptions <- function(optlist, expr)
{
  oldopt <- options(optlist)
  on.exit(options(oldopt))
  expr <- substitute(expr)
  eval.parent(expr)
}