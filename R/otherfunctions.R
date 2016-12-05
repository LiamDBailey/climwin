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
                    cmissing = FALSE, cinterval = "day", nrandom = 0, k = 0,
                    spatial, upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"),
                    cohort = NULL, fast){
  
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
  if (length(exclude) == 2){ maxmodno  <- maxmodno- exclude[1]*(duration-exclude[2]-1)+(exclude[1]-1)*exclude[1]/2 }
  if (stat == "slope") { 
    ifelse(is.na(exclude[2])==TRUE,  maxmodno  <- maxmodno - duration, maxmodno  <- maxmodno-exclude[2]-1)
  } 
  cont      <- convertdate(bdate = bdate, cdate = cdate, xvar = xvar, 
                           cinterval = cinterval, type = type, 
                           refday = refday, cohort = cohort, spatial = spatial)   # create new climate dataframe with continuous daynumbers, leap days are not a problem
  
  if(is.null(spatial) == FALSE){
    
    if ((min(cont$bintno$Date) - range[1]) < min(cont$cintno$Date)){
      stop(paste("You do not have enough climate data to search ", range[1], " ", cinterval, "s before ", min(as.Date(bdate, format = "%d/%m/%Y")), ". Please adjust the value of range or add additional climate data.", sep = ""))
    }
    
    if (max(cont$bintno$Date) - range[2] > max(cont$cintno$Date)){
      stop(paste("You need more recent climate data. The most recent climate data is from ", max(as.Date(cdate, format = "%d/%m/%Y")), " while the most recent biological data is from ", max(as.Date(cdate, format = "%d/%m/%Y")), sep = ""))
    }
    
  } else {

    if ((min(cont$bintno) - range[1]) < min(cont$cintno)){
      stop(paste("You do not have enough climate data to search ", range[1], " ", cinterval, "s before ", min(as.Date(bdate, format = "%d/%m/%Y")), ". Please adjust the value of range or add additional climate data.", sep = ""))
    }
    
    if ((max(cont$bintno) - range[2] - 1) > max(cont$cintno)){
      stop(paste("You need more recent climate data. The most recent climate data is from ", max(as.Date(cdate, format = "%d/%m/%Y")), " while the most recent biological data is from ", max(as.Date(cdate, format = "%d/%m/%Y")), sep = ""))
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
        .GlobalEnv$missing <- c(paste("Month:", (lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1)) - (floor((lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1))/12)*12),
                                      "Year:", (floor((which(is.na(cont$xvar)) - 1)/12) + lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))))))
      }
      if (cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", ceiling(((as.numeric((as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y"))) - (floor(which(is.na(cmatrix))/nrow(cmatrix))*7)) - as.numeric(as.Date(paste("01/01/", lubridate::year(as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) + 1) / 7),
                                      "Year:", lubridate::year(as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y"))))
      }
    }

    stop(c("Climate data should not contain NA values: ", length(.GlobalEnv$missing),
           " NA value(s) found. Please add missing climate data or set cmissing=TRUE.
           See object 'missing' for all missing climate data"))
  }
  
  if (cmissing != FALSE && length(which(is.na(cmatrix))) > 0){
    
    print("Missing climate data detected. Please wait while appropriate data is calculated to replace NAs.")
    
    if(cmissing == "method1"){
      
      for(i in which(is.na(cmatrix))){
        
        cmatrix[i] <- mean(c(cmatrix[i - (1:2)], cmatrix[i + (1:2)]))
        
      }
      
    } else if(cmissing == "method2"){
      
      cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"),
                              Year  = lubridate::year(as.Date(cdate, format = "%d/%m/%Y")),
                              Month = lubridate::month(as.Date(cdate, format = "%d/%m/%Y")),
                              Day   = lubridate::day(as.Date(cdate, format = "%d/%m/%Y")))
      
      if(cinterval == "week"){
        
        for(j in 1:nrow(cdate_new)){
          
          cdate_new$Week[j] <- ceiling((as.numeric(cdate_new$Date[j]) - min(as.numeric(subset(cdate_new, Year == cdate_new$Year[j])$Date)) + 1) / 7)
          
        }
        
      }
      
      for(i in which(is.na(cmatrix))){
        
        col <- floor(i/nrow(cmatrix))
        
        brecord <- cont$bintno[i - col*nrow(cmatrix)] - (range[2] + col) - 1
        
        min_date <- min(as.Date(cdate, format = "%d/%m/%Y"))
        
        if(cinterval == "day"){
          
          missing_rec <- as.Date(brecord, format = "%d/%m/%Y", origin = min_date)
          
          cmatrix[i] <- mean(xvar[which(cdate_new$Month == lubridate::month(missing_rec) & cdate_new$Day == lubridate::day(missing_rec))], na.rm = T)

        } else if(cinterval == "week"){
          
          missing_week <- ceiling(((as.numeric((as.Date(bdate[i - col*nrow(cmatrix)], format = "%d/%m/%Y"))) - (col*7)) - as.numeric(as.Date(paste("01/01/", lubridate::year(as.Date(bdate[i - col*nrow(cmatrix)], format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) + 1) / 7)
          
          cmatrix[i] <- mean(xvar[which(cdate_new$Week == missing_week)], na.rm = T)

        } else if(cinterval == "month"){
          
          missing_month <- (lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1)) - (floor((lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1))/12)*12)
          
          cmatrix[i] <- mean(xvar[which(cdate_new$Month == missing_month)], na.rm = T)
          
        }
        
      }
      
    }
    modeldat <- modeldat[complete.cases(cmatrix), ]
    baseline <- update(baseline, yvar~., data = modeldat)
    cmatrix  <- cmatrix[complete.cases(cmatrix), ]
  }
  
  if (is.null(weights(baseline)) == FALSE){
    if (class(baseline)[1] == "glm" & sum(weights(baseline)) == nrow(model.frame(baseline)) || attr(class(baseline), "package") == "lme4" & sum(weights(baseline)) == nrow(model.frame(baseline))){
    } else {
      modeldat$modweights <- weights(baseline)
      baseline <- update(baseline, .~., weights = modeldat$modweights, data = modeldat)
    }
  }

  if(all(!colnames(modeldat) %in% "climate")){
    
    modeldat$climate <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
    
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
    
  } else {
    
    modeloutput <- update(baseline, yvar ~., data = modeldat)
    
    coef_data <- list()
    
  }
  
  if (k > 1){
    modeldat$K <- sample(seq(from = 1, to = length(modeldat$climate), by = 1) %% k + 1)
  }   # create labels k-fold crossvalidation
  
  pb <- txtProgressBar(min = 0, max = maxmodno, style = 3, char = "|")
  
  if(stat == "mean" && fast == TRUE){
    for(n in 1:duration){
      
      if(n == 1){
        
        new_cmatrix <- t(cmatrix)
        
      } else {
        
        new_cmatrix <- roll_mean(t(cmatrix), n = n)
        
      }
      
      for(m in 1:nrow(new_cmatrix)){
        
       modeldat$climate <- new_cmatrix[m, ]
       
       modeloutput <- my_update(modeloutput, .~., data = modeldat)
       
       modlist$deltaAICc[[modno]] <- AICc(modeloutput) - AICc(baseline)
       modlist$ModelAICc[[modno]] <- AICc(modeloutput)
       
       modlist$WindowOpen[[modno]]  <- ((m + range[2]) - 1) + (n - 1)
       modlist$WindowClose[[modno]] <- ((m + range[2]) - 1)
       
       modlist$ModelBeta[[modno]]  <- coef(modeloutput)[length(coef(modeloutput))]
       modlist$Std.Error[[modno]]  <- coef(summary(modeloutput))[, "Std. Error"][2]
       modlist$ModelBetaQ[[modno]] <- NA
       modlist$ModelBetaC[[modno]] <- NA
       modlist$ModelInt[[modno]]   <- coef(modeloutput)[1]
       
       modno <- modno + 1
       
       setTxtProgressBar(pb, modno - 1)
       
      }
      
    }
       
  } else {
  
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
              test$predictions         <- predict(modeloutputcv, newdata = test, allow.new.levels = TRUE, type = "response") # Test the output of the climate model fitted using the test data
              test$predictionsbaseline <- predict(baselinecv, newdata = test, allow.new.levels = TRUE, type = "response") # Test the output of the null models fitted using the test data
              
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
          
          if(any(colnames(model.frame(baseline)) %in% "climate")){
              
              coefs <- coef(summary(modeloutput))[, 1:2]
              
              temp.df <- data.frame("Y", t(coefs[-1, 1]), t(coefs[-1, 2]))
              
              colnames(temp.df) <- c("Custom.mod", colnames(model.frame(modeloutput)[-1]), paste(colnames(model.frame(modeloutput)[-1]), "SE", sep = ""))
              
              coef_data[[modno]] <- temp.df
            
          } else {
            
            if (class(baseline)[length(class(baseline))] == "coxph") {
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
          }
          modno <- modno + 1        #Increase modno#
        }
      }
    }  
    #Fill progress bar
    setTxtProgressBar(pb, modno - 1)
  }
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
  
  if(exists("coef_data")){
    
    modlist <- cbind(modlist, plyr::rbind.fill(coef_data))
    
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

basewin_weight <- function(n, xvar, cdate, bdate, baseline, range, 
                      func = "lin", type, refday, nrandom = 0, centre = NULL,
                      weightfunc = "W", cinterval = "day", cohort = NULL, spatial = NULL,
                      par = c(3, 0.2, 0), control = list(ndeps = c(0.001, 0.001, 0.001)), 
                      method = "L-BFGS-B", cutoff.day = NULL, cutoff.month = NULL,
                      furthest = NULL, closest = NULL, grad = FALSE){
  
  if(is.null(cohort) == TRUE){
    cohort = lubridate::year(as.Date(bdate, format = "%d/%m/%Y")) 
  }
  
  if(type == "variable" || type == "fixed"){
    stop("Parameter 'type' now uses levels 'relative' and 'absolute' rather than 'variable' and 'fixed'.")
  }
  
  if(is.null(furthest) == FALSE & is.null(closest) == FALSE){
    stop("furthest and closest are now redundant. Please use parameter 'range' instead.")
  }
  
  if(is.null(cutoff.day) == FALSE & is.null(cutoff.month) == FALSE){
    stop("cutoff.day and cutoff.month are now redundant. Please use parameter 'refday' instead.")
  }
  
  if(is.null(centre[[1]]) == FALSE){
    func = "centre"
    if(centre[[2]] != "both" & centre[[2]] != "dev" & centre[[2]] != "mean"){
      stop("Please set centre to one of 'both', 'dev', or 'mean'. See help file for details.")
    }
  }
  
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
    } else {
      sample.size <- length(levels(as.factor(lubridate::year(as.Date(bdate, format = "%d/%m/%Y")))))
    }  
  }
  
  if (is.null(centre[[1]]) == FALSE){
    func <- "centre"
  }
  
  if(nrandom == 0){
    xvar = xvar[[1]]    
  }
  
  funcenv       <- environment()
  cont          <- convertdate(bdate = bdate, cdate = cdate, xvar = xvar, 
                               cinterval = cinterval, type = type, 
                               refday = refday, cohort = cohort, spatial = spatial)   
  # create new climate dataframe with continuous daynumbers, leap days are not a problem 
  
  modno         <- 1
  DAICc         <- list()
  par_shape     <- list()
  par_scale     <- list()
  par_location  <- list()
  duration      <- (range[1] - range[2]) + 1
  cmatrix       <- matrix(ncol = (duration), nrow = length(bdate))
  baseline      <- update(baseline, .~.)
  nullmodel     <- AICc(baseline)
  modeldat      <- model.frame(baseline)
  modeldat$yvar <- modeldat[, 1]
  
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
  
  if(all(!colnames(modeldat) %in% "climate")){
    
    modeldat$climate <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
    
    if (func == "lin"){
      modeloutput <- update(baseline, .~. + climate, data = modeldat)
    } else if (func == "quad") {
      modeloutput <- update(baseline, .~. + climate + I(climate ^ 2), data = modeldat)
    } else if (func == "cub") {
      modeloutput <- update(baseline, .~. + climate + I(climate ^ 2) + I(climate ^ 3), data = modeldat)
    } else if (func == "log") {
      modeloutput <- update(baseline, .~. + log(climate), data = modeldat)
    } else if (func == "inv") {
      modeloutput <- update(baseline, .~. + I(climate ^ -1), data = modeldat)
    } else if (func == "centre"){
      if(centre[[2]] == "both"){
        modeldat$wgdev  <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeloutput <- update(baseline, yvar ~. + wgdev + wgmean, data = modeldat)
      }
      if(centre[[2]] == "mean"){
        modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeloutput <- update(baseline, yvar ~. + wgmean, data = modeldat)
      }
      if(centre[[2]] == "dev"){
        modeldat$wgdev  <- matrix(ncol = 1, nrow = nrow(modeldat), seq(from = 1, to = nrow(modeldat), by = 1))
        modeloutput <- update(baseline, yvar ~. + wgdev, data = modeldat)
      }
    } else {
      print("Define func")
    } 
    
  } else {
    
    modeloutput <- update(baseline, yvar~., data = modeldat)
    
  }
  
  # now run one of two optimization functions
  if (weightfunc == "W"){
    if (par[1] <= 0){
      stop("Weibull shape parameter should be >0")
    }
    if (par[2] <= 0){
      stop("Weibull scale parameter should be >0")
    }
    if (par[3] > 0){
      stop("Weibull location parameter should be <=0")
    }
    j      <- seq(1:duration) / duration
    #result <- optimx(par = par, fn = modloglik_W, control = control, 
    #                method = "L-BFGS-B", lower = c(0.0001, 0.0001, -Inf), 
    #                upper = c(Inf, Inf, 0), duration = duration, 
    #                modeloutput = modeloutput, modeldat = modeldat, 
    #                funcenv = funcenv,  
    #                cmatrix = cmatrix, nullmodel = nullmodel)
    #result <- optimx(par = par, fn = modloglik_W, control = control, 
    #                 method = "bobyqa", lower = c(0.0001, 0.0001, -Inf), 
    #                 upper = c(Inf, Inf, 0), duration = duration, 
    #                 modeloutput = modeloutput, modeldat = modeldat, 
    #                 funcenv = funcenv,  
    #                 cmatrix = cmatrix, nullmodel = nullmodel)  
    if(grad == TRUE){
      
      result <- optim(par = par, fn = modloglik_W, 
                      gr = Uni_grad_W, 
                      control = control, 
                      method = method, lower = c(0.0001, 0.0001, -Inf), 
                      upper = c(Inf, Inf, 0), duration = duration, 
                      modeloutput = modeloutput, modeldat = modeldat, 
                      funcenv = funcenv,  
                      cmatrix = cmatrix, nullmodel = nullmodel) 
      
    } else {
      
      result <- optim(par = par, fn = modloglik_W,
                      control = control, 
                      method = method, lower = c(0.0001, 0.0001, -Inf), 
                      upper = c(Inf, Inf, 0), duration = duration, 
                      modeloutput = modeloutput, modeldat = modeldat, 
                      funcenv = funcenv,  
                      cmatrix = cmatrix, nullmodel = nullmodel)
      
    }
    #result  <- nlminb(start = par, objective = modloglik_W, control = list(step.min = 0.001, step.max = 0.001),
    #                  lower = c(0.0001, 0.0001, -100), upper = c(100, 100, 0),
    #                  duration = duration, modeloutput = modeloutput, modeldat = modeldat,
    #                  funcenv = funcenv, cmatrix = cmatrix, nullmodel = nullmodel)
    #result  <- lbfgs(x0 = par, fn = modloglik_W, control = list(xtol_rel = 1e-10),
    #                 lower = c(0.0001, 0.0001, -Inf), upper = c(Inf, Inf, 0),
    #                 duration = duration, modeloutput = modeloutput, modeldat = modeldat,
    #                 funcenv = funcenv, cmatrix = cmatrix, nullmodel = nullmodel)
    #result  <- tnewton(x0 = par, fn = modloglik_W, control = list(xtol_rel = 1e-10),
    #                   lower = c(0.0001, 0.0001, -Inf), upper = c(Inf, Inf, 0),
    #                   duration = duration, modeloutput = modeloutput, modeldat = modeldat,
    #                   funcenv = funcenv, cmatrix = cmatrix, nullmodel = nullmodel)
    #result  <- varmetric(x0 = par, fn = modloglik_W, control = list(xtol_rel = 1e-10),
    #                     lower = c(0.0001, 0.0001, -Inf), upper = c(Inf, Inf, 0),
    #                     duration = duration, modeloutput = modeloutput, modeldat = modeldat,
    #                     funcenv = funcenv, cmatrix = cmatrix, nullmodel = nullmodel)
    
    if(n == 1){
      
      print(result)
      
    }
    
  } else if (weightfunc == "G"){
    if (par[2] <= 0){
      stop("GEV scale parameter should be >0")
    }
    j      <- seq(-10, 10, by = (2 * 10 / duration))
    
    if(grad == TRUE){
      
      result <- optim(par = par, fn = modloglik_G, 
                      gr = Uni_grad_G, 
                      control = control, 
                      method = method, lower = c(-Inf, 0.0001, -Inf), 
                      upper = c(Inf, Inf, Inf), duration = duration, 
                      modeloutput = modeloutput, funcenv = funcenv,
                      cmatrix = cmatrix, nullmodel = nullmodel)
      
    } else {
      
      result <- optim(par = par, fn = modloglik_G, 
                      gr = Uni_grad_G, 
                      control = control, 
                      method = method, lower = c(-Inf, 0.0001, -Inf), 
                      upper = c(Inf, Inf, Inf), duration = duration, 
                      modeloutput = modeloutput, funcenv = funcenv,
                      cmatrix = cmatrix, nullmodel = nullmodel)
      
    }
    
    if(n == 1){
      
      print(result)
      
    }
    
  } else if (weightfunc == "U"){
    
    if(length(par) > 2){
      print("Uniform distribution only uses two parameters (start and end). All other parameter values are ignored.")
    }
    
    if(par[1] > range[1]){
      stop(paste("Uniform scale parameter 1 must be <= the possible window range (", range[1], ")"))
    }
    
    if(par[2] > range[1]){
      stop(paste("Uniform scale parameter 2 must be <= the possible window range (", range[1], ")"))
    }
    
    if(par[1] < par[2]){
      stop(paste("The end parameter must be larger than the start parameter"))
    }
    
    j <- seq(0, 1, length.out = duration)
    
    if(grad == TRUE){
      
      result <- optim(par = par, fn = modloglik_Uni, 
                      gr = Uni_grad_U,
                      control = control,
                      method = method, lower = c(0, 0), upper = c(range[1], range[1]), duration = duration,
                      modeloutput = modeloutput, funcenv = funcenv,
                      cmatrix = cmatrix, nullmodel = nullmodel) 
      
    } else {
      
      result <- optim(par = par, fn = modloglik_Uni,
                      control = control,
                      method = method, lower = c(0, 0), upper = c(range[1], range[1]), duration = duration,
                      modeloutput = modeloutput, funcenv = funcenv,
                      cmatrix = cmatrix, nullmodel = nullmodel)
      
    }
    
    if(n == 1){
      
      print(result)
      
    }
    
  } else {
    stop("Please choose Method to equal either W, U or G")
  }
  
  if(weightfunc == "U"){
    
    WeightedOutput           <- data.frame(DelatAICc = as.numeric(result$value),
                                           duration = duration,
                                           start = as.numeric(result$par[1]),
                                           end = as.numeric(result$par[2]),
                                           Function = func, Weight_function = weightfunc,
                                           sample.size = sample.size)
    colnames(WeightedOutput) <- c("deltaAICc", "duration", "start", "end", "function", "Weight_function", "sample.size")
    
  } else {
    
    WeightedOutput                <- data.frame(DelatAICc = as.numeric(result$value),
                                                duration = duration,
                                                shape = as.numeric(result$par[1]),
                                                scale = as.numeric(result$par[2]),
                                                location = as.numeric(result$par[3]),
                                                Function = func, Weight_function = weightfunc,
                                                sample.size = sample.size)
    colnames(WeightedOutput) <- c("deltaAICc", "duration", "shape", "scale", "location", "function", "Weight_function", "sample.size") 
    
  }
  
  if(weightfunc == "W"){
    
    weight <- weibull3(x = j[1:duration], shape = as.numeric(result$par[1]), 
                       scale = as.numeric(result$par[2]), 
                       location = as.numeric(result$par[3]))
    
  } else if(weightfunc == "G"){
    
    weight <- dgev(j[1:duration], shape = as.numeric(result$par[1]), 
                   scale = as.numeric(result$par[2]), 
                   loc = as.numeric(result$par[3]), 
                   log = FALSE)
    
  } else if(weightfunc == "U"){
    
    weight  <- rep(0, times = duration)
    weight[par[1]:par[2]] <- 1
    
  }
  
  weight[is.na(weight)] <- 0
  if (sum(weight) == 0){
    weight <- weight + 1
  }
  
  weight                <- weight / sum(weight) 
  modeldat$climate      <- apply(cmatrix, 1, FUN = function(x) {sum(x * weight)})
  LocalModel            <- update(modeloutput, .~., data = modeldat)
  
  if(any(colnames(model.frame(baseline)) %in% "climate")){
    
    coefs <- coef(summary(LocalModel))[, 1:2]
    
    temp.df <- data.frame("Y", t(coefs[-1, 1]), t(coefs[-1, 2]))
    
    colnames(temp.df) <- c("Custom.mod", colnames(model.frame(LocalModel)[-1]), paste(colnames(model.frame(LocalModel)[-1]), "SE", sep = ""))
    
  } else {
    
    if (class(LocalModel)[length(class(LocalModel))]=="coxph") {
      if (func == "quad"){
        WeightedOutput$ModelBeta  <- coef(LocalModel)[length(coef(LocalModel))-1]
        WeightedOutput$Std.Error  <- coef(summary(LocalModel))[, "se(coef)"][length(coef(LocalModel))-1]
        WeightedOutput$ModelBetaQ <- coef(LocalModel)[length(coef(LocalModel))]
        WeightedOutput$Std.ErrorQ <- coef(summary(LocalModel))[, "se(coef)"][length(coef(LocalModel))]
        WeightedOutput$ModelBetaC <- NA
        WeightedOutput$ModelInt   <- 0
      } else if (func == "cub"){
        WeightedOutput$ModelBeta  <- coef(LocalModel)[length(coef(LocalModel))-2]
        WeightedOutput$Std.Error  <- coef(summary(LocalModel))[, "se(coef)"][length(coef(LocalModel))-2]
        WeightedOutput$ModelBetaQ <- coef(LocalModel)[length(coef(LocalModel))-1]
        WeightedOutput$Std.ErrorQ <- coef(summary(LocalModel))[, "se(coef)"][length(coef(LocalModel))-1]
        WeightedOutput$ModelBetaC <- coef(LocalModel)[length(coef(LocalModel))]
        WeightedOutput$Std.ErrorC <- coef(summary(LocalModel))[, "se(coef)"][length(coef(LocalModel))]
        WeightedOutput$ModelInt   <- 0
      } else {
        WeightedOutput$ModelBeta  <- coef(LocalModel)[length(coef(LocalModel))]
        WeightedOutput$Std.Error  <- coef(summary(LocalModel))[, "se(coef)"][length(coef(LocalModel))]
        WeightedOutput$ModelBetaQ <- NA
        WeightedOutput$ModelBetaC <- NA
        WeightedOutput$ModelInt   <- 0
      }
    } 
    else if (length(attr(class(LocalModel),"package")) > 0 && attr(class(LocalModel), "package") == "lme4"){            
      if (func == "quad"){
        WeightedOutput$ModelBeta  <- fixef(LocalModel)[length(fixef(LocalModel)) - 1]
        WeightedOutput$Std.Error  <- coef(summary(LocalModel))[, "Std. Error"][2]
        WeightedOutput$ModelBetaQ <- fixef(LocalModel)[length(fixef(LocalModel))]
        WeightedOutput$Std.ErrorQ <- coef(summary(LocalModel))[, "Std. Error"][3]
        WeightedOutput$ModelBetaC <- NA
        WeightedOutput$ModelInt   <- fixef(LocalModel)[1]
      } else if (func == "cub"){
        WeightedOutput$ModelBeta  <- fixef(LocalModel)[length(fixef(LocalModel)) - 2]
        WeightedOutput$Std.Error  <- coef(summary(LocalModel))[, "Std. Error"][2]
        WeightedOutput$ModelBetaQ <- fixef(LocalModel)[length(fixef(LocalModel)) - 1]
        WeightedOutput$Std.ErrorQ <- coef(summary(LocalModel))[, "Std. Error"][3]
        WeightedOutput$ModelBetaC <- fixef(LocalModel)[length(fixef(LocalModel))]
        WeightedOutput$Std.ErrorC <- coef(summary(LocalModel))[, "Std. Error"][3]
        WeightedOutput$ModelInt   <- fixef(LocalModel)[1]
      } else {
        WeightedOutput$ModelBeta  <- fixef(LocalModel)[length(fixef(LocalModel))]
        WeightedOutput$Std.Error  <- coef(summary(LocalModel))[, "Std. Error"][2]
        WeightedOutput$ModelBetaQ <- NA
        WeightedOutput$ModelBetaC <- NA
        WeightedOutput$ModelInt   <- fixef(LocalModel)[1]
      }
    } else {
      if (func == "quad"){
        WeightedOutput$ModelBeta  <- coef(LocalModel)[length(coef(LocalModel)) - 1]
        WeightedOutput$Std.Error  <- coef(summary(LocalModel))[, "Std. Error"][2]
        WeightedOutput$ModelBetaQ <- coef(LocalModel)[length(coef(LocalModel))]
        WeightedOutput$Std.ErrorQ <- coef(summary(LocalModel))[, "Std. Error"][3]
        WeightedOutput$ModelBetaC <- NA
        WeightedOutput$ModelInt   <- coef(LocalModel)[1]
      } else if (func == "cub"){
        WeightedOutput$ModelBeta  <- coef(LocalModel)[length(coef(LocalModel)) - 2]
        WeightedOutput$Std.Error  <- coef(summary(LocalModel))[, "Std. Error"][2]
        WeightedOutput$ModelBetaQ <- coef(LocalModel)[length(coef(LocalModel)) - 1]
        WeightedOutput$Std.ErrorQ <- coef(summary(LocalModel))[, "Std. Error"][3]
        WeightedOutput$ModelBetaC <- coef(LocalModel)[length(coef(LocalModel))]
        WeightedOutput$Std.ErrorC <- coef(summary(LocalModel))[, "Std. Error"][4]
        WeightedOutput$ModelInt   <- coef(LocalModel)[1]
      } else {
        WeightedOutput$ModelBeta  <- coef(LocalModel)[length(coef(LocalModel))]
        WeightedOutput$Std.Error  <- coef(summary(LocalModel))[, "Std. Error"][2]
        WeightedOutput$ModelBetaQ <- NA
        WeightedOutput$ModelBetaC <- NA
        WeightedOutput$ModelInt   <- coef(LocalModel)[1]
      }
    } 
  }
  
  if(nrandom == 0){
    Return.list <- list()
    Return.list$BestModel <- LocalModel
    Return.list$BestModelData <- model.frame(LocalModel)
    Return.list$WeightedOutput <- WeightedOutput
    Return.list$WeightedOutput$Randomised <- "no"
    
    if(any(colnames(model.frame(baseline)) %in% "climate")){
      
      Return.list$WeightedOutput <- merge(Return.list$WeightedOutput, temp.df)
      
    }
    
    Return.list$Weights <- weight
    return(Return.list)     
  } else {
    Return.list <- list()
    Return.list$BestModel <- LocalModel
    Return.list$BestModelData <- model.frame(LocalModel)
    Return.list$WeightedOutput <- WeightedOutput
    Return.list$WeightedOutput$Randomised <- "yes"
    
    if(any(colnames(model.frame(baseline)) %in% "climate")){
      
      Return.list$WeightedOutput <- merge(Return.list$WeightedOutput, temp.df)
      
    }
    
    Return.list$Weights <- weight
    return(Return.list)
  }
}


##################################################################################

#Function to convert dates into day/week/month number
convertdate <- function(bdate, cdate, xvar, xvar2 = NULL, cinterval, type, 
                        refday, cross = FALSE, cohort, spatial){
  
  
  if (cinterval != "day" && cinterval != "week" && cinterval != "month"){
    stop("cinterval should be either day, week or month")
  }
  
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
  } else if (min(cdate) > min(bdate)){
    stop(paste("Climate data does not cover all years of biological data. Earliest climate data is ", min(cdate), ". Earliest biological data is ", min(bdate), sep = ""))
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
        newclim3    <- cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ]
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        climspatial <- newclim3$spatial
      } else {
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar)
        newclim2    <- melt(newclim, id = "cintno")
        newclim3    <- cast(newclim2, cintno ~ variable, mean, na.rm = T)
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
        newclim3    <- cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ]
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        climspatial <- newclim3$spatial
      } else {
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar)
        newclim2   <- melt(newclim, id = "cintno")
        newclim3   <- cast(newclim2, cintno ~ variable, mean, na.rm = T)
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
          bintno <- refday[2] + 12 * (year(bdate) - min(year(cdate2)))
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
        newclim3    <- cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        xvar2       <- newclim3$xvar2
        climspatial <- newclim3$spatial
      } else {
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2)
        newclim2   <- melt(newclim, id = "cintno")
        newclim3   <- cast(newclim2, cintno ~ variable, mean, na.rm = T)
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
        newclim3    <- cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        xvar2       <- newclim3$xvar2
        climspatial <- newclim3$spatial
      } else {
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2)
        newclim2   <- melt(newclim, id = "cintno")
        newclim3   <- cast(newclim2, cintno ~ variable, mean, na.rm = T)
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

#Gradient function?
Uni_grad_U <- function(par = par, modeloutput = modeloutput, 
                     duration = duration, cmatrix = cmatrix, 
                     nullmodel = nullmodel, funcenv = funcenv){
  
  
  grad(function(u) modloglik_Uni(par = u, modeloutput = modeloutput, 
                                 duration = duration, cmatrix = cmatrix, 
                                 nullmodel = nullmodel, funcenv = funcenv), par)
  
}

Uni_grad_W <- function(par = par, modeloutput = modeloutput, 
                       duration = duration, cmatrix = cmatrix, 
                       nullmodel = nullmodel, funcenv = funcenv,
                       modeldat = modeldat){
  
  
  grad(function(u) modloglik_W(par = u, modeloutput = modeloutput, 
                                 duration = duration, cmatrix = cmatrix, 
                                 nullmodel = nullmodel, funcenv = funcenv,
                               modeldat = modeldat), par)
  
}

Uni_grad_G <- function(par = par, modeloutput = modeloutput, 
                       duration = duration, cmatrix = cmatrix, 
                       nullmodel = nullmodel, funcenv = funcenv){
  
  
  grad(function(u) modloglik_G(par = u, modeloutput = modeloutput, 
                                 duration = duration, cmatrix = cmatrix, 
                                 nullmodel = nullmodel, funcenv = funcenv), par)
  
}

# define a function that returns the AICc or -2loglikelihood of model using uniform weight function
modloglik_Uni <- function(par = par, modeloutput = modeloutput, 
                          duration = duration, cmatrix = cmatrix, 
                          nullmodel = nullmodel, funcenv = funcenv){
  
  if(par[2] > par[1]){
    
    deltaAICc <- 0
    return(deltaAICc)
    
  }
  
  j       <- seq(0, 1, length.out = duration)
  weight  <- rep(0, times = duration) # calculate weights based on a uniform function
  weight[(par[1]:par[2] + 1)] <- 1

  if (sum(weight) == 0){
    weight <- weight + 1
  }
  
  weight                              <- weight / sum(weight) 
  funcenv$modeldat$climate            <- apply(cmatrix, 1, FUN = function(x) {sum(x*weight)})    # calculate weighted mean from weather data
  modeloutput                         <- update(modeloutput, .~., data = funcenv$modeldat)   # rerun regression model using new weather index
  deltaAICc                           <- AICc(modeloutput) - nullmodel
  funcenv$DAICc[[funcenv$modno]]      <- deltaAICc
  funcenv$par_open[[funcenv$modno]]   <- par[1]
  funcenv$par_close[[funcenv$modno]]  <- par[2]
  funcenv$track_mean[[funcenv$modno]] <- mean(funcenv$modeldat$climate)
  
  # plot the weight function and corresponding weather index being evaluated
  par(mfrow = c(3, 2))
  plot((weight / sum(weight)), type = "l", ylab = "weight", xlab = "timestep (e.g. days)", main = "Output of current weighted window being tested")
  plot(as.numeric(funcenv$DAICc), type = "l", ylab = expression(paste(Delta, "AICc")), xlab = "convergence step")
  plot(as.numeric(funcenv$par_open), type = "l", ylab = "open parameter", xlab = "convergence step")
  plot(as.numeric(funcenv$track_mean), type = "l", ylab = "weighted mean of weather", xlab = "convergence step")
  plot(as.numeric(funcenv$par_close), type = "l", ylab = "close parameter", xlab = "convergence step")
  
  #####
  
  #if(funcenv$modno == 1){
    
    #Matrix_3d <- matrix(nrow = max(Data_3d$WindowOpen), ncol = max(Data_3d$WindowOpen), data = 0)
    #for(i in 1:nrow(Data_3d)){
      
    #  Matrix_3d[Data_3d$WindowOpen[i], Data_3d$WindowClose[i]] <- Data_3d$deltaAICc[i]
      
    #}
    
    #norm_palette <- colorRampPalette(c("blue", "yellow", "red"))
    
    #z <- -(Matrix_3d);
    #x <- (1:nrow(z));
    #y <- (1:nrow(z));
    #zlim <- range(z);
    #zlen <- zlim[2] - zlim[1]+1;
    #colourlut <- norm_palette(zlen);
    #col <- colourlut[z-zlim[1]+1];
    #open3d();
    #rgl.surface(x, y, z, color = col, alpha = 1, back = "lines");
    #rgl.surface(x, y, matrix(1, nrow(z), ncol(z)), color = "grey", alpha = 0.5, back = "fill");
    #points3d(x = par[1], y = -(deltaAICc - 2), z = par[2], col = "red", size = 10, alpha = 1);
    
  #} else {
    
    #points3d(x = par[1], y = -(deltaAICc - 2), z = par[2], col = "black", size = 5, alpha = 1)
    
  #}
  
  ####
  
  funcenv$modno <- funcenv$modno + 1
  return(deltaAICc)  # returns deltaAICc as optim() minimizes! 
}


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
  plot(funcenv$modeldat$climate[1:(3 * duration)], type = "s", ylab = "weighted mean of weather", xlab = "timestep (e.g. days)")
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

weibull3 <- function(x, shape,scale,location){
  shape / scale * ((x - location) / scale) ^ (shape - 1) * exp( - ((x - location) / scale) ^ shape)
}

##################################################################################

gaussian <- function(x, scale, location){
  
  pnorm(q = x, mean = location, sd = scale)
  
}

#################################################################################

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