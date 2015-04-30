# make sure you run all the function in otherfunctions.r from climwin package, they do not seem to be automatically recognized

cvwin <- function(Xvar, CDate, BDate, baseline, furthest, closest, 
                       FIXED, cutoff.day, cutoff.month, STAT = "mean", FUNC = "L",
                       CMISSING = FALSE, CINTERVAL = "D",  nrandom = 0, CVK=0){
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
  modeldat$Yvar      <- modeldat[, 1]
  modeldat$temporary <- matrix(ncol = 1, nrow = nrow(CMatrix), seq(from = 1, to = nrow(CMatrix), by = 1))
  
  if(is.null(weights(baseline)) == FALSE){
    modeldat$modweights <- weights(baseline)
    baseline <- update(baseline, .~., weights = modweights, data = modeldat)
  }
  
  if (CVK>1){
    modeldat$K <- sample(seq(from = 1, to = length(modeldat$temporary), by = 1) %% CVK + 1)
    }   # create labels K-fold crossvalidation
  
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
          #modeloutput <- update(modeloutput, .~., weights = modweights)
          #baseline    <- update(baseline, .~., weights = modweights)
        
          # perform k-fold crossvalidation
          if (CVK>1) {      
            for (k in 1:CVK) {
              test           <- subset(modeldat, modeldat$K == k)
              train          <- subset(modeldat, modeldat$K != k)
              modeloutputcv <- update(modeloutput, Yvar~., data = train)  # note that we now use Yvar
              baselinecv    <- update(baseline, Yvar~., data = train)
              test$predictions<-predict(modeloutputcv, newdata=test)
              test$predictionsbaseline<-predict(baselinecv,newdata=test)
              N<-length(test$predictions) 
              P<-length(test$predictions)-modeloutputcv$df[1]  
              P_baseline<-length(test$predictions)-baselinecv$df[1]  
              mse<-sum((test$predictions-test[,1])^2)/N  #calc mse only works non-categorical Yvars, e.g. normal, binary, count data 
              mse_baseline<-sum((test$predictionsbaseline-test[,1])^2)/N
              AICc_cv<-N*log(mse)+(2*P*(P+1))/(N-P-1)
              AICc_cv_baseline<-N*log(mse_baseline)+(2*P_baseline*(P_baseline+1))/(N-P_baseline-1)  
              deltaAICc_cv<-AICc_cv-AICc_cv_baseline
              #rmse_corrected<-sqrt(sum((test$predictions-test[,1])^2)/modeloutputcv$df[1])
              ifelse(k==1,deltaAICc_cvtotal<-deltaAICc_cv,  deltaAICc_cvtotal<-deltaAICc_cvtotal+deltaAICc_cv)              
            }
            deltaAICcCV<-deltaAICc_cvtotal/CVK
          }

          #Add model parameters to list
          if (CVK>1) { MODLIST$deltaAICcCV[[MODNO]]<-deltaAICcCV }
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
    ifelse (windowopen - windowclose == 0, modeldat$temporary <- CMatrix[, windowclose:windowopen], modeldat$temporary <- apply(CMatrix[, windowclose:windowopen], 1, FUN = STAT))
  }
  LocalModel           <- update(modeloutput, .~.)
  MODLIST$baselineAICc <- AICc(baseline)
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
    return(list(BestModel = LocalModel, BestModelData = LocalData, WindowOutput = LocalOutput))
  } else {
    return(LocalOutputRand)
  }
}

#######################

#data(Mass)
#data(MassClimate)
#Mass$weight1<-abs(rnorm(length(Mass$Mass),0,1))
#Mass$cov<-abs(rnorm(length(Mass$Mass),0,1))
#Mass<-Mass[1:10,]
#MassWin <- cvwin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
#                        baseline = lm(Mass$Mass ~ 1, weights=Mass$weight1),
#                        furthest = 150, closest = 0,
#                        STAT = "mean", FUNC = "L",
#                        FIXED = TRUE, cutoff.day = 20, cutoff.month = 5,
#                        nrandom = 0, CMISSING = FALSE, CINTERVAL = "D", CVK=2)
#give Error in model.frame.default(formula = Yvar ~ temporary, data = train,  : variable lengths differ (found for '(weights)') 



#MassWin <- cvwin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
#                 baseline = lm(Mass ~ 1+cov, weights=weight1, data=Mass),
#                 furthest = 150, closest = 0,
#                 STAT = "mean", FUNC = "L",
#                 FIXED = TRUE, cutoff.day = 20, cutoff.month = 5,
#                 nrandom = 0, CMISSING = FALSE, CINTERVAL = "D", CVK=2)
# gives  Error in eval(expr, envir, enclos) : object 'weight1' not found 


#MassWin <- cvwin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
#                 baseline = lm(Mass$Mass ~ 1+Mass$cov, weights=Mass$weight1),
#                 furthest = 150, closest = 0,
#                 STAT = "mean", FUNC = "L",
#                 FIXED = TRUE, cutoff.day = 20, cutoff.month = 5,
#                 nrandom = 0, CMISSING = FALSE, CINTERVAL = "D", CVK=2)
#give Error in model.frame.default(formula = Yvar ~ Mass$cov + temporary, data = train,  : variable lengths differ (found for 'Mass$cov') 

                 
#summary(MassWin$WindowOutput$deltaAICcCV)
#plotall(Dataset=MassWin$WindowOutput, BestModel=MassWin$BestModel, BestModelData=MassWin$BestModelData)
#x11()
#plotcv(Dataset=MassWin$WindowOutput)
#write.table(MassWin$WindowOutput, file="outputMass.txt")
