#Function to convert dates into day/week/month number
DateConverter <- function(BDate, CDate, Xvar, Xvar2 = NULL, CINTERVAL, FIXED, 
                          cutoff.day, cutoff.month, cross = FALSE){
  
  BDate  <- as.Date(BDate, format = "%d/%m/%Y") # Convert the date variables into the R date format
  CDate2 <- seq(min(as.Date(CDate, format = "%d/%m/%Y")), max(as.Date(CDate, format = "%d/%m/%Y")), "days") # Convert the date variables into the R date format
  CDate  <- as.Date(CDate, format = "%d/%m/%Y")
  
  Xvar       <- Xvar[match(CDate2, CDate)]
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
      CIntNo     <- ceiling((as.numeric(CDate) - min(as.numeric(CDate)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to CIntNo 1
      RealBIntNo <- ceiling((as.numeric(BDate) - min(as.numeric(CDate)) + 1) / 7)
      NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar)
      NewClim2   <- melt(NewClim, id = "CIntNo")
      NewClim3   <- cast(NewClim2, CIntNo~variable, mean)
      CIntNo     <- NewClim3$CIntNo
      Xvar       <- NewClim3$Xvar
      if (FIXED == TRUE){ 
        BIntNo            <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(BDate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1) / 7) 
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(BDate[wrongyear]) + 1), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1) / 7)
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
      NewClim3   <- cast(NewClim2, CIntNo~variable, mean)
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
  } else {
    if (CINTERVAL == "D"){  
      if (FIXED == TRUE){   
        BIntNo            <- as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(BDate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1 
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- (as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(BDate[wrongyear]) + 1), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1)
      } else {
        BIntNo <- RealBIntNo
      }    
    } else if (CINTERVAL == "W"){
      CIntNo     <- ceiling((as.numeric(CDate) - min(as.numeric(CDate)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to CIntNo 1
      RealBIntNo <- ceiling((as.numeric(BDate) - min(as.numeric(CDate)) + 1) / 7)
      NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar, "Xvar2" = Xvar2)
      NewClim2   <- melt(NewClim, id = "CIntNo")
      NewClim3   <- cast(NewClim2, CIntNo~variable, mean)
      CIntNo     <- NewClim3$CIntNo
      Xvar       <- NewClim3$Xvar
      Xvar2      <- NewClim3$Xvar2
      if (FIXED == TRUE){ 
        BIntNo            <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, year(BDate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1) / 7) 
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- ceiling((as.numeric(as.Date(paste(cutoff.day, cutoff.month, (year(BDate[wrongyear]) + 1), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(CDate)) + 1) / 7)
      } else {
        BIntNo <- RealBIntNo
      }
    } else if (CINTERVAL == "M"){ 
      Cmonth     <- month(CDate)
      Cyear      <- year(CDate) - min(year(CDate))
      CIntNo     <- Cmonth + 12 * Cyear
      RealBIntNo <- month(BDate) + 12 * (year(BDate) - min(year(CDate)))
      NewClim    <- data.frame("CIntNo" = CIntNo, "Xvar" = Xvar, "Xvar2" = Xvar2)
      NewClim2   <- melt(NewClim, id = "CIntNo")
      NewClim3   <- cast(NewClim2, CIntNo ~ variable, mean)
      CIntNo     <- NewClim3$CIntNo
      Xvar       <- NewClim3$Xvar
      Xvar2      <- NewClim3$Xvar2
      if (FIXED == TRUE){ 
        BIntNo            <- cutoff.month + 12 * (year(BDate) - min(year(CDate)))
        wrongyear         <- which(BIntNo < RealBIntNo)
        BIntNo[wrongyear] <- cutoff.month + 12 * (year(BDate[wrongyear]) + 1 - min(year(CDate)))
      } else {
        BIntNo <- RealBIntNo
      }
    }
  }
  return(list(CIntNo = CIntNo, BIntNo = BIntNo, Xvar = Xvar, Xvar2 = Xvar2))
}


#Set progress bar
SetProgressBar <- function(furthest, closest, STAT) {
  MaxMODNO <- 0
  duration <- (furthest - closest) + 1
  for (m in closest:furthest){
    for (n in 1:duration){
      if ( (m-n) >= (closest - 1)){  
        if (STAT != "slope" || n > 1){
          MaxMODNO <- MaxMODNO + 1
        }
      }
    }
  }
  pb <- txtProgressBar(min = 0, max = MaxMODNO, style = 3, char = "|") # create progress bar object#
  return(pb)
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
  funcenv$modeldat$temporary            <- apply(CMatrix, 1, FUN = function(x) {sum(x*weight)})    # calculate weighted mean from weather data
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
  plot(funcenv$modeldat$temporary[1:(3 * duration)], type = "s", ylab = "weighted mean of climate", xlab = "timestep (e.g. days)")
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
  funcenv$modeldat$temporary            <- apply(CMatrix, 1, FUN = function(x) {sum(x*weight)})    # calculate weighted mean from weather data
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
  plot(funcenv$modeldat$temporary[1:(duration)], type = "s", ylab = "weighted mean of weather", xlab = "time step (e.g days)")
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