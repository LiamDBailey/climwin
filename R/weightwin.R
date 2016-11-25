#'Find a weighted climate window
#'
#'Finds the best weighted average of a weather variable over a period that 
#'correlates most strongly with a biological variable. Uses weibull or 
#'Generalised Extreme Value (GEV) distribution. See references for a full 
#'description.
#'
#'@param xvar A list object containing all climate variables of interest. 
#'  Please specify the parent environment and variable name (e.g. Climate$Temp).
#'@param cdate The climate date variable. Please specify the parent environment 
#'  and variable name (e.g. Climate$Date).
#'@param bdate The biological date variable. Please specify the parent 
#'  environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for testing correlation. 
#'  Currently known to support lm, lme, glm and glmer objects.
#'@param range Two values signifying respectively the furthest and closest number 
#'  of time intervals (set by cinterval) back from the cutoff date or biological record to include 
#'  in the climate window search.
#'@param func The function used to fit the climate variable in the model. Can be
#'  linear ("lin"), quadratic ("quad"), cubic ("cub"), inverse ("inv") or log ("log").
#'@param type "absolute" or "relative", whether you wish the climate window to be relative
#'  (e.g. the number of days before each biological record is measured) or absolute
#'  (e.g. number of days before a set point in time).
#'@param refday If type is absolute, the day and month respectively of the 
#'  year from which the absolute window analysis will start.
#'@param cohort A varaible used to group biological records that occur in the same biological
#'  season but cover multiple years (e.g. southern hemisphere breeding season). Only required
#'  when type is "absolute". The cohort variable should be in the same dataset as the variable bdate. 
#'@param weightfunc The distribution to be used for optimisation. Can be 
#'  either a Weibull ("W") or Generalised Extreme Value distribution ("G").
#'@param cinterval The resolution at which the climate window analysis will be 
#'  conducted. May be days ("day"), weeks ("week"), or months ("month"). Note the units 
#'  of parameter 'range' will differ depending on the choice 
#'  of cinterval.
#'@param spatial A list item containing:
#'  1. A factor that defines which spatial group (i.e. population) each biological
#'  record is taken from. The length of this factor should correspond to the length 
#'  of the biological dataset.
#'  2. A factor that defines which spatial group (i.e. population) climate data
#'  corresponds to. This length of this factor should correspond to the length of
#'  the climate dataset.
#'@param par Shape, scale and location parameters of the Weibull or GEV weight 
#'  function used as start weight function. For Weibull : Shape and scale 
#'  parameters must be greater than 0, while location parameter must be less 
#'  than or equal to 0. For GEV : Scale parameter must be greater than 0.
#'@param control Parameters used to determine step size for the optimisation 
#'  function. Please see \code{\link{optim}} for more detail.
#'@param method The method used for the optimisation function. Please see 
#'  \code{\link{optim}} for more detail.
#'@param cutoff.day,cutoff.month Redundant parameters. Now replaced by refday.
#'@param furthest,closest Redundant parameters. Now replaced by range.
#'@param nrandom Used when conducting data randomisation, should not be
#'  changed manually.
#'@param centre A list item containing:
#'  1. The variable used for mean centring (e.g. Year, Site, Individual). 
#'  Please specify the parent environment and variable name (e.g. Biol$Year).
#'  2. Whether the model should include both within-group means and variance ("both"),
#'  only within-group means ("mean"), or only within-group variance ("var").
#'@param grad Run the optimisation procedure with a numerically derived gradient function.
#'  This can improve model convergence but will increase computational time. 
#'@references van de Pol & Cockburn 2011 Am Nat 177(5):698-707 (doi: 
#'  10.1086/659101) "Identifying the critical climatic time window that affects 
#'  trait expression"
#'@return Produces a constantly updating grid of plots as the optimisation 
#'  function is running. 
#'  \itemize{ 
#'  \item Right panel from top to bottom: The
#'  three parameters (shape, scale and location) determining the weight 
#'  function.
#'  
#'  \item Left top panel: The resulting weight function.
#'  
#'  \item Right middle panel: The delta AICc compared to the baseline model.
#'  
#'  \item Right bottom panel: The weighted mean of climate for the current
#'  weight function. }
#'  
#'  Also returns a list containing three objects: \itemize{ 
#'  \item BestModel, a model object. The best weighted window model determined
#'  by deltaAICc.
#'  
#'  \item BestModelData, a dataframe. Biological and climate data used to fit
#'  the best weighted window model.
#'  
#'  \item WeightedOutput. Parameter values for the best weighted window.
#'  }
#'@author Martijn van de Pol and Liam D. Bailey
#'@examples
#'  \dontrun{
#'  
#'# Test for a weighted average over a fixed climate window 
#'# using datasets 'Offspring' and 'OffspringClimate'
#'  
#'# N.B. THIS EXAMPLE MAY TAKE A MOMENT TO CONVERGE ON THE BEST MODEL.
#'  
#'# Load data
#'  
#'data(Offspring)
#'data(OffspringClimate)
#'  
#'# Test for climate windows between 365 and 0 days ago (range = c(365, 0))
#'# Fit a quadratic term for the mean weighted climate (func="quad")
#'# in a Poisson regression (offspring number ranges 0-3)
#'# Test a variable window (type = "absolute")
#'# Test at the resolution of days (cinterval="day")
#'# Uses a Weibull weight function (weightfunc="week")
#'  
#'weight <- weightwin(xvar = list(Temp = OffspringClimate$Temperature), 
#'                    cdate = OffspringClimate$Date, 
#'                    bdate = Offspring$Date, 
#'                    baseline = glm(Offspring ~ 1, family = poisson, data = Offspring), 
#'                    range = c(365, 0), func = "quad", 
#'                    type = "relative", weightfunc = "W", cinterval = "day", 
#'                    par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)), 
#'                    method = "L-BFGS-B") 
#'  
#'# View output
#'  
#'head(weight[[3]])
#'summary(weight[[1]])
#'head(weight[[2]])
#'  }
#'
#'@importFrom evd dgev
#'@import numDeriv
#'@export

weightwin <- function(xvar, cdate, bdate, baseline, range, 
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
    print(result)
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
    
    print(result)
  } else if (weightfunc == "U"){
    
    if(length(par) > 2){
      print("Uniform distribution only uses two parameters (start and end). All other parameter values are ignored.")
    }
    
    if(par[1] > range[1]){
      stop(paste("Uniform scale parameter 1 must be less than the possible window range (", range[1], ")"))
    }
    
    if(par[2] > range[1]){
      stop(paste("Uniform scale parameter 2 must be less than the possible window range (", range[1], ")"))
    }
    
    if(par[1] < par[2]){
      stop(paste("The end parameter must be larger than the start parameter"))
    }
    
    j <- seq(0, 1, length.out = duration)
    
    if(grad == TRUE){
      
      result <- optim(par = par, fn = modloglik_Uni, 
                      gr = Uni_grad,
                      control = control,
                      method = method, lower = c(0, 0), upper = c(range[1], range[1]), duration = duration,
                      modeloutput = modeloutput, funcenv = funcenv,
                      cmatrix = cmatrix, nullmodel = nullmodel) 
      
    } else {
      
      result <- optim(par = par, fn = modloglik_Uni, 
                      gr = Uni_grad,
                      control = control,
                      method = method, lower = c(0, 0), upper = c(range[1], range[1]), duration = duration,
                      modeloutput = modeloutput, funcenv = funcenv,
                      cmatrix = cmatrix, nullmodel = nullmodel)
      
    }
    
    print(result)
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