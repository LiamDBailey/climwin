#'Find a weighted climate window
#'
#'Finds the best weighted average of a weather variable over a period that 
#'correlates most strongly with a biological variable. Uses weibull or 
#'Generalised Extreme Value (GEV) distribution. See references for a full 
#'description.
#'
#'@param Xvar The climate variable of interest. Please specify the parent 
#'  environment and variable name (e.g. Climate$Temp).
#'@param CDate The climate date variable. Please specify the parent environment 
#'  and variable name (e.g. Climate$Date).
#'@param BDate The biological date variable. Please specify the parent 
#'  environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for testing correlation. 
#'  Currently known to support lm, lme, glm objects.
#'@param furthest The furthest number of days back that you want to search for a
#'  climate window.
#'@param closest The closest number of days back that you want any climate 
#'  windows to reach.
#'@param FUNC The function used to fit the climate variable in the model. Can be
#'  linear ("L"), quadratic ("Q"), cubic ("C"), inverse ("I") or log ("LOG").
#'@param FIXED TRUE or FALSE, whether you wish the climate window to be variable
#'  (i.e. the number of days before each biological record is measured) or fixed
#'  (i.e. number of days before a set point in time).
#'@param cutoff.day,cutoff.month If FIXED is TRUE, the day and month of the year
#'  from which the fixed window analysis will start.
#'@param WeightFunction The distribution to be used for optimisation. Can be 
#'  either a Weibull ("W") or Generalised Extreme Value distribution ("G").
#'@param CINTERVAL The resolution at which the climate window analysis will be 
#'  conducted. May be days ("D"), weeks ("W"), or months ("M"). Note the units 
#'  of parameters 'furthest' and 'closest' will differ depending on the choice 
#'  of CINTERVAL.
#'@param par Shape, scale and location parameters of the Weibull of GEV weight 
#'  function used as start weight function. For Weibull : Shape and scale 
#'  parameters must be greater than 0, while location parameter must be less 
#'  than or equal to 0. For GEV : Scale parameter must be greater than 0.
#'@param control Parameters used to determine step size for the optimisation 
#'  function. Please see \code{\link{optim}} for more detail.
#'@param method The method used for the optimisation function. Please see 
#'  \code{\link{optim}} for more detail.
#'@references van de Pol & Cockburn 2011 Am Nat 177(5):698-707 (doi: 
#'  10.1086/659101) "Identifying the critical climatic time window that affects 
#'  trait expression"
#'@return Produces a constantly updating grid of plots as the optimisation 
#'  function is running. \itemize{ \item Right panel from top to bottom: The
#'  three parameters (shape, scale and location) determining the weight 
#'  function.
#'  
#'  \item Left top panel: The resulting weight function.
#'  
#'  \item Right middle panel: The delta AICc compared to the baseline model.
#'  
#'  \item Right bottom panel: The weighted mean of climate for the current
#'  weight function. } Also returns a list containing three objects: \itemize{ 
#'  \item BestModel, a model object. The best weighted window model deterimend
#'  by AICc.
#'  
#'  \item BestModelData, a dataframe. Biological and climate data used to fit
#'  the best weighted window model.
#'  
#'  \item WeightedOutput, a list. Parameter values for the best weighted window.
#'  }
#'@author Martijn van de Pol and Liam D. Bailey
#'  @examples
#'  \dontrun{
#'  
#'  # Test for a weighted average over a fixed climate window 
#'  # using datasets 'Offspring' and 'OffspringClimate'
#'  
#'  # N.B. THIS EXAMPLE MAY TAKE A MOMENT TO CONVERGE ON THE BEST MODEL.
#'  
#'  # Load data
#'  
#'  data(Offspring)
#'  data(OffspringClimate)
#'  
#'  # Test for climate windows between 365 and 0 days ago (furthest=365, closest=0)
#'  # Fit a quadratic term for the mean weighted climate (FUNC="Q")
#'  # in a Poisson regression (offspring number ranges 0-3)
#'  # Test a variable window (FIXED=FALSE)
#'  # Test at the resolution of days (CINTERVAL="D")
#'  # Uses a Weibull weight function (WeightFunction="W")
#'  
#'  weight <- weightwin(Xvar = OffspringClimate$Temperature, CDate = OffspringClimate$Date, 
#'                      BDate = Offspring$Date, 
#'                      baseline = glm(Offspring ~ 1, family = poisson, data = Offspring), 
#'                      furthest = 365, closest = 0, FUNC = "Q", 
#'                      FIXED = FALSE, WeightFunction = "W", CINTERVAL = "D", 
#'                      par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)), 
#'                      method = "L-BFGS-B") 
#'  
#'  # View output
#'  
#'  head(weight[[3]])
#'  summary(weight[[1]])
#'  head(weight[[2]])
#'  }
#'
#'@importFrom evd dgev
#'@export

#LAST EDITED: 19/02/2015
#EDITED BY: LIAM
#NOTES: Removed parscale as it was unused in the code

# wishlist weightedplot
# 0.optim function still gives error with optimx
# 1.add all new stuff from ClimateWindow
# 2.add plotting function
# 5. GEV method returns "invalid shape" error

weightwin <- function(Xvar, CDate, BDate, baseline, furthest, closest, 
                      FUNC = "L", FIXED = FALSE, cutoff.day, cutoff.month, 
                      WeightFunction = "W", CINTERVAL = "D",
                      par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)), 
                      method = "L-BFGS-B"){
  
  funcenv                 <- environment()
  cont                    <- DateConverter(BDate = BDate, CDate = CDate, Xvar = Xvar, 
                                           CINTERVAL = CINTERVAL, FIXED = FIXED, 
                                           cutoff.day = cutoff.day, cutoff.month = cutoff.month )   # create new climate dataframe with continuous daynumbers, leap days are not a problem 
  duration                <- (furthest - closest) + 1
  CMatrix                 <- matrix(ncol = (duration), nrow = length(BDate))
  baseline                <- update(baseline, .~.)
  nullmodel               <- AICc(baseline)
  MODNO        <- 1
  DAICc        <- list()
  par_shape    <- list()
  par_scale    <- list()
  par_location <- list()
  
  for (i in 1:length(BDate)){
    for (j in closest:furthest){
      k <- j - closest + 1
      CMatrix[i, k] <- Xvar[match(cont$BIntNo[i] - j, cont$CIntNo)]   #Create a matrix which contains the climate data from furthest to furthest from each biological record#    
    }
  }
  
  funcenv$modeldat           <- model.frame(baseline)
  funcenv$modeldat$temporary <- matrix(ncol = 1, nrow = nrow(CMatrix), seq(from = 1, to = nrow(CMatrix), by = 1))
  
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
  
  # now run one of two optimization functions
  if (WeightFunction == "W"){
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
    result <- optim(par = par, fn = ModelLogLikelihoodW, control = control, 
                    method = method, lower = c(0.0001, 0.0001, -Inf), 
                    upper = c(Inf, Inf, 0), duration = duration, 
                    modeloutput = modeloutput, funcenv = funcenv,  
                    CMatrix = CMatrix, nullmodel = nullmodel)  
    
  } else if (WeightFunction == "G"){
    if (par[2] <= 0){
      stop("GEV scale parameter should be >0")
    }
    j      <- seq(-10, 10, by = (2 * 10 / duration))
    result <- optim(par = par, fn = ModelLogLikelihoodG, control = control, 
                               method = method, lower = c(-Inf, 0.0001, -Inf), 
                               upper = c(Inf, Inf, Inf), duration = duration, 
                               modeloutput = modeloutput, funcenv = funcenv,
                               CMatrix = CMatrix, nullmodel = nullmodel)
  } else {
    stop("Please choose Method to equal either W or G")
  } 
  bestmodel                     <- which(as.numeric(funcenv$DAICc) == min(as.numeric(funcenv$DAICc)))[1] # sometimes there are several bestmodels with similar DAICc, in which case we just pick one as they are all very similar
  WeightedOutput                <- list()   # prepare output of best model
  WeightedOutput$DeltaAICc      <- funcenv$DAICc[bestmodel]
  WeightedOutput$par_shape      <- funcenv$par_shape[bestmodel]
  WeightedOutput$par_scale      <- funcenv$par_scale[bestmodel]
  WeightedOutput$par_loc        <- funcenv$par_location[bestmodel]
  WeightedOutput$Function       <- FUNC
  WeightedOutput$WeightFunction <- WeightFunction
  
  ifelse (WeightFunction == "W", weight <- weibull3(x = j[1:duration], 
                                                    shape = as.numeric(funcenv$par_shape[bestmodel]), 
                                                    scale = as.numeric(funcenv$par_scale[bestmodel]), 
                                                    location = as.numeric(funcenv$par_location[bestmodel])), 
          weight <- dgev(j[1:duration], loc = as.numeric(funcenv$par_location[bestmodel]), 
                         scale = as.numeric(funcenv$par_scale[bestmodel]), 
                         shape = as.numeric(funcenv$par_shape[bestmodel]), 
                         log = FALSE))
  
  weight[is.na(weight)] <- 0
  if (sum(weight) == 0){
    weight <- weight + 1
  }
  
  weight                <- weight / sum(weight) 
  modeldat$temporary    <- apply(CMatrix, 1, FUN = function(x) {sum(x * weight)})
  LocalModel            <- update(modeloutput, .~., data = modeldat)
  WeightedOutput$Weight <- weight

  return(list(BestModel = LocalModel, BestModelData = model.frame(LocalModel), WeightedOutput = WeightedOutput))  
}