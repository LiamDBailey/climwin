#'Find a weighted climate window
#'
#'Finds the best weighted average of a weather variable over a period that 
#'correlates most strongly with a biological variable. Uses weibull or 
#'Generalised Extreme Value (GEV) distribution. See references for a full 
#'description.
#'
#'@param n The number of iterations used to run weightwin. If n > 1, iterations 
#'will use randomly generated starting parameters. These are stored in the 
#'output data frame `iterations`. 
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
#'@param cmissing Determines what should be done if there are 
#'  missing climate data. Three approaches are possible: 
#'   - FALSE; the function will not run if missing climate data is encountered.
#'   An object 'missing' will be returned containing the dates of missing climate.
#'   - "method1"; missing climate data will be replaced with the mean climate
#'   of the preceding and following 2 records.
#'   - "method2"; missing climate data will be replaced with the mean climate
#'   of all records on the same date.
#'   
#'   Note: Other methods are possible. Users should consider those methods most
#'   appropriate for their data and apply them manually before using climwin if
#'   required.
#'@param cohort A variable used to group biological records that occur in the same biological
#'  season but cover multiple years (e.g. southern hemisphere breeding season). Only required
#'  when type is "absolute". The cohort variable should be in the same dataset as the variable bdate. 
#'@param weightfunc The distribution to be used for optimisation. Can be 
#'  either a Weibull ("W") or Generalised Extreme Value distribution ("G").
#'@param cinterval The resolution at which the climate window analysis will be 
#'  conducted. May be days ("day"), weeks ("week"), or months ("month"). Note the units 
#'  of parameter 'range' will differ depending on the choice 
#'  of cinterval.
#'@param k The number of folds used for k-fold cross validation. By default
#'  this value is set to 0, so no cross validation occurs. Value should be a
#'  minimum of 2 for cross validation to occur.
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
#'  \item Left middle panel: The delta AICc compared to the baseline model.
#'  
#'  \item Left bottom panel: Plotted relationship between the weighted mean of climate 
#'  and the biological response variable.}
#'  
#'  Also returns a list containing three objects: \itemize{ 
#'  \item BestModel, a model object. The best weighted window model determined
#'  by deltaAICc.
#'  
#'  \item BestModelData, a dataframe. Biological and climate data used to fit
#'  the best weighted window model.
#'  
#'  \item WeightedOutput. Parameter values for the best weighted window.
#'  
#'  \item iterations. If n > 1, the starting parameters and deltaAICc values
#'  from each iteration of weightwin.
#'  }
#'@author Martijn van de Pol and Liam D. Bailey
#'@examples
#'
#'#Simple test example
#'#Create data from a subset of our test dataset
#'biol_data <- Mass[1:5, ]
#'data(MassClimate)
#'
#'
#'weight <- weightwin(xvar = list(Temp = MassClimate$Temp), 
#'                    cdate = MassClimate$Date, 
#'                    bdate = biol_data$Date, 
#'                    baseline = glm(Mass ~ 1, data = biol_data), 
#'                    range = c(100, 0), func = "lin", 
#'                    type = "relative", weightfunc = "W", cinterval = "day", 
#'                    par = c(2.26, 8.45, 0), control = list(ndeps = c(0.01, 0.01, 0.01)), 
#'                    method = "L-BFGS-B")
#'                    
#'
#'\dontrun{
#'
#'# Full working example
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

weightwin <- function(n = 1, xvar, cdate, bdate, baseline, range, k = 0,
                      func = "lin", type, refday, nrandom = 0, centre = NULL,
                      weightfunc = "W", cinterval = "day", cmissing = FALSE, cohort = NULL, spatial = NULL,
                      par = c(3, 0.2, 0), control = list(ndeps = c(0.001, 0.001, 0.001)), 
                      method = "L-BFGS-B", cutoff.day = NULL, cutoff.month = NULL,
                      furthest = NULL, closest = NULL, grad = FALSE){
  
  if(n == 1){
    
    single_weight <- suppressMessages(basewin_weight(n = n, xvar = xvar, cdate = cdate, bdate = bdate,
                                                     baseline = baseline, range = range, func = func,
                                                     type = type, refday = refday, nrandom = nrandom,
                                                     centre = centre, weightfunc = weightfunc, k = k,
                                                     cinterval = cinterval, cmissing = cmissing, cohort = cohort,
                                                     spatial = spatial, par = par, control = control,
                                                     method = method, cutoff.day = cutoff.day, cutoff.month = cutoff.month,
                                                     furthest = furthest, closest = closest, grad = grad))
    
    return(single_weight)
    
  } else {
    
    weight.list <- list()
    
    par.list    <- list()
    
    pb <- txtProgressBar(min = 0, max = n, style = 3, char = "|")
    
    for(i in 1:n){
      
      if(weightfunc == "W"){
        
        if(i == 1){
          
          par = par
          
          save_par <- data.frame(start_shape = par[1], start_scale = par[2], start_location = par[3])
          
        } else {
          
          par = c(runif(1, min = 0.1, max = 10), runif(1, min = 0.1, max = 10), runif(1, min = -10, max = 0))
          
          save_par <- data.frame(start_shape = par[1], start_scale = par[2], start_location = par[3])
          
        }
        
      } else if(weightfunc == "G"){
        
        if(i == 1){
          
          par = par
          
          save_par <- data.frame(start_shape = par[1], start_scale = par[2], start_location = par[3])
          
        } else {
          
          par = c(runif(1, min = -10, max = 10), runif(1, min = 0.1, max = 10), runif(1, min = -10, max = 10))
          
          save_par <- data.frame(start_shape = par[1], start_scale = par[2], start_location = par[3])
          
        }
        
      } else if(weightfunc == "U"){
        
        if(i == 1){
          
          par = par
          
          save_par <- data.frame(start_open = par[1], start_close = par[2])
          
        } else {
          
          open = runif(1, min = range[2], max = range[1])
          
          close = runif(1, min = range[2], max = open)
          
          par = c(open, close)
          
          save_par <- data.frame(start_open = par[1], start_close = par[2])
          
        }
        
      }
      
      weight.list[[i]] <- suppressMessages(basewin_weight(n = n, xvar = xvar, cdate = cdate, bdate = bdate, k = k,
                                                          baseline = baseline, range = range, func = func,
                                                          type = type, refday = refday, nrandom = nrandom,
                                                          centre = centre, weightfunc = weightfunc,
                                                          cinterval = cinterval, cmissing = cmissing, cohort = cohort,
                                                          spatial = spatial, par = par, control = control,
                                                          method = method, cutoff.day = cutoff.day, cutoff.month = cutoff.month,
                                                          furthest = furthest, closest = closest, grad = grad))
      
      weight.list[[i]]$WeightedOutput <- merge(save_par, weight.list[[i]]$WeightedOutput)
      
      par.list[[i]] <- merge(save_par, data.frame(deltaAICc = weight.list[[i]]$WeightedOutput$deltaAICc))
      
      if (interactive()){
        setTxtProgressBar(pb, i - 1)
      }
    }
    
    if (interactive()){
      setTxtProgressBar(pb, n)
    }
    
    print(do.call(rbind, par.list))
    
    weight.list$iterations <- do.call(rbind, par.list)
    
    return(weight.list)
    
  }
  
}

