#'Test for a climate windows in data.
#'
#'Finds the time period when a biological variable is most strongly affected 
#'by climate. Note that climate data and biological data should be loaded as 
#'two separate objects. Both objects should contain a date column to designate
#'when the data were recorded (dd/mm/yyyy).
#'
#'Note that slidingwin allows you to test multiple possible parameters with the
#'same code (e.g. func, stat, xvar). See examples for more detail.
#'@param exclude Two values (duration and distance) which allow users
#'  to exclude short-duration long-lag climate windows from analysis (e.g., 
#'  windows with a duration of 10 days which occur over a month ago).
#'  These windows are often considered to be biologically implausible.
#'@param xvar A list object containing all climate variables of interest. 
#'  Please specify the parent environment and variable name (e.g. climate$Temp).
#'@param cdate The climate date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. climate$Date).
#'@param bdate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for model testing. 
#'  Currently known to support lm, glm, lmer, glmer and coxph objects.
#'@param range Two values signifying respectively the furthest and closest number 
#'  of time intervals (set by cinterval) back from the refday or biological record to include 
#'  in the climate window search.
#'@param stat The aggregate statistics used to analyse the climate data. Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format 
#'  function(x) (...). See FUN in \code{\link{apply}} for more detail.
#'@param func The functions used to fit the climate variable. Can be linear 
#'  ("lin"), quadratic ("quad"), cubic ("cub"), inverse ("inv") or log ("log").
#'@param type "absolute" or "relative", whether you wish the climate window to be relative
#'  (e.g. the number of days before each biological record is measured) or absolute
#'  (e.g. number of days before a set point in time).
#'@param refday If type is "absolute", the day and month respectively of the 
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
#'
#'@param cinterval The resolution at which climate window analysis will be 
#'  conducted. May be days ("day"), weeks ("week"), or months ("month"). Note the units
#'  of parameter 'range' will differ depending on the choice
#'  of cinterval.
#'@param k The number of folds used for k-fold cross validation. By default
#'  this value is set to 0, so no cross validation occurs. Value should be a
#'  minimum of 2 for cross validation to occur.
#'@param upper Cut-off values used to determine growing degree days or positive 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, slidingwin will instead calculate an 
#'  optimal climate zone.
#'@param lower Cut-off values used to determine chill days or negative 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, slidingwin will instead calculate an 
#'  optimal climate zone.
#'@param binary TRUE or FALSE. Determines whether to use values of upper and
#'  lower to calculate binary climate data (binary = TRUE), or to use for
#'  growing degree days (binary = FALSE).
#'@param centre A list item containing:
#'  1. The variable used for mean centring (e.g. Year, Site, Individual). 
#'  Please specify the parent environment and variable name (e.g. Biol$Year).
#'  2. Whether the model should include both within-group means and variance ("both"),
#'  only within-group means ("mean"), or only within-group variance ("var").
#'@param cohort A variable used to group biological records that occur in the same biological
#'  season but cover multiple years (e.g. southern hemisphere breeding season). Only required
#'  when type is "absolute". The cohort variable should be in the same dataset as the variable bdate.
#'@param spatial A list item containing:
#'  1. A factor that defines which spatial group (i.e. population) each biological
#'  record is taken from. The length of this factor should correspond to the length 
#'  of the biological dataset.
#'  2. A factor that defines which spatial group (i.e. population) climate data
#'  corresponds to. This length of this factor should correspond to the length of
#'  the climate dataset.
#'@return Will return a list with an output for each tested set of climate
#'  window parameters. Each list item contains three objects:
#'  
#'  \itemize{
#'  \item BestModel, a model object. The strongest climate window model based on AICc. 
#'  \item BestModelData, a dataframe used to fit the strongest climate window model.
#'  \item Dataset, a dataframe with information on all fitted climate windows. 
#'  Ordered using deltaAICc, with most negative deltaAICc values first. 
#'  See \code{\link{MassOutput}} as an example.}
#'  
#'  In addition, the returned list includes an object 'combos', a summary of all
#'  tested sets of climate window parameters. 
#'@author Liam D. Bailey and Martijn van de Pol
#'@import stats
#'@import utils
#'@import graphics
#'@import lme4
#'@import MuMIn
#'@importFrom lubridate weeks
#'@importFrom MuMIn AICc
#'@importFrom Matrix Matrix
#'@importFrom RcppRoll roll_mean
#'@importFrom nlme lme
#'@importFrom nlme varIdent
#'@importFrom nlme varExp  
#'@importFrom nlme lme.formula
#'@export

old_slidingwin <- function(exclude = NA, xvar, cdate, bdate, baseline,
                       type, refday = NULL, stat = "mean", func = "lin", range,
                       cmissing = FALSE, cinterval = "day", k = 0,
                       upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"),
                       spatial = NULL, cohort = NULL) {

  ## Validate xvar input
  if (!is.list(xvar)) stop("xvar should be an object of type list")
  if (is.null(names(xvar))) {
    names(xvar) <- paste0("climate", seq_along(xvar))
  }

  ## Convert refday: c(day, month) -> "DD/MM/YYYY" (year is irrelevant — only
  ## day/month are extracted in process_data for absolute windows)
  if (!is.null(refday) && length(refday) >= 2 && !any(is.na(refday))) {
    refday_str <- sprintf("%02d/%02d/2000", as.integer(refday[1]), as.integer(refday[2]))
  } else {
    refday_str <- NULL
  }

  ## Validate cmissing argument
  if (!identical(cmissing, FALSE) && !cmissing %in% c("method1", "method2")) {
    stop("'cmissing' must be FALSE, 'method1', or 'method2'")
  }

  ## Convert range: c(furthest, closest) -> seq(closest, furthest)
  new_range <- seq(range[2], range[1])

  ## Build combinations across xvar × stat × func
  allcombos <- expand.grid(
    climate = names(xvar),
    stat    = stat,
    func    = func,
    stringsAsFactors = FALSE
  )

  combined <- vector("list", nrow(allcombos))

  for (combo in seq_len(nrow(allcombos))) {
    xvar_name <- allcombos$climate[combo]
    stat_val  <- allcombos$stat[combo]
    func_val  <- allcombos$func[combo]

    ## Build climate data frame from the cdate vector and xvar values
    climate_df <- data.frame(Date = as.character(cdate), stringsAsFactors = FALSE)
    climate_df[[xvar_name]] <- xvar[[xvar_name]]

    ## Handle missing values in xvar per cmissing
    if (anyNA(climate_df[[xvar_name]])) {
      if (identical(cmissing, FALSE)) {
        stop(sprintf(
          "Missing values found in '%s'. Set cmissing = 'method1' or 'method2' to impute.",
          xvar_name
        ))
      } else if (cmissing == "method1") {
        climate_df <- repair_climate(climate_df, cdate = "Date", xvar = xvar_name,
                                     method = function(x) imputeTS::na_ma(x, k = 2))
      } else {
        temp_dates <- as.Date(climate_df$Date, format = "%d/%m/%Y")
        full_dates <- seq.Date(min(temp_dates, na.rm = TRUE),
                               max(temp_dates, na.rm = TRUE), by = "day")
        dm_key <- format(full_dates, "%m-%d")
        method2_fn <- local({
          dm <- dm_key
          function(x) {
            for (i in which(is.na(x))) {
              same_dm <- which(dm == dm[i] & !is.na(x))
              if (length(same_dm) > 0L) x[i] <- mean(x[same_dm])
            }
            x
          }
        })
        climate_df <- repair_climate(climate_df, cdate = "Date", xvar = xvar_name,
                                     method = method2_fn)
      }
    }

    ## Build biological data frame from the baseline model frame + date column
    bio_df           <- model.frame(baseline)
    bio_df[["Date"]] <- as.character(bdate)

    ## Add cohort column if provided
    cohort_col <- NULL
    if (!is.null(cohort)) {
      bio_df[["cohort_var"]] <- cohort
      cohort_col <- "cohort_var"
    }

    ## Add spatial columns if provided
    spatial_col <- NULL
    if (!is.null(spatial)) {
      bio_df[["spatial_var"]]     <- spatial[[1]]
      climate_df[["spatial_var"]] <- spatial[[2]]
      spatial_col <- "spatial_var"
    }

    ## Pre-aggregate climate data for non-daily intervals
    active_xvar <- xvar_name
    if (cinterval != "day") {
      climate_df <- trans_clim_interval(climate_df,
                                        xvar      = xvar_name,
                                        cdate     = "Date",
                                        cinterval = cinterval)
    }

    ## Apply thresholds if specified (applied after any aggregation)
    if (!is.na(upper) || !is.na(lower)) {
      upper_val  <- if (!is.na(upper)) upper else NULL
      lower_val  <- if (!is.na(lower)) lower else NULL
      climate_df <- append_clim_threshold(climate_df,
                                          xvar   = xvar_name,
                                          upper  = upper_val,
                                          lower  = lower_val,
                                          binary = binary)
      active_xvar <- "threshold"
    }

    ## Convert stat string to a function
    if (stat_val == "slope") {
      fn <- function(x) coef(lm(x ~ seq_along(x)))[2]
    } else {
      fn <- match.fun(stat_val)
    }

    ## Build new model formula by adding the climate predictor based on func
    base_formula <- formula(baseline)
    if (func_val == "lin") {
      new_formula <- update.formula(base_formula, . ~ . + climate)
    } else if (func_val == "quad") {
      new_formula <- update.formula(base_formula, . ~ . + poly(climate, 2))
    } else if (func_val == "cub") {
      new_formula <- update.formula(base_formula, . ~ . + poly(climate, 3))
    } else if (func_val == "inv") {
      new_formula <- update.formula(base_formula, . ~ . + I(1 / climate))
    } else if (func_val == "log") {
      new_formula <- update.formula(base_formula, . ~ . + log(climate))
    } else {
      new_formula <- update.formula(base_formula, . ~ . + climate)
    }

    ## Build a model call that swaps in new_formula and bio_data (resolved at
    ## eval time inside run_slidingwin)
    model_call          <- baseline$call
    model_call$formula  <- new_formula
    model_call$data     <- quote(bio_data)

    ## Delegate to run_slidingwin
    result <- run_slidingwin(
      range            = new_range,
      climate_data     = climate_df,
      bio_data         = bio_df,
      basemodel        = model_call,
      cdate            = "Date",
      bdate            = "Date",
      xvar             = active_xvar,
      fn               = fn,
      type             = type,
      refday           = refday_str,
      spatial          = spatial_col,
      cohort           = cohort_col,
      cinterval        = cinterval,
      progress         = FALSE,
      .basemodelIsCall = TRUE
    )

    combined[[combo]] <- result
  }

  if (length(combined) == 1L) {
    return(combined[[1]])
  }
  return(combined)
}

#'Test for a climate windows in data.
#'
#'Finds the time period when a biological variable is most strongly affected 
#'by climate. Note that climate data and biological data should be loaded as 
#'two separate objects. Both objects should contain a date column to designate
#'when the data were recorded (dd/mm/yyyy).
#'
#'Note that slidingwin allows you to test multiple possible parameters with the
#'same code (e.g. func, stat, xvar). See examples for more detail.
#'@param exclude Two values (duration and distance) which allow users
#'  to exclude short-duration long-lag climate windows from analysis (e.g., 
#'  windows with a duration of 10 days which occur over a month ago).
#'  These windows are often considered to be biologically implausible.
#'@param xvar A list object containing all climate variables of interest. 
#'  Please specify the parent environment and variable name (e.g. climate$Temp).
#'@param cdate The climate date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. climate$Date).
#'@param bdate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for model testing. 
#'  Currently known to support lm, glm, lmer, glmer and coxph objects.
#'@param range Two values signifying respectively the furthest and closest number 
#'  of time intervals (set by cinterval) back from the refday or biological record to include 
#'  in the climate window search.
#'@param stat The aggregate statistics used to analyse the climate data. Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format 
#'  function(x) (...). See FUN in \code{\link{apply}} for more detail.
#'@param func The functions used to fit the climate variable. Can be linear 
#'  ("lin"), quadratic ("quad"), cubic ("cub"), inverse ("inv") or log ("log").
#'@param type "absolute" or "relative", whether you wish the climate window to be relative
#'  (e.g. the number of days before each biological record is measured) or absolute
#'  (e.g. number of days before a set point in time).
#'@param refday If type is "absolute", the day and month respectively of the 
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
#'
#'@param cinterval The resolution at which climate window analysis will be 
#'  conducted. May be days ("day"), weeks ("week"), or months ("month"). Note the units
#'  of parameter 'range' will differ depending on the choice
#'  of cinterval.
#'@param k The number of folds used for k-fold cross validation. By default
#'  this value is set to 0, so no cross validation occurs. Value should be a
#'  minimum of 2 for cross validation to occur.
#'@param upper Cut-off values used to determine growing degree days or positive 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, slidingwin will instead calculate an 
#'  optimal climate zone.
#'@param lower Cut-off values used to determine chill days or negative 
#'  climate thresholds (depending on parameter thresh). Note that when values
#'  of lower and upper are both provided, slidingwin will instead calculate an 
#'  optimal climate zone.
#'@param binary TRUE or FALSE. Determines whether to use values of upper and
#'  lower to calculate binary climate data (binary = TRUE), or to use for
#'  growing degree days (binary = FALSE).
#'@param centre A list item containing:
#'  1. The variable used for mean centring (e.g. Year, Site, Individual). 
#'  Please specify the parent environment and variable name (e.g. Biol$Year).
#'  2. Whether the model should include both within-group means and variance ("both"),
#'  only within-group means ("mean"), or only within-group variance ("var").
#'@param cohort A variable used to group biological records that occur in the same biological
#'  season but cover multiple years (e.g. southern hemisphere breeding season). Only required
#'  when type is "absolute". The cohort variable should be in the same dataset as the variable bdate.
#'@param spatial A list item containing:
#'  1. A factor that defines which spatial group (i.e. population) each biological
#'  record is taken from. The length of this factor should correspond to the length 
#'  of the biological dataset.
#'  2. A factor that defines which spatial group (i.e. population) climate data
#'  corresponds to. This length of this factor should correspond to the length of
#'  the climate dataset.
#'@return Will return a list with an output for each tested set of climate
#'  window parameters. Each list item contains three objects:
#'  
#'  \itemize{
#'  \item BestModel, a model object. The strongest climate window model based on AICc. 
#'  \item BestModelData, a dataframe used to fit the strongest climate window model.
#'  \item Dataset, a dataframe with information on all fitted climate windows. 
#'  Ordered using deltaAICc, with most negative deltaAICc values first. 
#'  See \code{\link{MassOutput}} as an example.}
#'  
#'  In addition, the returned list includes an object 'combos', a summary of all
#'  tested sets of climate window parameters. 
#'@author Liam D. Bailey and Martijn van de Pol
#'@export
slidingwin <- function(exclude = NA, xvar, cdate, bdate, baseline, 
                       type, refday, stat = "mean", func = "lin", range, 
                       cmissing = FALSE, cinterval = "day", k = 0,
                       upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"),
                       spatial = NULL, cohort = NULL){
  
  if (!require("reshape", quietly = TRUE)){
    stop("Install package 'reshape' to run old code.")
  }
  
  ### Implementing scientific notation can cause problems because years
  ### are converted to characters in scientific notation (e.g. 2000 = "2e+3")
  ### Check options and convert scipen TEMPORARILY if needed.
  if(getOption("scipen") < 0){
    
    current_option <- getOption("scipen")
    options(scipen = 0)
    
  }
  
  #### INITIAL CHECKS ####
  
  if(cmissing != FALSE && cmissing != "method1" && cmissing != "method2"){
    
    stop("cmissing must be FALSE, 'method1' or 'method2'.")
    
  }
  
  if(type != "absolute" && type != "relative"){
    
    stop("type must be either absolute or relative.")
    
  }
  
  if(is.null(cohort) == TRUE){
    cohort = lubridate::year(as.Date(bdate, format = "%d/%m/%Y"))
  }
  
  if(k > 0 && class(baseline)[length(class(baseline))]=="coxph"){
    stop("Sorry, cross-validation is not available yet for coxph models")
  }
  
  #If the baseline model is fitted with nlme and cross validation is requested, return an error.
  if(attr(baseline, "class")[1] == "lme" && k > 0){
    
    stop("Sorry, cross-validation is currently not functioning for nlme models. Consider using lme4 if possible.")
    
  }
  
  #If spatial information is not specified, check that there are no duplicate calendar dates.
  if(is.null(spatial) & length(unique(cdate)) < length(cdate)){
    
    stop("Your cdate variable has repeated date measures. Do you have climate data from multiple sites? If so, you should specify the parameter `spatial`.")
    
  }
  
  #Create a centre function that over-rides quadratics etc. when centre != NULL
  if(is.null(centre[[1]]) == FALSE){
    func = "centre"
  }
    
  #Make xvar a list where the name of list object is the climate variable (e.g. Rain, Temp)
  if (is.list(xvar) == FALSE){
    stop("xvar should be an object of type list")
  }

  if (is.null(names(xvar)) == TRUE){
    numbers <- seq(1, length(xvar), 1)
    for (xname in 1:length(xvar)){
      names(xvar)[xname] = paste("climate", numbers[xname])
    }
  }
  
  if (is.na(upper) == FALSE && is.na(lower) == FALSE){
    combos       <- expand.grid(list(upper = upper, lower = lower))
    combos       <- combos[which(combos$upper >= combos$lower), ]
    allcombos    <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, gg = c(1:nrow(combos)), binary = binary))
    allcombos    <- cbind(allcombos, combos[allcombos$gg, ], deparse.level = 2)
    binarylevel  <- "two"
    allcombos$gg <- NULL
  } else if (is.na(upper) == FALSE && is.na(lower) == TRUE){
    allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, upper = upper, lower = lower, binary = binary))
    binarylevel <- "upper"
  } else if (is.na(upper) == TRUE && is.na(lower) == FALSE){
    allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func, upper = upper, lower = lower, binary = binary))
    binarylevel <- "lower"
  } else if (is.na(upper) == TRUE && is.na(lower) == TRUE){
    allcombos   <- expand.grid(list(climate = names(xvar), type = type, stat = stat, func = func))
    binarylevel <- "none"
  }
  
  rownames(allcombos) <- seq(1, nrow(allcombos), 1)
  # message("All combinations to be tested...")
  # message(allcombos)
  
  combined <- list()
  for (combo in 1:nrow(allcombos)){
    runs <- basewin(exclude = exclude, xvar = xvar[[paste(allcombos[combo, 1])]], cdate = cdate, bdate = bdate, baseline = baseline,
                    range = range, type = paste(allcombos[combo, 2]), refday = refday, stat = paste(allcombos[combo, 3]), func = paste(allcombos[combo, 4]),
                    cmissing = cmissing, cinterval = cinterval, k = k, 
                    upper = ifelse(binarylevel == "two" || binarylevel == "upper", allcombos$upper[combo], NA),
                    lower = ifelse(binarylevel == "two" || binarylevel == "lower", allcombos$lower[combo], NA),
                    binary = paste(allcombos$binary[combo]), centre = centre, cohort = cohort,
                    spatial = spatial)
    
    combined[[combo]]            <- runs
    allcombos$DeltaAICc[combo]   <- round(runs$Dataset$deltaAICc[1], digits = 2)
    allcombos$WindowOpen[combo]  <- runs$Dataset$WindowOpen[1]
    allcombos$WindowClose[combo] <- runs$Dataset$WindowClose[1]
    
    if(any(grepl("climate", model.frame(baseline)))){
      
      if(length(which("lin" == levels(allcombos$func))) >0){
        allcombos$betaL[combo] <- round(runs$Dataset$ModelBeta[1], digits = 2)
      }
      if(allcombos$func[1] == "centre"){
        if(centre[[2]] == "both"){
          allcombos$WithinGrpMean <- round(runs$Dataset$WithinGrpMean[1], digits = 2)
          allcombos$WithinGrpDev  <- round(runs$Dataset$WithinGrpDev[1], digits = 2)
        }
        if(centre[[2]] == "dev"){
          allcombos$WithinGrpDev  <- round(runs$Dataset$WithinGrpDev[1], digits = 2)
        }
        if(centre[[2]] == "mean"){
          allcombos$WithinGrpMean <- round(runs$Dataset$WithinGrpMean[1], digits = 2)
        }
      }
      if(length(which("quad" == levels(allcombos$func))) > 0){
        allcombos$betaL[combo]   <- round(runs$Dataset$ModelBeta[1], digits = 2)
        allcombos$betaQ[combo]   <- round(runs$Dataset$ModelBetaQ[1], digits = 2)
      }
      if(length(which("cub" == levels(allcombos$func))) > 0){
        allcombos$betaL[combo]   <- round(runs$Dataset$ModelBeta[1], digits = 2)
        allcombos$betaQ[combo]   <- round(runs$Dataset$ModelBetaQ[1], digits = 2)
        allcombos$betaC[combo]   <- round(runs$Dataset$ModelBetaC[1], digits = 2)
      }
      if(length(which("inv" == levels(allcombos$func))) > 0){
        allcombos$betaInv[combo] <- round(runs$Dataset$ModelBeta[1], digits = 2)
      }
      if(length(which("log" == levels(allcombos$func))) > 0){
        allcombos$betaLog[combo] <- round(runs$Dataset$ModelBeta[1], digits = 2)
      } 
    }
  }
  allcombos <- cbind(response = colnames(model.frame(baseline))[1], allcombos)
  combined <- c(combined, combos = list(allcombos))
  
  #If we changed scipen at the start, switch it back to default
  if(exists("current_option")){
    
    options(scipen = current_option)
    
  }
  
  return(combined)
}

#############################################################################################################

# #climatewin is now redundant, will transfer straight to slidingwin with message
# climatewin <- function(exclude = NA, xvar, cdate, bdate, baseline, 
#                        type, refday, stat = "mean", func = "lin", range, 
#                        cmissing = FALSE, cinterval = "day", k = 0,
#                        upper = NA, lower = NA, binary = FALSE, centre = list(NULL, "both"),
#                        spatial = NULL, cutoff.day = NULL, cutoff.month = NULL, 
#                        furthest = NULL, closest = NULL,
#                        thresh = NULL, cvk = NULL, cohort = NULL){
#   
#   print("PLEASE NOTE: Function 'climatewin' is being made redundant. Please use 'slidingwin' as an alternative")
#   
#   slidingwin(exclude = exclude, xvar = xvar, cdate = cdate, bdate = bdate, baseline = baseline, 
#              type = type, refday = refday, stat = stat, func = func, range = range, 
#              cmissing = cmissing, cinterval = cinterval, k = k,
#              upper = upper, lower = lower, binary = binary, centre = centre,
#              spatial = spatial, cutoff.day = cutoff.day, cutoff.month = cutoff.month, 
#              furthest = furthest, closest = closest,
#              thresh = thresh, cvk = cvk, cohort = cohort)
#   
# }

###########################################################################################################

#Basewin function that is combined with manywin to test multiple climate window characteristics
basewin <- function(exclude, xvar, cdate, bdate, baseline, range, 
                    type, stat = "mean", func = "lin", refday,
                    cmissing = FALSE, cinterval = "day", nrandom = 0, k = 0,
                    spatial, upper = NA, lower = NA, binary = FALSE, scale = FALSE, centre = list(NULL, "both"),
                    cohort = NULL, randwin = FALSE, randwin_thresholdQ){
  
  message("Initialising, please wait...")
  
  options(warn = 0, nwarnings = 1)
  
  ##########################################################################
  
  #### INITIAL CHECKS ####
  
  #Check that climate date data is in the correct date format
  if(all(is.na(as.Date(cdate, format = "%d/%m/%Y")))){
    
    stop("cdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  #Check that biological date data is in the correct date format
  if(all(is.na(as.Date(bdate, format = "%d/%m/%Y")))){
    
    stop("bdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  #If the user wants to use slope with log or inverse...
  if (stat == "slope" && func == "log" || stat == "slope" && func == "inv"){
    
    #Return an error...
    stop("stat = slope cannot be used with func = log or inv as negative values may be present")
  }
  
  #If the user has centred the data
  if(!is.null(centre[[1]])){
    
    #But they haven't specified whether they want within and/or between group...
    if(centre[[2]] != "both" && centre[[2]] != "dev" && centre[[2]] != "mean"){
      
      #Return an error...
      stop("Please set centre to one of 'both', 'dev', or 'mean'. See help file for details.")
    }
  }
  
  ##########################################################################
  
  #### DEALING WITH THRESHOLDS ####
  
  #By default, don't ask a question about how you want to apply the thresholds.
  thresholdQ <- "N"
  
  #If you are not using randwin, then ask the question about how thresholds should be applied.
  if(randwin == FALSE){
    
    #If you have specified an upper or lower value for which a threshold should be applied and you are not working at a daily scale...
    if((!is.na(upper) || !is.na(lower)) && (cinterval == "week" || cinterval == "month")){
      
      #Determine whether the user wants to: 1. apply the threshold at the daily level BEFORE estimating monthly/weekly mean (i.e. daily data is binary but monthly/weekly is not)
      #                                     2. apply the threshold AFTER applying monthly/weekly mean (i.e. daily data is non-binary, monthly/weekly is)
      thresholdQ <- readline("You specified a climate threshold using upper and/or lower and are working at a weekly or monthly scale. 
                           Do you want to apply this threshold before calculating weekly/monthly means (i.e. calculate thresholds for each day)? Y/N")
      
      #Convert to upper case for logical conditions
      thresholdQ <- toupper(thresholdQ)
      
      #If they didn't specify a Y/N answer, ask again.
      if(thresholdQ != "Y" & thresholdQ != "N"){
        
        thresholdQ <- readline("Please specify yes (Y) or no (N)")
        
      }
      
    }
    
    #If they are using randwin, make a decision based on earlier specified argument.    
  } else {
    
    if((!is.na(upper) || !is.na(lower)) && (cinterval == "week" || cinterval == "month")){
      
      thresholdQ <- randwin_thresholdQ
      
    }
    
  }
  
  ##########################################################################
  
  #### SPATIAL REPLICATION ####
  
  #If spatial info has been provided...
  if(!is.null(spatial)){
    
    #Create a data frame with the biological data and its spatial information.  
    sample.size <- 0
    data <- data.frame(bdate = bdate, spatial = spatial[[1]], cohort = as.factor(cohort))
    
    #For each cohort (usually year, but may also be breeding period)...  
    for(i in unique(data$cohort)){
      
      #Take a subset of the data for that cohort...
      sub <- subset(data, cohort == i)
      #Relevel spatial...
      sub$spatial <- factor(sub$spatial)
      #Add 1 to sample size for every site in each cohort.
      sample.size <- sample.size + length(levels(sub$spatial))
      
    }
    
    #If spatial is not provided...
  } else if(is.null(spatial)) {
    
    #Sample size is just the length of cohorts (i.e. number of years)
    sample.size <- length(unique(cohort))
    
  }
  
  ##########################################################################
  
  #PROCESS DATA INTO CLIMWIN FORMAT
  
  #Duration of searching period is the difference between the two levels or range + 1 (e.g. also including day 0)
  duration  <- (range[1] - range[2]) + 1
  #Determine maximum number of potential windows to fit.
  maxmodno  <- (duration * (duration + 1))/2
  
  #If the user has provided exclude data (i.e. there are certain windows that will not be considered)...
  if (length(exclude) == 2){
    
    #Remove these excluded windows from the estimate of maximum number of windows.
    maxmodno  <- maxmodno - exclude[1] * (duration - exclude[2] - 1) + (exclude[1] - 1) * exclude[1] / 2
    
  }
  
  #If slope stat is used...
  if (stat == "slope") { 
    
    #Adjust number of models...
    ifelse(is.na(exclude[2]) == TRUE,  maxmodno  <- maxmodno - duration, maxmodno  <- maxmodno - exclude[2] - 1)
    
  }
  
  #Convert date information in to numbers and apply absolute window info (if appropriate)
  #This code creates a new climate dataframe with continuous daynumbers, leap days are not a problem
  cont      <- convertdate(bdate = bdate, cdate = cdate, xvar = xvar, 
                           cinterval = cinterval, type = type, 
                           refday = refday, cohort = cohort, spatial = spatial, 
                           binary = binary, upper = upper, lower = lower, thresholdQ = thresholdQ)   
  
  if(!is.null(spatial)){ #If spatial data is provided...
    
    for(i in unique(spatial[[1]])){ #For each site...
      
      SUB_clim <- subset(cont$cintno, spatial == i) # ...subset the date numbers from climate data...
      SUB_biol <- subset(cont$bintno, spatial == i) # ...subset the date numbers from biological data...
      
      #Check that you have enough data to go back the specified range at EACH SITE
      if ((min(SUB_biol$Date) - range[1]) < min(SUB_clim$Date)){
        
        stop(paste("At site ", i, " you do not have enough climate data to search ", range[1], " ", cinterval, "s back. Please adjust the value of range or add additional climate data.", sep = ""))
        
      }
      
      #Check that you have enough data to start in the specified range at EACH SITE
      if (max(SUB_biol$Date) - range[2] > max(SUB_clim$Date)){
        
        stop(paste("At site ", i, " you need more recent climate data. The most recent climate data is from ", max(SUB_clim$Date), " while the most recent biological data is from ", max(SUB_biol$Date), sep = ""))
        
      }
    }
    
  } else { #If spatial data is not provided.
    
    #Check that you have enough data to go back the specified range
    if ((min(cont$bintno) - range[1]) < min(cont$cintno)){
      stop(paste("You do not have enough climate data to search ", range[1], " ", cinterval, "s before ", min(as.Date(bdate, format = "%d/%m/%Y")), ". Please adjust the value of range or add additional climate data.", sep = ""))
    }
    
    #Check that you have enough data to start in the specified range
    if ((max(cont$bintno) - range[2] - 1) > max(cont$cintno)){
      stop(paste("You need more recent climate data to test over this range. The most recent climate data is from ", max(as.Date(cdate, format = "%d/%m/%Y")), " while the most recent biological data is from ", max(as.Date(bdate, format = "%d/%m/%Y")), sep = ""))
    }
    
  }
  
  modno     <- 1  #Create a model number variable that will count up during the loop#
  cmatrix   <- matrix(ncol = (duration), nrow = length(bdate))  # matrix that stores the weather data for variable or fixed windows
  
  modlist   <- list()   # dataframes to store ouput
  if(class(baseline)[1] == "lme"){
    
    baseline  <- update(baseline, .~.)
    
  } else {
    
    baseline  <- my_update(baseline, .~.) 
    
  }
  nullmodel <- MuMIn::AICc(baseline)
  modeldat  <- model.frame(baseline)
  
  #If there are any variables that have been scaled (i.e. using scale(x)) we need to change the model data to remove the scale argument.
  #If not, the model.frame output has a column scale(x) but the model during update is looking for x.
  if(any(grepl("scale\\(|\\)", colnames(modeldat)))){
    
    colnames(modeldat) <- gsub("scale\\(|\\)", "", colnames(modeldat))
    
  }
  
  if(attr(baseline, "class")[1] == "lme"){ #If model is fitted using nlme package
    
    if(is.null(baseline$modelStruct$varStruct) == FALSE && !is.null(attr(baseline$modelStruct$varStruct, "groups"))){ #If a custom variance structure has been included and it has multiple levels...
      
      modeldat <- cbind(modeldat, attr(baseline$modelStruct$varStruct, "groups")) #Add the variables from this variance structure to the model data used for updating models.
      
      colnames(modeldat)[ncol(modeldat)] <- strsplit(x = as.character(attr(baseline$modelStruct$varStruct, "formula"))[2], split = " | ")[[1]][3] #Make the names equivalent.
      
    }
    
    #If a complex variance structure hasn't been provided...
    
    non_rand <- ncol(modeldat) #Determine the number of non-random variables from the original model data.
    
    modeldat <- cbind(modeldat, baseline$data[, colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")]]) #Include random effects (i.e. those that AREN'T FIXED)
    
    colnames(modeldat)[-(1:non_rand)] <- colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")] #Make sure these columns are named correctly
    
  }
  
  #If using coxph models, adjust naming of variables to deal with frailty terms.
  if(class(baseline)[length(class(baseline))]=="coxph" && grepl("frailty\\(", colnames(modeldat)[ncol(modeldat)])){
    colnames(modeldat)[ncol(modeldat)] <- gsub("frailty\\(", "", colnames(modeldat)[ncol(modeldat)])
    colnames(modeldat)[ncol(modeldat)] <- gsub("\\)", "", colnames(modeldat)[ncol(modeldat)])
  }
  
  #Rename response variable as yvar (this way the name is standardised and can be easily called)
  colnames(modeldat)[1] <- "yvar"
  
  #If user has provided some variable for mean centring change the func to centre (i.e. you can't do mean centring and quadratic/cubic)
  if (is.null(centre[[1]]) == FALSE){
    func <- "centre"
  }
  
  #Determine length of data provided for response variable.
  ifelse(class(baseline)[length(class(baseline))]=="coxph", leng <- length(modeldat$yvar[,1]), leng <- length(modeldat$yvar))
  #If there are NAs present in the biological data, provide an error.
  if (leng != length(bdate)){
    stop("NA values present in biological response. Please remove NA values")
  }
  
  if(cinterval == "day" || (!is.na(thresholdQ) && thresholdQ == "N")){ #If dealing with daily data OR user chose to apply threshold later...
    
    if(is.null(spatial) == FALSE){ #...and spatial information is provided...
      
      if (is.na(upper) == FALSE && is.na(lower) == TRUE){ #...and an upper bound is provided...
        if (binary == TRUE){ #...and we want data to be binary (i.e. it's above the value or it's not)
          cont$xvar$Clim <- ifelse (cont$xvar$Clim > upper, 1, 0) #Then turn climate data into binary data.
        } else { #Otherwise, if binary is not true, simply make all data below the upper limit into 0.
          cont$xvar$Clim <- ifelse (cont$xvar$Clim > upper, cont$xvar$Clim, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == TRUE){ #If a lower limit has been provided, do the same.
        if (binary == TRUE){
          cont$xvar$Clim <- ifelse (cont$xvar$Clim < lower, 1, 0)
        } else {
          cont$xvar$Clim <- ifelse (cont$xvar$Clim < lower, cont$xvar$Clim, 0)
        }
      }
      
      if (is.na(lower) == FALSE && is.na(upper) == FALSE){ #If both an upper and lower limit are provided, do the same.
        if (binary == TRUE){
          cont$xvar$Clim <- ifelse (cont$xvar$Clim > lower && cont$xvar$Clim < upper, 1, 0)
        } else {
          cont$xvar$Clim <- ifelse (cont$xvar$Clim > lower && cont$xvar$Clim < upper, cont$xvar$Clim - lower, 0)
        } 
      }
      
    } else { #Do the same with non-spatial data (syntax is just a bit different, but method is the same.)
      
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
    
  }
  
  if(is.null(spatial) == FALSE){ #If spatial information is provided...
    for (i in 1:length(bdate)){ #For each biological record we have...
      #Take a row in the empty matrix and add climate data from the correct site and over the full date range chosen by the user.
      cmatrix[i, ] <- cont$xvar[which(cont$cintno$spatial %in% cont$bintno$spatial[i] & cont$cintno$Date %in% (cont$bintno$Date[i] - c(range[2]:range[1]))), 1]  
    }
  } else { #If no spatial data is provided, do the same but without checking site ID
    for (i in 1:length(bdate)){
      cmatrix[i, ] <- cont$xvar[which(cont$cintno %in% (cont$bintno[i] - c(range[2]:range[1])))]    
    } 
  }
  
  #Make sure the order is correct, so most recent climate data is in the earliest column
  cmatrix <- as.matrix(cmatrix[, c(ncol(cmatrix):1)])
  
  #return(list(cmatrix, cont))
  
  if(cmissing == FALSE & any(is.na(cmatrix))){ #If the user doesn't expect missing climate data BUT there are missing data present...
    if(is.null(spatial) == FALSE){ #And spatial data has been provided...
      
      if (cinterval == "day"){ #Where a daily interval is used...
        #...save an object 'missing' with the full dates of all missing data.
        .GlobalEnv$missing <- as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      
      if (cinterval == "month"){ #Where a monthly interval is used...
        #...save an object 'missing' with the month and year of all missing data.
        .GlobalEnv$missing <- c(paste("Month:", cont$cintno$Date[is.na(cont$xvar$Clim)] - (floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/12) * 12),
                                      #lubridate::month(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/12)))
        #lubridate::year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if (cinterval == "week"){ #Where weekly data is used...
        #...save an object 'missing' with the week and year of all missing data.
        .GlobalEnv$missing <- c(paste("Week:", cont$cintno$Date[is.na(cont$xvar$Clim)] - (floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/52) * 52),
                                      #lubridate::week(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/52)))
        #lubridate::year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
    } else { #If spatial data is not provided.
      
      if (cinterval == "day"){ #Do the same for day (syntax is just a bit differen)
        .GlobalEnv$missing <- as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if (cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", (lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1)) - (floor((lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1))/12)*12),
                                      "Year:", (floor((which(is.na(cont$xvar)) - 1)/12) + lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))))))
      }
      if (cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", cont$cintno[is.na(cont$xvar)] - (floor(cont$cintno[is.na(cont$xvar)]/52) * 52),
                                      #ceiling(((as.numeric((as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y"))) - (floor(which(is.na(cmatrix))/nrow(cmatrix))*7)) - as.numeric(as.Date(paste("01/01/", lubridate::year(as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) + 1) / 7),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cont$cintno[is.na(cont$xvar)]/52)))
        #lubridate::year(as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y"))))
      }
    }
    
    #Create an error to warn about missing data
    stop(c("Climate data should not contain NA values: ", length(.GlobalEnv$missing),
           " NA value(s) found. Please add missing climate data or set cmissing to `method1` or `method2`.
           See object 'missing' for all missing climate data"))
  }
  
  #If we expect NAs and choose a method to deal with them...
  if (cmissing != FALSE && any(is.na(cmatrix))){
    
    message("Missing climate data detected. Please wait while NAs are replaced.")
    
    for(i in which(is.na(cmatrix))){
      
      #Determine the column and row location...
      if(i %% nrow(cmatrix) == 0){
        
        col <- i/nrow(cmatrix)
        row <- nrow(cmatrix)
        
      } else {
        
        col <- i%/%nrow(cmatrix) + 1
        row <- i %% nrow(cmatrix)
        
      }
      
      
      #If we are using method1
      if(cmissing == "method1"){
        
        #If we are using a daily interval
        if(cinterval == "day"){
          
          #For the original cdate data extract date information.
          cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"))
          
          #Extract the original biological date
          bioldate <- as.Date(bdate[row], format = "%d/%m/%Y")
          
          #Determine from this on which date data is missing
          missingdate <- bioldate - (col + range[2] - 1)
          
          #If we have spatial replication
          if(is.null(spatial) == FALSE){
            
            cdate_new$spatial <- spatial[[2]]
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)) & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)))], na.rm = T)
            
          }
          
        } else if(cinterval == "week" || cinterval == "month"){
          
          if(is.null(spatial) == FALSE){
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno$Date,
                                    spatial = cont$cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno$Date[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(cont$xvar$Clim[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)) & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            cmatrix[row, col] <- mean(cont$xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)))], na.rm = T)
            
          }
          
        }
        
        #If the record is still an NA, there must be too many NAs. Give an error.
        if(is.na(cmatrix[row, col])){
          
          stop("Too many consecutive NAs present in the data. Consider using method2 or manually replacing NAs.")
          
        }
        
      } else if(cmissing == "method2"){
        
        if(cinterval == "day"){
          
          #For the original cdate data, determine the year, month, week and day.
          cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"),
                                  Month = lubridate::month(as.Date(cdate, format = "%d/%m/%Y")),
                                  Day   = lubridate::day(as.Date(cdate, format = "%d/%m/%Y")))
          
          #Extract the original biological date
          bioldate <- as.Date(bdate[row], format = "%d/%m/%Y")
          
          #Determine from this on which date data is missing
          missingdate <- bioldate - (col + range[2] - 1)
          
          missingdate <- data.frame(Date  = missingdate,
                                    Month = lubridate::month(missingdate),
                                    Day   = lubridate::day(missingdate))
          
          if(is.null(spatial) == FALSE){
            
            cdate_new$spatial <- spatial[[2]]
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% missingdate$Month & cdate_new$Day %in% missingdate$Day & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% missingdate$Month & cdate_new$Day %in% missingdate$Day)], na.rm = T)
            
          }
          
        } else if(cinterval == "week" || cinterval == "month"){
          
          if(is.null(spatial) == FALSE){
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno$Date,
                                    spatial = cont$cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno$Date[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            #Convert all dates back into year specific values
            if(cinterval == "week"){
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/52) * 52)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 52, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/52) * 52)
              missingdate <- ifelse(missingdate == 0, 52, missingdate)
              
            } else {
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/12) * 12)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 12, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/12) * 12)
              missingdate <- ifelse(missingdate == 0, 12, missingdate)
              
            }
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(cont$xvar$Clim[which(cdate_new$Date %in% missingdate & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            #Convert all dates back into year specific values
            if(cinterval == "week"){
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/52) * 52)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 52, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/52) * 52)
              missingdate <- ifelse(missingdate == 0, 52, missingdate)
              
            } else {
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/12) * 12)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 12, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/12) * 12)
              missingdate <- ifelse(missingdate == 0, 12, missingdate)
              
            }
            
            cmatrix[row, col] <- mean(cont$xvar[which(cdate_new$Date %in% missingdate)], na.rm = T)
            
          }
        }
        
        if(is.na(cmatrix[row, col])){
          
          stop("There is not enough data to replace missing values using method2. Consider dealing with NA values manually")
          
        }
        
      } else {
        
        stop("cmissing should be method1, method2 or FALSE")
        
      }
    }
  }
  
  #Check to see if the model contains a weight function. If so, incorporate this into the data used for updating the model.
  if("(weights)" %in% colnames(model.frame(baseline))){
    
    modeldat$model_weights  <- weights(baseline)
    #baseline <- update(baseline, yvar~., weights = model_weights, data = modeldat)
    
    call <- as.character(getCall(baseline))
    
    weight_name <- call[length(call)]
    
    names(modeldat)[length(names(modeldat))] <- weight_name
    
  }
  # if (is.null(weights(baseline)) == FALSE){
  #   if(class(baseline) == "lm"){
  #     
  #     if(!is.null(weights(baseline))){
  #       
  #       
  #       
  #     }
  #     
  #   }
  #   if (class(baseline)[1] == "glm" && sum(weights(baseline)) == nrow(model.frame(baseline)) || attr(class(baseline), "package") == "lme4" && sum(weights(baseline)) == nrow(model.frame(baseline))){
  #     
  #   } else {
  #     
  #     modeldat$model_weights  <- weights(baseline)
  #     #baseline <- update(baseline, yvar~., weights = model_weights, data = modeldat)
  #     
  #     call <- as.character(getCall(baseline))
  #     
  #     weight_name <- call[length(call)]
  #     
  #     names(modeldat)[length(names(modeldat))] <- weight_name
  #     
  #   }
  # }
  
  #If using a mixed model, ensure that maximum likelihood is specified (because we are comparing models with different fixed effects)
  if(!is.null(attr(class(baseline), "package")) && attr(class(baseline), "package") == "lme4" && class(baseline)[1] == "lmerMod" && baseline@resp$REML == 1){
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, REML = F)
    
  }
  
  if(attr(baseline, "class")[1] == "lme" && baseline$method == "REML"){
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, method = "ML")
    
  }
  
  #If there are no variables in the baseline model called climate (i.e. the user has not specified more complex role for climate in the model, such as an interaction or random effects.)
  if(all(!grepl("climate", colnames(modeldat)))){
    
    #Create a new dummy variable called climate, that is made up all of 1s (unless it's using lme, because this will cause errors).
    if(attr(baseline, "class")[1] == "lme"){
      
      modeldat$climate <- seq(1, nrow(modeldat), 1)
      
    } else {
      
      modeldat$climate <- 1
      
    }
    
    #Update the baseline model to include this new variable in the required format (e.g. linear, quadratic etc.)
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
      stop("Define func")
    }
    
  } else {
    
    #If climate has already been provided, simply update the model with the new term yvar.
    modeloutput <- update(baseline, yvar ~., data = modeldat)
    
    coef_data <- list()
    
  }
  
  #If cross validation has been specified...
  if (k >= 1){
    modeldat$K <- sample(seq(from = 1, to = length(modeldat$climate), by = 1) %% k + 1)
  }   # create labels k-fold crossvalidation
  
  #Create the progress bar
  pb <- txtProgressBar(min = 0, max = maxmodno, style = 3, char = "|")
  
  #CREATE A FOR LOOP TO FIT DIFFERENT CLIMATE WINDOWS#
  for (m in range[2]:range[1]){ #For every day in the given range...
    for (n in 1:duration){ #And for each possible window duration...
      if (length(exclude) == 2 && m >= exclude[2] && (m-n) >= exclude[2] && n <= exclude[1]){
        next #If an exclude term has been provided, skip those windows that are meant to be excluded.
      }
      if ( (m - n) >= (range[2] - 1)){  # do not use windows that overshoot the closest possible day in window
        if (stat != "slope" || n > 1){ #Don't use windows one day long with function slope...
          windowopen  <- m - range[2] + 1 #Determine the windowopen time (i.e. the point in the past where the window STARTS)
          windowclose <- windowopen - n + 1 #Determine the windowclose time (i.e. the more recent point where the window FINISHES)
          
          if (stat == "slope"){ #If we are using the slope function
            time             <- n:1 #Determine the number of days over which we will calculate slope.
            #Determine the slope (i.e. change in climate over time)
            modeldat$climate <- apply(cmatrix[, windowopen:windowclose], 1, FUN = function(x) coef(lm(x ~ time))[2])
          } else {
            #If slopes is not specified, apply the chosen aggregate statistic (e.g. mean, mass) to the window.
            ifelse (n == 1, modeldat$climate <- cmatrix[, windowopen:windowclose], 
                    modeldat$climate <- apply(cmatrix[, windowopen:windowclose], 1, FUN = stat))
          }
          
          #Stop climwin if there are values <=0 and func is log or inverse.
          if (min(modeldat$climate) <= 0 && func == "log" || min(modeldat$climate) <= 0 && func == "inv"){
            stop("func = log or inv cannot be used with climate values <= 0. 
                 Consider adding a constant to climate data to remove these values")
          }
          
          #If using models from nlme and there is an issue where climate has no variance (e.g. short windows where rainfall is all 0)
          if(attr(modeloutput, "class")[1] == "lme" && var(modeldat$climate) == 0){
            
            #skip the fitting of the climate data and just treat it has deltaAICc of 0
            #This is necessary as nlme doesn't have a way to deal with rank deficiency (unlike lme4) and will give an error
            modeloutput  <- baseline
            AICc_cv_avg  <- AICc(baseline)
            deltaAICc_cv <- AICc(baseline) - AICc(baseline)
            
          } else {
            
            #If mean centring is specified, carry this out on the data from the climate window.
            if (is.null(centre[[1]]) == FALSE){
              if(centre[[2]] == "both"){
                modeldat$wgdev  <- wgdev(modeldat$climate, centre[[1]])
                modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
                
                if(class(baseline)[1] == "coxph"){
                  
                  modeloutput <- my_update(modeloutput, .~., data = modeldat)
                  
                } else {
                  
                  modeloutput <- update(modeloutput, .~., data = modeldat)
                  
                }
                
              }
              if(centre[[2]] == "mean"){
                modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
                
                if(class(baseline)[1] == "coxph"){
                  
                  modeloutput <- my_update(modeloutput, .~., data = modeldat)
                  
                } else {
                  
                  modeloutput <- update(modeloutput, .~., data = modeldat)
                  
                }
                
              }
              if(centre[[2]] == "dev"){
                modeldat$wgdev  <- wgdev(modeldat$climate, centre[[1]])
                
                if(class(baseline)[1] == "coxph"){
                  
                  modeloutput <- my_update(modeloutput, .~., data = modeldat)
                  
                } else {
                  
                  modeloutput <- update(modeloutput, .~., data = modeldat)
                  
                }
                
              }
            } else {
              
              #Update models with this new climate data (syntax is a bit different for nlme v. other models)
              if(attr(modeloutput, "class")[1] == "lme"){
                
                modeloutput <- tryCatch({
                  
                  update(modeloutput, .~., data = modeldat); 
                  update(modeloutput, .~., data = modeldat)
                  
                }, error = function(e){
                  
                  update(baseline, yvar~., data = modeldat)
                  
                })
                
                if(all(!colnames(model.frame(modeloutput)) %in% "climate")){
                  
                  warning("A model from one climate windows failed to converge. This model was replaced with the null model")
                  
                }
                
              } else {
                
                if(class(baseline)[1] == "coxph"){
                  
                  modeloutput <- my_update(modeloutput, .~., data = modeldat)
                  
                } else {
                  
                  modeloutput <- update(modeloutput, .~., data = modeldat)
                  
                }
                
              }
            }
            
            # If valid, perform k-fold crossvalidation
            if (k >= 1) {      
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
            
          }
          
          #Add model parameters to list
          if (k > 1){
            
            modlist$ModelAICc[modno]    <- AICc_cv_avg
            modlist$deltaAICc[modno]    <- deltaAICc_cv
            
          } else {
            
            modlist$deltaAICc[modno] <- AICc(modeloutput) - AICc(baseline)
            modlist$ModelAICc[modno] <- AICc(modeloutput)
          }
          
          modlist$WindowOpen[modno]  <- m
          modlist$WindowClose[modno] <- m - n + 1
          
          #Extract model coefficients (syntax is slightly different depending on the model type e.g. lme4 v. nlme v. lm)
          if(any(grepl("climate", colnames(model.frame(baseline))))){
            
            coefs <- coef(summary(modeloutput))[, 1:2]
            
            temp.df <- data.frame("Y", t(coefs[-1, 1]), t(coefs[-1, 2]))
            
            colnames(temp.df) <- c("Custom.mod", gsub("scale\\(|\\)", "", rownames(coefs)[-1]), paste(gsub("scale\\(|\\)", "", rownames(coefs)[-1]), "SE", sep = ""))
            
            coef_data[[modno]] <- temp.df
            
          } else {
            
            if (class(baseline)[length(class(baseline))] == "coxph") {
              if (func == "quad"){
                modlist$ModelBeta[modno]  <- coef(modeloutput)[length(coef(modeloutput))-1]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-1]
                modlist$ModelBetaQ[modno] <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.ErrorQ[modno]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
                modlist$ModelBetaC[modno] <- NA
                modlist$ModelInt[modno]   <- 0
              } else if (func == "cub"){
                modlist$ModelBeta[modno]  <- coef(modeloutput)[length(coef(modeloutput))-2]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-2]
                modlist$ModelBetaQ[modno] <- coef(modeloutput)[length(coef(modeloutput))-1]
                modlist$Std.ErrorQ[modno]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-1]
                modlist$ModelBetaC[modno] <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.ErrorC[modno]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
                modlist$ModelInt[modno]   <- 0
              } else if (func == "centre"){
                if(centre[[2]] == "both"){
                  modlist$WithinGrpMean[modno] <- coef(modeloutput)[length(coef(modeloutput))]
                  modlist$Std.ErrorMean[modno] <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
                  modlist$WithinGrpDev[modno]  <- coef(modeloutput)[length(coef(modeloutput))-1]
                  modlist$Std.ErrorDev[modno]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))-1]
                  modlist$ModelInt[modno]      <- 0
                }
                if(centre[[2]] == "mean"){
                  modlist$WithinGrpMean[modno] <- coef(modeloutput)[length(coef(modeloutput))]
                  modlist$Std.Error[modno]     <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
                  modlist$ModelInt[modno]      <- 0
                }
                if(centre[[2]] == "dev"){
                  modlist$WithinGrpDev[modno]  <- coef(modeloutput)[length(coef(modeloutput))]
                  modlist$Std.Error[modno]     <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
                  modlist$ModelInt[modno]      <- 0
                }
              } else {
                modlist$ModelBeta[modno]  <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "se(coef)"][length(coef(modeloutput))]
                modlist$ModelBetaQ[modno] <- NA
                modlist$ModelBetaC[modno] <- NA
                modlist$ModelInt[modno]   <- 0
              }
            } else if (length(attr(class(modeloutput),"package")) > 0 && attr(class(modeloutput), "package") == "lme4"){            
              if (func == "quad"){
                modlist$ModelBeta[modno]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelBetaQ[modno] <- fixef(modeloutput)[length(fixef(modeloutput))]
                modlist$Std.ErrorQ[modno]  <- coef(summary(modeloutput))[, "Std. Error"][3]
                modlist$ModelBetaC[modno] <- NA
                modlist$ModelInt[modno]   <- fixef(modeloutput)[1]
              } else if (func == "cub"){
                modlist$ModelBeta[modno]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 2]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelBetaQ[modno] <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                modlist$Std.ErrorQ[modno]  <- coef(summary(modeloutput))[, "Std. Error"][3]
                modlist$ModelBetaC[modno] <- fixef(modeloutput)[length(fixef(modeloutput))]
                modlist$Std.ErrorC[modno]  <- coef(summary(modeloutput))[, "Std. Error"][3]
                modlist$ModelInt[modno]   <- fixef(modeloutput)[1]
              } else if (func == "centre"){
                if(centre[[2]] == "both"){
                  modlist$WithinGrpMean[modno] <- fixef(modeloutput)[length(fixef(modeloutput))]
                  modlist$Std.ErrorMean[modno] <- coef(summary(modeloutput))[, "Std. Error"][2]
                  modlist$WithinGrpDev[modno]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                  modlist$Std.ErrorDev[modno]  <- coef(summary(modeloutput))[, "Std. Error"][3]
                  modlist$ModelInt[modno]      <- fixef(modeloutput)[1]
                }
                if(centre[[2]] == "mean"){
                  modlist$WithinGrpMean[modno] <- fixef(modeloutput)[length(fixef(modeloutput))]
                  modlist$Std.Error[modno]     <- coef(summary(modeloutput))[, "Std. Error"][2]
                  modlist$ModelInt[modno]      <- fixef(modeloutput)[1]
                }
                if(centre[[2]] == "dev"){
                  modlist$WithinGrpDev[modno]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                  modlist$Std.Error[modno]     <- coef(summary(modeloutput))[, "Std. Error"][2]
                  modlist$ModelInt[modno]      <- fixef(modeloutput)[1]
                }
              } else {
                
                modlist$ModelBeta[modno]  <- fixef(modeloutput)[length(fixef(modeloutput))]
                
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelBetaQ[modno] <- NA
                modlist$ModelBetaC[modno] <- NA
                modlist$ModelInt[modno]   <- fixef(modeloutput)[1]
              }
              
            } else if(attr(baseline, "class")[1] == "lme"){
              
              if (func == "quad"){
                modlist$ModelBeta[modno]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "Std.Error"][2]
                modlist$ModelBetaQ[modno] <- fixef(modeloutput)[length(fixef(modeloutput))]
                modlist$Std.ErrorQ[modno]  <- coef(summary(modeloutput))[, "Std.Error"][3]
                modlist$ModelBetaC[modno] <- NA
                modlist$ModelInt[modno]   <- fixef(modeloutput)[1]
              } else if (func == "cub"){
                modlist$ModelBeta[modno]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 2]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "Std.Error"][2]
                modlist$ModelBetaQ[modno] <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                modlist$Std.ErrorQ[modno]  <- coef(summary(modeloutput))[, "Std.Error"][3]
                modlist$ModelBetaC[modno] <- fixef(modeloutput)[length(fixef(modeloutput))]
                modlist$Std.ErrorC[modno]  <- coef(summary(modeloutput))[, "Std.Error"][3]
                modlist$ModelInt[modno]   <- fixef(modeloutput)[1]
              } else if (func == "centre"){
                if(centre[[2]] == "both"){
                  modlist$WithinGrpMean[modno] <- fixef(modeloutput)[length(fixef(modeloutput))]
                  modlist$Std.ErrorMean[modno] <- coef(summary(modeloutput))[, "Std.Error"][2]
                  modlist$WithinGrpDev[modno]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                  modlist$Std.ErrorDev[modno]  <- coef(summary(modeloutput))[, "Std.Error"][3]
                  modlist$ModelInt[modno]      <- fixef(modeloutput)[1]
                }
                if(centre[[2]] == "mean"){
                  modlist$WithinGrpMean[modno] <- fixef(modeloutput)[length(fixef(modeloutput))]
                  modlist$Std.Error[modno]     <- coef(summary(modeloutput))[, "Std.Error"][2]
                  modlist$ModelInt[modno]      <- fixef(modeloutput)[1]
                }
                if(centre[[2]] == "dev"){
                  modlist$WithinGrpDev[modno]  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
                  modlist$Std.Error[modno]     <- coef(summary(modeloutput))[, "Std.Error"][2]
                  modlist$ModelInt[modno]      <- fixef(modeloutput)[1]
                }
              } else {
                
                modlist$ModelBeta[modno]  <- fixef(modeloutput)[length(fixef(modeloutput))]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "Std.Error"][2]
                modlist$ModelBetaQ[modno] <- NA
                modlist$ModelBetaC[modno] <- NA
                modlist$ModelInt[modno]   <- fixef(modeloutput)[1]
              }
              
            } else {
              if (func == "quad"){
                modlist$ModelBeta[modno]  <- coef(modeloutput)[length(coef(modeloutput)) - 1]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelBetaQ[modno] <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.ErrorQ[modno]  <- coef(summary(modeloutput))[, "Std. Error"][3]
                modlist$ModelBetaC[modno] <- NA
                modlist$ModelInt[modno]   <- coef(modeloutput)[1]
              } else if (func == "cub"){
                modlist$ModelBeta[modno]  <- coef(modeloutput)[length(coef(modeloutput)) - 2]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelBetaQ[modno] <- coef(modeloutput)[length(coef(modeloutput)) - 1]
                modlist$Std.ErrorQ[modno]  <- coef(summary(modeloutput))[, "Std. Error"][3]
                modlist$ModelBetaC[modno] <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.ErrorC[modno]  <- coef(summary(modeloutput))[, "Std. Error"][4]
                modlist$ModelInt[modno]   <- coef(modeloutput)[1]
              } else if (func == "centre"){
                if(centre[[2]] == "both"){
                  modlist$WithinGrpMean[modno] <- coef(modeloutput)[length(coef(modeloutput))]
                  modlist$Std.ErrorMean[modno]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                  modlist$WithinGrpDev[modno]  <- coef(modeloutput)[length(coef(modeloutput)) - 1]
                  modlist$Std.ErrorDev[modno]  <- coef(summary(modeloutput))[, "Std. Error"][3]
                  modlist$ModelInt[modno]      <- coef(modeloutput)[1]
                }
                if(centre[[2]] == "mean"){
                  modlist$WithinGrpMean[modno] <- coef(modeloutput)[length(coef(modeloutput))]
                  modlist$Std.ErrorMean[modno]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                  modlist$ModelInt[modno]      <- coef(modeloutput)[1]
                }
                if(centre[[2]] == "dev"){
                  modlist$WithinGrpDev[modno]  <- coef(modeloutput)[length(coef(modeloutput)) - 1]
                  modlist$Std.ErrorDev[modno]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                  modlist$ModelInt[modno]      <- coef(modeloutput)[1]
                }
              } else {
                modlist$ModelBeta[modno]  <- coef(modeloutput)[length(coef(modeloutput))]
                modlist$Std.Error[modno]  <- coef(summary(modeloutput))[, "Std. Error"][2]
                modlist$ModelBetaQ[modno] <- NA
                modlist$ModelBetaC[modno] <- NA
                modlist$ModelInt[modno]   <- coef(modeloutput)[1]
              }
            }
          }
          modno <- modno + 1        #Increase modno#
        }
      }
    }  
    if (interactive()){
      setTxtProgressBar(pb, modno - 1) 
    }
  }
  
  #Save the best model output
  m <- (modlist$WindowOpen[modlist$ModelAICc %in% min(modlist$ModelAICc)])
  n <- (modlist$WindowOpen[modlist$ModelAICc %in% min(modlist$ModelAICc)]) - (modlist$WindowClose[modlist$ModelAICc %in% min(modlist$ModelAICc)]) + 1
  windowopen  <- m[1] - range[2] + 1
  windowclose <- windowopen - n[1] + 1
  if (stat == "slope"){
    time      <- n[1]:1
    modeldat$climate <- apply(cmatrix[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
  } else {
    ifelse (windowopen - windowclose == 0, 
            modeldat$climate <- cmatrix[, windowclose:windowopen], 
            modeldat$climate <- apply(cmatrix[, windowclose:windowopen], 1, FUN = stat))
  }
  
  if (!is.null(centre[[1]])){
    if (centre[[2]] == "both"){
      modeldat$WGdev   <- wgdev(modeldat$climate, centre[[1]])
      modeldat$WGmean  <- wgmean(modeldat$climate, centre[[1]])
      
      if(class(baseline)[1] == "coxph"){
        
        LocalModel <- my_update(modeloutput, .~., data = modeldat)
        
      } else {
        
        LocalModel <- update(modeloutput, .~., data = modeldat)
        
      }
      
    }
    if (centre[[2]] == "dev"){
      modeldat$WGdev   <- wgdev(modeldat$climate, centre[[1]])
      
      if(class(baseline)[1] == "coxph"){
        
        LocalModel <- my_update(modeloutput, .~., data = modeldat)
        
      } else {
        
        LocalModel <- update(modeloutput, .~., data = modeldat)
        
      }
      
    }
    if (centre[[2]] == "mean"){
      modeldat$WGmean  <- wgmean(modeldat$climate, centre[[1]])
      
      if(class(baseline)[1] == "coxph"){
        
        LocalModel <- my_update(modeloutput, .~., data = modeldat)
        
      } else {
        
        LocalModel <- update(modeloutput, .~., data = modeldat)
        
      }
      
    }
    modlist$Function <- "centre"
  } else {
    
    if(class(baseline)[1] == "coxph"){
      
      LocalModel <- my_update(modeloutput, .~., data = modeldat)
      
    } else {
      
      LocalModel <- update(modeloutput, .~., data = modeldat)
      
    }
    
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
    
    modlist <- cbind(modlist, do.call(rbind, coef_data))
    
  }
  
  if (nrandom == 0){
    if (is.null(centre[[1]]) == FALSE){
      LocalData         <- model.frame(LocalModel)
      LocalData$climate <- modeldat$climate
    } else {
      LocalData <- model.frame(LocalModel)
      
      if(attr(LocalModel, "class")[1] == "lme"){
        
        non_rand <- ncol(LocalData)
        
        LocalData <- cbind(LocalData, LocalModel$data[, colnames(LocalModel$fitted)[-which(colnames(LocalModel$fitted) %in% "fixed")]])
        
        colnames(LocalData)[-(1:non_rand)] <- colnames(LocalModel$fitted)[-which(colnames(LocalModel$fitted) %in% "fixed")]
        
      }
      
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
                           func = "lin", type, refday, nrandom = 0, centre = NULL, k = 0, 
                           weightfunc = "W", cinterval = "day", cmissing = FALSE, cohort = NULL, spatial = NULL,
                           par = c(3, 0.2, 0), control = list(ndeps = c(0.001, 0.001, 0.001)), 
                           method = "L-BFGS-B", cutoff.day = NULL, cutoff.month = NULL,
                           furthest = NULL, closest = NULL, grad = FALSE){
  
  #Check date formats
  if(all(is.na(as.Date(cdate, format = "%d/%m/%Y")))){
    
    stop("cdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  if(all(is.na(as.Date(bdate, format = "%d/%m/%Y")))){
    
    stop("bdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
    
  }
  
  if(is.null(cohort) == TRUE){
    cohort = lubridate::year(as.Date(bdate, format = "%d/%m/%Y")) 
  }
  
  if(type == "variable" || type == "fixed"){
    stop("Parameter 'type' now uses levels 'relative' and 'absolute' rather than 'variable' and 'fixed'.")
  }
  
  if(is.null(furthest) == FALSE && is.null(closest) == FALSE){
    stop("furthest and closest are now redundant. Please use parameter 'range' instead.")
  }
  
  if(is.null(cutoff.day) == FALSE && is.null(cutoff.month) == FALSE){
    stop("cutoff.day and cutoff.month are now redundant. Please use parameter 'refday' instead.")
  }
  
  if(is.null(centre[[1]]) == FALSE){
    func = "centre"
    if(centre[[2]] != "both" && centre[[2]] != "dev" && centre[[2]] != "mean"){
      stop("Please set centre to one of 'both', 'dev', or 'mean'. See help file for details.")
    }
  }
  
  if(is.null(spatial) == FALSE){
    
    if(is.null(cohort) == FALSE){
      
      sample.size <- 0
      data <- data.frame(bdate = bdate, spatial = as.factor(spatial[[1]]), cohort = as.factor(cohort))
      
      for(i in unique(data$cohort)){
        
        sub <- subset(data, cohort == i)
        sub$spatial <- factor(sub$spatial)
        sample.size <- sample.size + length(levels(sub$spatial))
        
      }
      
    } else if(is.null(cohort) == TRUE){
      
      sample.size <- 0
      data <- data.frame(bdate = bdate, spatial = as.factor(spatial[[1]]))
      data$Year <- lubridate::year(as.Date(data$bdate, format = "%d/%m/%Y"))
      
      for(i in unique(data$Year)){
        
        sub <- subset(data, data$Year == i)
        sub$spatial <- factor(sub$spatial)
        sample.size <- sample.size + length(levels(sub$spatial))        
        
      }
      
    }
    
  } else if(is.null(spatial) == TRUE) {
    
    if(is.null(cohort) == FALSE){
      sample.size <- length(unique(cohort))
    } else {
      sample.size <- length(unique(lubridate::year(as.Date(bdate, format = "%d/%m/%Y"))))
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
  
  if(attr(baseline, "class")[1] == "lme"){
    
    if(is.null(baseline$modelStruct$varStruct) == FALSE && !is.null(attr(baseline$modelStruct$varStruct, "groups"))){
      
      modeldat <- cbind(modeldat, attr(baseline$modelStruct$varStruct, "groups"))
      
      colnames(modeldat)[ncol(modeldat)] <- strsplit(x = as.character(attr(baseline$modelStruct$varStruct, "formula"))[2], split = " | ")[[1]][3]
      
    }
    
    non_rand <- ncol(modeldat)
    
    modeldat <- cbind(modeldat, baseline$data[, colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")]])
    
    colnames(modeldat)[-(1:non_rand)] <- colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% "fixed")]
    
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
  
  #return(cmatrix)
  
  if(cmissing == FALSE & any(is.na(cmatrix))){ #If the user doesn't expect missing climate data BUT there are missing data present...
    if(is.null(spatial) == FALSE){ #And spatial data has been provided...
      
      if (cinterval == "day"){ #Where a daily interval is used...
        #...save an object 'missing' with the full dates of all missing data.
        .GlobalEnv$missing <- as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      
      if (cinterval == "month"){ #Where a monthly interval is used...
        #...save an object 'missing' with the month and year of all missing data.
        .GlobalEnv$missing <- c(paste("Month:", cont$cintno$Date[is.na(cont$xvar$Clim)] - (floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/12) * 12),
                                      #lubridate::month(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/12)))
        #lubridate::year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
      if (cinterval == "week"){ #Where weekly data is used...
        #...save an object 'missing' with the week and year of all missing data.
        .GlobalEnv$missing <- c(paste("Week:", cont$cintno$Date[is.na(cont$xvar$Clim)] - (floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/52) * 52),
                                      #lubridate::week(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cont$cintno$Date[is.na(cont$xvar$Clim)]/52)))
        #lubridate::year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1))))
      }
    } else { #If spatial data is not provided.
      
      if (cinterval == "day"){ #Do the same for day (syntax is just a bit differen)
        .GlobalEnv$missing <- as.Date(cont$cintno[is.na(cont$xvar)], origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 1)
      }
      if (cinterval == "month"){
        .GlobalEnv$missing <- c(paste("Month:", (lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1)) - (floor((lubridate::month(min(as.Date(cdate, format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) - 1))/12)*12),
                                      "Year:", (floor((which(is.na(cont$xvar)) - 1)/12) + lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))))))
      }
      if (cinterval == "week"){
        .GlobalEnv$missing <- c(paste("Week:", cont$cintno[is.na(cont$xvar)] - (floor(cont$cintno[is.na(cont$xvar)]/52) * 52),
                                      #ceiling(((as.numeric((as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y"))) - (floor(which(is.na(cmatrix))/nrow(cmatrix))*7)) - as.numeric(as.Date(paste("01/01/", lubridate::year(as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) + 1) / 7),
                                      "Year:", lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + floor(cont$cintno[is.na(cont$xvar)]/52)))
        #lubridate::year(as.Date(bdate[which(is.na(cmatrix)) - floor(which(is.na(cmatrix))/nrow(cmatrix))*nrow(cmatrix)], format = "%d/%m/%Y"))))
      }
    }
    
    #Create an error to warn about missing data
    stop(c("Climate data should not contain NA values: ", length(.GlobalEnv$missing),
           " NA value(s) found. Please add missing climate data or set cmissing to `method1` or `method2`.
           See object 'missing' for all missing climate data"))
  }
  
  #If we expect NAs and choose a method to deal with them...
  if (cmissing != FALSE && any(is.na(cmatrix))){
    
    message("Missing climate data detected. Please wait while NAs are replaced.")
    
    for(i in which(is.na(cmatrix))){
      
      #Determine the column and row location...
      if(i %% nrow(cmatrix) == 0){
        
        col <- i/nrow(cmatrix)
        row <- nrow(cmatrix)
        
      } else {
        
        col <- i%/%nrow(cmatrix) + 1
        row <- i %% nrow(cmatrix)
        
      }
      
      
      #If we are using method1
      if(cmissing == "method1"){
        
        #If we are using a daily interval
        if(cinterval == "day"){
          
          #For the original cdate data extract date information.
          cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"))
          
          #Extract the original biological date
          bioldate <- as.Date(bdate[row], format = "%d/%m/%Y")
          
          #Determine from this on which date data is missing
          missingdate <- bioldate - (col + range[2] - 1)
          
          #If we have spatial replication
          if(is.null(spatial) == FALSE){
            
            cdate_new$spatial <- spatial[[2]]
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)) & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)))], na.rm = T)
            
          }
          
        } else if(cinterval == "week" || cinterval == "month"){
          
          if(is.null(spatial) == FALSE){
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno$Date,
                                    spatial = cont$cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno$Date[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(cont$xvar$Clim[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)) & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            cmatrix[row, col] <- mean(cont$xvar[which(cdate_new$Date %in% c(missingdate - (1:2), missingdate + (1:2)))], na.rm = T)
            
          }
          
        }
        
        #If the record is still an NA, there must be too many NAs. Give an error.
        if(is.na(cmatrix[row, col])){
          
          stop("Too many consecutive NAs present in the data. Consider using method2 or manually replacing NAs.")
          
        }
        
      } else if(cmissing == "method2"){
        
        if(cinterval == "day"){
          
          #For the original cdate data, determine the year, month, week and day.
          cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"),
                                  Month = lubridate::month(as.Date(cdate, format = "%d/%m/%Y")),
                                  Day   = lubridate::day(as.Date(cdate, format = "%d/%m/%Y")))
          
          #Extract the original biological date
          bioldate <- as.Date(bdate[row], format = "%d/%m/%Y")
          
          #Determine from this on which date data is missing
          missingdate <- bioldate - (col + range[2] - 1)
          
          missingdate <- data.frame(Date  = missingdate,
                                    Month = lubridate::month(missingdate),
                                    Day   = lubridate::day(missingdate))
          
          if(is.null(spatial) == FALSE){
            
            cdate_new$spatial <- spatial[[2]]
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% missingdate$Month & cdate_new$Day %in% missingdate$Day & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% missingdate$Month & cdate_new$Day %in% missingdate$Day)], na.rm = T)
            
          }
          
        } else if(cinterval == "week" || cinterval == "month"){
          
          if(is.null(spatial) == FALSE){
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno$Date,
                                    spatial = cont$cintno$spatial)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno$Date[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            #Convert all dates back into year specific values
            if(cinterval == "week"){
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/52) * 52)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 52, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/52) * 52)
              missingdate <- ifelse(missingdate == 0, 52, missingdate)
              
            } else {
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/12) * 12)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 12, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/12) * 12)
              missingdate <- ifelse(missingdate == 0, 12, missingdate)
              
            }
            
            siteID <- spatial[[1]][row]
            
            cmatrix[row, col] <- mean(cont$xvar$Clim[which(cdate_new$Date %in% missingdate & cdate_new$spatial %in% siteID)], na.rm = T)
            
          } else {
            
            #Extract the climate week numbers
            cdate_new <- data.frame(Date = cont$cintno)
            
            #Extract the biological week number that is missing
            bioldate <- cont$bintno[row]
            
            #Determine from this on which week data is missing
            missingdate <- bioldate - (col + range[2] - 1)
            
            #Convert all dates back into year specific values
            if(cinterval == "week"){
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/52) * 52)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 52, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/52) * 52)
              missingdate <- ifelse(missingdate == 0, 52, missingdate)
              
            } else {
              
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/12) * 12)
              cdate_new$Date <- ifelse(cdate_new$Date == 0, 12, cdate_new$Date)
              
              missingdate <- missingdate - (floor(missingdate/12) * 12)
              missingdate <- ifelse(missingdate == 0, 12, missingdate)
              
            }
            
            cmatrix[row, col] <- mean(cont$xvar[which(cdate_new$Date %in% missingdate)], na.rm = T)
            
          }
        }
        
        if(is.na(cmatrix[row, col])){
          
          stop("There is not enough data to replace missing values using method2. Consider dealing with NA values manually")
          
        }
        
      } else {
        
        stop("cmissing should be method1, method2 or FALSE")
        
      }
    }
  }
  
  #Check to see if the model contains a weight function. If so, incorporate this into the data used for updating the model.
  if("(weights)" %in% colnames(model.frame(baseline))){
    
    modeldat$model_weights  <- weights(baseline)
    #baseline <- update(baseline, yvar~., weights = model_weights, data = modeldat)
    
    call <- as.character(getCall(baseline))
    
    weight_name <- call[length(call)]
    
    names(modeldat)[length(names(modeldat))] <- weight_name
    
  }
  # if (is.null(weights(baseline)) == FALSE){
  #   if (class(baseline)[1] == "glm" && sum(weights(baseline)) == nrow(model.frame(baseline)) || attr(class(baseline), "package") == "lme4" && sum(weights(baseline)) == nrow(model.frame(baseline))){
  #   } else {
  #     
  #     modeldat$model_weights  <- weights(baseline)
  #     #baseline <- update(baseline, yvar~., weights = model_weights, data = modeldat)
  #     
  #     call <- as.character(getCall(baseline))
  #     
  #     weight_name <- call[length(call)]
  #     
  #     names(modeldat)[length(names(modeldat))] <- weight_name
  #     
  #   }
  # }
  
  #If using a mixed model, ensure that maximum likelihood is specified (because we are comparing models with different fixed effects)
  if(!is.null(attr(class(baseline), "package")) && attr(class(baseline), "package") == "lme4" && class(baseline)[1] == "lmerMod" && baseline@resp$REML == 1){
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, REML = F)
    
  }
  
  if(attr(baseline, "class")[1] == "lme" && baseline$method == "REML"){
    
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    
    baseline <- update(baseline, yvar ~., data = modeldat, method = "ML")
    
  }
  
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
      stop("Define func")
    } 
    
  } else {
    
    modeloutput <- update(baseline, yvar~., data = modeldat)
    
  }
  
  #If cross validation has been specified...
  if (k > 1){
    modeldat$K <- sample(seq(from = 1, to = length(modeldat$climate), by = 1) %% k + 1)
  }   # create labels k-fold crossvalidation
  
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
                      modeloutput = modeloutput, baseline = baseline, modeldat = modeldat, 
                      funcenv = funcenv,  
                      cmatrix = cmatrix, nullmodel = nullmodel) 
      
    } else {
      
      result <- optim(par = par, fn = modloglik_W,
                      control = control, 
                      method = method, lower = c(0.0001, 0.0001, -Inf), 
                      upper = c(Inf, Inf, 0), duration = duration, 
                      modeloutput = modeloutput, baseline = baseline, modeldat = modeldat, 
                      funcenv = funcenv, k = k,
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
      
      message(result)
      
    }
    
  } else if (weightfunc == "G"){
    if (par[2] <= 0){
      stop("GEV scale parameter should be >0")
    }
    j      <- seq(-10, 10, by = (2 * 10 / duration))
    
    if(grad == TRUE){
      
      result <- optim(par = par, fn = modloglik_G, 
                      gr = Uni_grad_G, 
                      control = control, k = k,
                      method = method, lower = c(-Inf, 0.0001, -Inf), 
                      upper = c(Inf, Inf, Inf), duration = duration, 
                      modeloutput = modeloutput, baseline = baseline, funcenv = funcenv,
                      cmatrix = cmatrix, nullmodel = nullmodel)
      
    } else {
      
      result <- optim(par = par, fn = modloglik_G, 
                      control = control, k = k, 
                      method = method, lower = c(-Inf, 0.0001, -Inf), 
                      upper = c(Inf, Inf, Inf), duration = duration, 
                      modeloutput = modeloutput, baseline = baseline, funcenv = funcenv,
                      cmatrix = cmatrix, nullmodel = nullmodel)
      
    }
    
    if(n == 1){
      
      message(result)
      
    }
    
  } else if (weightfunc == "U"){
    
    if(length(par) > 2){
      warning("Uniform distribution only uses two parameters (start and end). All other parameter values are ignored.")
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
      
      message(result)
      
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
    } else if(attr(baseline, "class")[1] == "lme"){
      
      if (func == "quad"){
        WeightedOutput$ModelBeta  <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
        WeightedOutput$Std.Error  <- coef(summary(modeloutput))[, "Std.Error"][2]
        WeightedOutput$ModelBetaQ <- fixef(modeloutput)[length(fixef(modeloutput))]
        WeightedOutput$Std.ErrorQ <- coef(summary(modeloutput))[, "Std.Error"][3]
        WeightedOutput$ModelBetaC <- NA
        WeightedOutput$ModelInt   <- fixef(modeloutput)[1]
      } else if (func == "cub"){
        WeightedOutput$ModelBeta  <- fixef(modeloutput)[length(fixef(modeloutput)) - 2]
        WeightedOutput$Std.Error  <- coef(summary(modeloutput))[, "Std.Error"][2]
        WeightedOutput$ModelBetaQ <- fixef(modeloutput)[length(fixef(modeloutput)) - 1]
        WeightedOutput$Std.ErrorQ <- coef(summary(modeloutput))[, "Std.Error"][3]
        WeightedOutput$ModelBetaC <- fixef(modeloutput)[length(fixef(modeloutput))]
        WeightedOutput$Std.ErrorC <- coef(summary(modeloutput))[, "Std.Error"][3]
        WeightedOutput$ModelInt   <- fixef(modeloutput)[1]
      } else {
        WeightedOutput$ModelBeta  <- fixef(modeloutput)[length(fixef(modeloutput))]
        WeightedOutput$Std.Error  <- coef(summary(modeloutput))[, "Std.Error"][2]
        WeightedOutput$ModelBetaQ <- NA
        WeightedOutput$ModelBetaC <- NA
        WeightedOutput$ModelInt   <- fixef(modeloutput)[1]
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
                        refday, cross = FALSE, cohort, spatial, 
                        upper, lower, binary, thresholdQ = NA){
  
  ######################################################################
  
  #INITIAL CHECKS
  
  if (cinterval != "day" && cinterval != "week" && cinterval != "month"){
    stop("cinterval should be either day, week or month")
  }
  
  ######################################################################
  
  # Convert the bdate variables into the R date format
  bdate  <- as.Date(bdate, format = "%d/%m/%Y")
  
  # If there is spatial replication (i.e. multiple sites are used)...
  if(!is.null(spatial)) {
    
    SUB.DATE <- list()
    NUM <- 1
    
    for(i in unique(spatial[[2]])){ # For every listed site...
      
      SUB <- cdate[which(spatial[[2]] == i)] # Extract the date data from this site...
      
      SUB.DATE[[NUM]] <- data.frame(Date = seq(min(as.Date(SUB, format = "%d/%m/%Y")), max(as.Date(SUB, format = "%d/%m/%Y")), "days"),
                                    spatial = i) # Save this data in its own dataframe, with all possible dates within the range for that site only.
      
      if (nrow(SUB.DATE[[NUM]]) != length(unique(SUB.DATE[[NUM]]$Date))){
        stop ("There are duplicate dayrecords in climate data") # Check there are no duplicates within each site...
      }
      
      NUM <- NUM + 1
      
    }
    
    spatialcdate <- do.call(rbind, SUB.DATE) # Combine all date data from each site together...
    cdate2       <- spatialcdate$Date # Save this new date data as cdate2..
    cintno       <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 1   # atrribute daynumbers for both climate and biological data with first date in the climate data set to cintno 1
    realbintno   <- as.numeric(bdate) - min(as.numeric(cdate2)) + 1
  } else { # If there is no spatial information...
    cdate2     <- seq(min(as.Date(cdate, format = "%d/%m/%Y")), max(as.Date(cdate, format = "%d/%m/%Y")), "days") # Create a new dataframe with all possible dates within the date range given... 
    cintno     <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to cintno 1
    realbintno <- as.numeric(bdate) - min(as.numeric(cdate2)) + 1
    if (length(cintno) != length(unique(cintno))){
      stop ("There are duplicate dayrecords in climate data") # Check for duplicate date information. 
    }
  }
  
  cdate  <- as.Date(cdate, format = "%d/%m/%Y") # Also have an object saving the original date information (this way we can work out where climate data is missing!)
  
  if(is.null(spatial) == FALSE){ # If spatial data is provided...
    
    for(i in unique(spatial[[2]])){ # For each possible spatial site...
      
      SUB <- cdate[which(spatial[[2]] == i)] # Extract the cdate information for each site
      SUB_biol <- bdate[which(spatial[[1]] == i)] # Extract the bdate information for each site
      if (min(SUB) > min(SUB_biol)){ # Check that the earliest climate data is before the earliest biological data...
        stop(paste("Climate data does not cover all years of biological data at site ", i ,". Earliest climate data is ", min(cdate), " Earliest biological data is ", min(bdate), ". Please increase range of climate data", sep = ""))
      }
      if (max(SUB) < max(SUB_biol)){ # Check that the latest climate data is after or the same time as the latest biological data...
        stop(paste("Climate data does not cover all years of biological data at site ", i ,". Latest climate data is ", max(cdate), " Latest biological data is ", max(bdate), ". Please increase range of climate data", sep = ""))
      }
    }
  } else if(is.null(spatial) == TRUE){
    
    if (min(cdate) > min(bdate)){ # If spatial data is not provided, also check the overlap between climate and biological data as above.
      stop(paste("Climate data does not cover all years of biological data. Earliest climate data is ", min(cdate), ". Earliest biological data is ", min(bdate), sep = ""))
    }
    
    if (max(cdate) < max(bdate)){
      stop(paste("Climate data does not cover all years of biological data. Earliest climate data is ", max(cdate), ". Earliest biological data is ", max(bdate), sep = ""))
    }
    
  }
  
  if (is.null(xvar2) == FALSE){ # if there are multiple climate variables included (i.e. for crosswin/autowin)...
    if (is.null(spatial) == FALSE){ # ...and there is spatial data provided...
      xvar2      <- data.frame(Clim = xvar2, spatial = spatial[[2]]) # ...create a new dataframe with the second climate variable and spatial info
      cdatetemp  <- data.frame(Date = cdate, spatial = spatial[[2]]) # ...do the same for date information
      split.list <- list()
      NUM <- 1
      for(i in unique(xvar2$spatial)){ # For each spatial site...
        SUB <- subset(xvar2, spatial == i) # ...subset out relevant climate data from that site...
        SUBcdate  <- subset(cdatetemp, spatial == i) # ...extract relevant date information...
        SUBcdate2 <- subset(spatialcdate, spatial == i)
        rownames(SUB) <- seq(1, nrow(SUB), 1)
        rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
        NewClim    <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)] #Work out where date information matches (i.e. there will be NAs where there is no match)
        Newspatial <- rep(i, times = length(NewClim))
        split.list[[NUM]] <- data.frame(NewClim, Newspatial) # Create a new dataframe with second climate variable and site ID
        NUM <- NUM + 1
      }
      xvar2    <- (do.call(rbind, split.list))$NewClim # Extract second climate data (with NAs where there is missing date info)
      climspatial <- (do.call(rbind, split.list))$Newspatial #Extract the new spatial data as well
    } else {
      xvar2    <- xvar2[match(cdate2, cdate)] # if there is no spatial replication, simply check for matches (i.e. include NAs where appropriate)
    }
  }
  
  if(is.null(spatial) == FALSE){ # If spatial replication is present...
    xvar       <- data.frame(Clim = xvar, spatial = spatial[[2]]) # extract original climate info and spatial data
    cdate      <- data.frame(Date = cdate, spatial = spatial[[2]]) # Do the same for date information
    split.list <- list()
    NUM <- 1
    
    for(i in unique(xvar$spatial)){ #For each site ID...
      SUB <- subset(xvar, spatial == i) #Subset out climate data for that site...
      SUBcdate  <- subset(cdate, spatial == i) #extract recorded date info for each site
      SUBcdate2 <- subset(spatialcdate, spatial == i) #extract potential dates for each site (i.e. range from earliest to latest)
      rownames(SUB) <- seq(1, nrow(SUB), 1)
      rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
      NewClim    <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)] #Determine where there are overlaps (i.e. NAs where there is no match)
      Newspatial <- rep(i, times = length(NewClim))
      split.list[[NUM]] <- data.frame(NewClim, Newspatial) # Create a new dataframe with this climate data and site ID info
      NUM <- NUM + 1
    }
    xvar    <- (do.call(rbind, split.list))$NewClim #save climate data (with NAs)
    climspatial <- (do.call(rbind, split.list))$Newspatial #Save site ID info (same length as that with NAs)
  } else {
    xvar    <- xvar[match(cdate2, cdate)] #When there is no spatial replication, simply check for missing date info.
  }
  
  if (cross == FALSE){ #When we are not running crosswin...
    if (cinterval == "day"){ #...and we are using daily data...
      if (type == "absolute"){ #..and using an absolute window...
        
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Combine biological data with cohort info (N.B. when cohort isn't provided, it will be made the year of capture by default. See code at start of slidingwin).
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for(i in unique(cohort)){ # For each cohort (i.e. year, unless specified otherwise)...
          sub                               <- subset(newdat, cohort == i) # ...subset bdate data from that cohort...
          # As we are using an absolute value, determine the biological date number as refday/minimum year in the cohort (i.e. assume they are in the previous year) - earliest cdate
          bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
        }
        
      } else {
        bintno <- realbintno #If we are using relative windows, biological date number is just the same as bdate - earliest cdate
      }
    } else if (cinterval == "week"){ #...if we are using weekly data...
      
      #For daily data, we can determine upper and lower binary limits AFTER calculating cdate info.
      #However, for weekly and monthly data, we need to group our daily data into weeks or months.
      
      #If the user has specified a threshold, they may want to apply this to the daily data 
      #(i.e. if they have daily data but are using weekly/monthly to speed up analysis)
      #We have checked to see if this is the case using 'thresholdQ'.
      #If 'thresholdQ' is Y, the user wants thresholds estimated BEFORE calculated weekly/monthly data.
      
      if(!is.na(thresholdQ) && thresholdQ == "Y"){
        
        if(binary == T){
          
          if(is.na(upper) == FALSE && is.na(lower) == TRUE){
            
            xvar <- ifelse(xvar > upper, 1, 0)
            
          } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){
            
            xvar <- ifelse(xvar < lower, 1, 0)
            
          } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){
            
            xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)
            
          }
          
        } else {
          
          if(is.na(upper) == FALSE && is.na(lower) == TRUE){
            
            xvar <- ifelse(xvar > upper, xvar, 0)
            
          } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){
            
            xvar <- ifelse(xvar < lower, xvar, 0)
            
          } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){
            
            xvar <- ifelse(xvar > lower & xvar < upper, xvar, 0)
            
          }
          
        }
      }
      
      cweek      <- lubridate::week(cdate2) # atrribute week numbers for both datafiles with first week in climate data set to cintno 1
      #A year doesn't divide evenly into weeks (what a silly method of splitting up a year!)
      #Therefore, there is a week 53 which has 1-2 days (depending on leapyears)
      #For our purposes, we will group the 1-2 days in week 53 into week 52.
      cweek[which(cweek == 53)] <- 52
      cyear      <- lubridate::year(cdate2) - min(lubridate::year(cdate2))
      cintno     <- cweek + 52 * cyear
      realbintno <- lubridate::week(bdate) + 52 * (lubridate::year(bdate) - min(lubridate::year(cdate2)))
      #cintno      <- ceiling((as.numeric(cdate2) - min(as.numeric(cdate2)) + 1) / 7)   
      #realbintno  <- ceiling((as.numeric(bdate) - min(as.numeric(cdate2)) + 1) / 7)
      if(is.null(spatial) == FALSE){ # If there is spatial replication...
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial) # ...create a dataframe with week number, climate data and site ID...
        newclim2    <- reshape::melt(newclim, id = c("cintno", "spatial")) # ...melt this so that we save the mean climate from each week at each site ID is seperated... #
        newclim3    <- reshape::cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T) 
        newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ] # Order data by site ID and week
        cintno      <- newclim3$cintno #Extract week numbers
        xvar        <- newclim3$xvar #Extract climate
        climspatial <- newclim3$spatial #Extract site ID
      } else { #If there is no spatial replication...
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar) # ...create data with week number and climate data 
        newclim2    <- reshape::melt(newclim, id = "cintno") #melt so that there is mean climate data for each week 
        newclim3    <- reshape::cast(newclim2, cintno ~ variable, mean, na.rm = T)
        cintno      <- newclim3$cintno #Extract week numbers
        xvar        <- newclim3$xvar #Extract climate
      }
      
      if (type == "absolute"){ #If we are dealing with absolute windows
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Combine date numbers from biological data with the cohort (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for(i in unique(cohort)){ # For each cohort...
          sub                               <- subset(newdat, cohort == i) #...subset out biological date data
          #Turn this date info into the same values based on refday
          bintno[as.numeric(rownames(sub))] <- lubridate::week(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) + 52 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
          #lubridate::week(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(cweek + 53 * cyear) + 1
        }
      } else { #...Otherwise just leave the biological date info as is.
        bintno <- realbintno
      }
    } else if (cinterval == "month"){ # If cinterval is month instead...
      
      #Once again, check if the user wants to calculate thresholds before determining weekly/monthly means.
      
      if(!is.na(thresholdQ) && thresholdQ == "Y"){
        
        if(binary == T){
          
          if(is.na(upper) == FALSE && is.na(lower) == TRUE){
            
            xvar <- ifelse(xvar > upper, 1, 0)
            
          } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){
            
            xvar <- ifelse(xvar < lower, 1, 0)
            
          } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){
            
            xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)
            
          }
          
        } else {
          
          if(is.na(upper) == FALSE && is.na(lower) == TRUE){
            
            xvar <- ifelse(xvar > upper, xvar, 0)
            
          } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){
            
            xvar <- ifelse(xvar < lower, xvar, 0)
            
          } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){
            
            xvar <- ifelse(xvar > lower & xvar < upper, xvar, 0)
            
          }
          
        }
      }
      
      cmonth     <- lubridate::month(cdate2) # Determine month numbers for all data...
      cyear      <- lubridate::year(cdate2) - min(lubridate::year(cdate2))
      cintno     <- cmonth + 12 * cyear
      realbintno <- lubridate::month(bdate) + 12 * (lubridate::year(bdate) - min(lubridate::year(cdate2)))
      
      if(is.null(spatial) == FALSE){ # If spatial replication is used...
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial) #Create a new dataframe with month number, climate data and site ID
        newclim2    <- reshape::melt(newclim, id = c("cintno", "spatial")) #Melt to just have mean climate for each month number and site ID
        newclim3    <- reshape::cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ] #Order by site ID and month
        cintno      <- newclim3$cintno #Save month, climate data and site ID
        xvar        <- newclim3$xvar
        climspatial <- newclim3$spatial
      } else { #If there is no spatial data...
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar) #Determine mean climate data for each month.
        newclim2   <- reshape::melt(newclim, id = "cintno")
        newclim3   <- reshape::cast(newclim2, cintno ~ variable, mean, na.rm = T)
        cintno     <- newclim3$cintno
        xvar       <- newclim3$xvar 
      }
      if (type == "absolute"){ #When using absolute windows...
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Bind biological date and cohort info (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for(i in unique(cohort)){ #For each year...
          sub                               <- subset(newdat, cohort == i) #Extract biological date info
          #Set the biological date the same for each cohort.
          bintno[as.numeric(rownames(sub))] <- refday[2] + 12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
        }
      } else { #Otherwise, just leave the biological date unchanged.
        bintno <- realbintno
      }
    }
  } else { # When we are running cross win...
    if (cinterval == "day"){  #And using a daily interval...
      if (type == "absolute"){ #And an absolute window...
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Combine biological date and cohort info (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for(i in unique(cohort)){ #For each cohort group...
          sub                               <- subset(newdat, cohort == i) #...subset out data.
          #Set all records within a cohort to the same value
          bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
        }
      } else { #If using relative windows, biological date data stays the same.
        bintno <- realbintno
      }    
    } else if (cinterval == "week"){ #If using weekly data...
      
      cweek      <- lubridate::week(cdate2) # atrribute week numbers for both datafiles with first week in climate data set to cintno 1
      cyear      <- lubridate::year(cdate2) - min(lubridate::year(cdate2))
      cintno     <- cweek + 53 * cyear
      cintno     <- cintno - min(cintno) + 1
      realbintno <- lubridate::month(bdate) + 53 * (lubridate::year(bdate) - min(lubridate::year(cdate2)))
      #cintno     <- ceiling((as.numeric(cdate2) - min(as.numeric(cdate2)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to cintno 1
      #realbintno <- ceiling((as.numeric(bdate) - min(as.numeric(cdate2)) + 1) / 7)
      if(is.null(spatial) == FALSE){ #When spatial data is available
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2, "spatial" = climspatial) #Create a dataset with both climate variables and siteID
        newclim2    <- reshape::melt(newclim, id = c("cintno", "spatial")) #Determine mean values for both climate variables each week at each site
        newclim3    <- reshape::cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        cintno      <- newclim3$cintno #Save info.
        xvar        <- newclim3$xvar
        xvar2       <- newclim3$xvar2
        climspatial <- newclim3$spatial
      } else { #If there is no spatial data, do the same but without site ID.
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2)
        newclim2   <- reshape::melt(newclim, id = "cintno")
        newclim3   <- reshape::cast(newclim2, cintno ~ variable, mean, na.rm = T)
        cintno     <- newclim3$cintno
        xvar       <- newclim3$xvar
        xvar2      <- newclim3$xvar2 
      }
      if (type == "absolute"){ #If using an absolute window.
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Combine biological data and cohort (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for(i in unique(cohort)){ #For each cohort...
          sub                               <- subset(newdat, cohort == i) #...subset data.
          #Create the same week value for every record in the same cohort.
          bintno[as.numeric(rownames(sub))] <- lubridate::month(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) + 53 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
        }
      } else { #If using relative windows just keep biological date data the same.
        bintno <- realbintno
      }
    } else if (cinterval == "month"){ #If using monthly data...
      
      cmonth     <- lubridate::month(cdate2) #Determine month number
      cyear      <- lubridate::year(cdate2) - min(lubridate::year(cdate2))
      cintno     <- cmonth + 12 * cyear
      realbintno <- lubridate::month(bdate) + 12 * (lubridate::year(bdate) - min(lubridate::year(cdate2)))
      if(is.null(spatial) == FALSE){ #If spatial data is used...
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2, "spatial" = climspatial) #Extract both climate variables and site ID
        newclim2    <- reshape::melt(newclim, id = c("cintno", "spatial")) #Determine mean climate for each climate variable at each site for each month.
        newclim3    <- reshape::cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        cintno      <- newclim3$cintno #Save extracted data.
        xvar        <- newclim3$xvar
        xvar2       <- newclim3$xvar2
        climspatial <- newclim3$spatial
      } else { #If no spatial data is provided, just determine mean for both climate variables in each month.
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2)
        newclim2   <- reshape::melt(newclim, id = "cintno")
        newclim3   <- reshape::cast(newclim2, cintno ~ variable, mean, na.rm = T)
        cintno     <- newclim3$cintno
        xvar       <- newclim3$xvar
        xvar2      <- newclim3$xvar2 
      }
      if (type == "absolute"){ #If using absolute windows.
        newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort)) #Extract date data and cohort (year by default)
        datenum  <- 1
        bintno   <- seq(1, length(bdate), 1)
        for(i in unique(cohort)){ #For each cohort
          sub                               <- subset(newdat, cohort == i) #Subset data
          #Set each record within a cohort to have the same month
          bintno[as.numeric(rownames(sub))] <- refday[2] + 12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
        }
      } else { #If not using absolute windows then just keep data as is.
        bintno <- realbintno
      }
    }
  }
  
  #Sometimes data may have infinity variables. Always make these NAs
  xvar <- ifelse(is.infinite(xvar), NA, xvar)
  
  if(is.null(xvar2) == FALSE){ #Do the same for the second climate variable if it is present.
    
    xvar2 <- ifelse(is.infinite(xvar2), NA, xvar2)
    
  }
  
  if(is.null(spatial) == FALSE){ #If spatial data is provided...
    if(is.null(xvar2) == FALSE){ #And a second climate variable is provided...
      #Return climate date, biological date and both climate variables (with spatial data included)
      return(list(cintno = data.frame(Date = cintno, spatial = climspatial),
                  bintno = data.frame(Date = bintno, spatial = spatial[[1]]),
                  xvar = data.frame(Clim = xvar, spatial = climspatial), 
                  xvar2 = data.frame(Clim = xvar2, spatial = climspatial)))
    } else { #If there is only one climate variable
      #Return climate date, biological date and single climate variables (with spatial data included)
      return(list(cintno = data.frame(Date = cintno, spatial = climspatial),
                  bintno = data.frame(Date = bintno, spatial = spatial[[1]]),
                  xvar = data.frame(Clim = xvar, spatial = climspatial)))
    }
  } else { #If spatial data is not included.
    if(is.null(xvar2) == FALSE){ #But a second climate variable is still used
      #Return climate date, biological date and both climate variables.
      return(list(cintno = cintno, bintno = bintno, xvar = xvar, xvar2 = xvar2))
    } else {
      #Return climate date, biological date and single climate variable.
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
modloglik_G <- function(par = par, modeloutput = modeloutput, baseline = baseline, k = k, 
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
  
  # If valid, perform k-fold crossvalidation
  if (k > 1) {      
    for (k in 1:k) {
      test                     <- subset(funcenv$modeldat, funcenv$modeldat$K == k) # Create the test dataset
      train                    <- subset(funcenv$modeldat, funcenv$modeldat$K != k) # Create the train dataset
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
    
    AICc_cv_avg                    <- AICc_cvtotal / k # Determine the average AICc value of the climate model from cross validations
    AICc_cv_baseline_avg           <- AICc_cv_basetotal / k # Determine the average AICc value of the null model from cross validations
    deltaAICc                      <- AICc_cv_avg - AICc_cv_baseline_avg # Calculate delta AICc
    funcenv$DAICc[[funcenv$modno]] <- deltaAICc 
    funcenv$par_shape[[funcenv$modno]]    <- par[1]
    funcenv$par_scale[[funcenv$modno]]    <- par[2]
    funcenv$par_location[[funcenv$modno]] <- par[3]
    
  } else {
    
    modeloutput                           <- update(modeloutput, .~., data = funcenv$modeldat)   # rerun regression model using new weather index
    deltaAICc                             <- AICc(modeloutput) - nullmodel
    funcenv$DAICc[[funcenv$modno]]        <- deltaAICc
    funcenv$par_shape[[funcenv$modno]]    <- par[1]
    funcenv$par_scale[[funcenv$modno]]    <- par[2]
    funcenv$par_location[[funcenv$modno]] <- par[3]
    
  }
  
  # plot the weight function and corresponding weather index being evaluated
  par(mfrow = c(3, 2))
  plot((weight / sum(weight)), type = "l", ylab = "weight", xlab = "timestep (e.g. days)", main = "Output of current weighted window being tested")
  plot(as.numeric(funcenv$par_shape), type = "l", ylab = "shape parameter", xlab = "convergence step", main = "GEV parameter values being tested")
  plot(as.numeric(funcenv$DAICc), type = "l", ylab = expression(paste(Delta, "AICc")), xlab = "convergence step")
  plot(as.numeric(funcenv$par_scale), type = "l", ylab = "scale parameter", xlab = "convergence step")
  #plot(funcenv$modeldat$climate[1:duration], type = "s", ylab = "weighted mean of weather", xlab = "timestep (e.g. days)")
  plot(x = funcenv$modeldat$climate, y = funcenv$modeldat$yvar, type = "p", ylab = "yvar", xlab = "weighted mean of weather")
  plot(as.numeric(funcenv$par_location), type = "l", ylab = "location parameter", xlab = "convergence step")
  
  funcenv$modno <- funcenv$modno + 1
  return(deltaAICc)  # returns deltaAICc as optim() minimizes! 
}


# define a function that returns the AICc or -2LogLikelihood of model using Weibull weight function
modloglik_W <- function(par = par,  modeloutput = modeloutput, baseline = baseline, k = k, duration = duration, 
                        cmatrix = cmatrix, modeldat = modeldat, nullmodel =  nullmodel, funcenv = funcenv){
  
  j                     <- seq(1:duration) / duration # rescale j to interval [0,1]
  weight                <- weibull3(x = j, shape = par[1], scale = par[2], location = par[3])  # calculate weights using the Weibull probability distribution function
  weight[is.na(weight)] <- 0
  
  if (sum(weight) == 0){
    weight <- weight + 1
  }
  
  weight                                <- weight / sum(weight)
  funcenv$modeldat$climate              <- apply(cmatrix, 1, FUN = function(x) {sum(x*weight)})    # calculate weighted mean from weather data
  
  # If valid, perform k-fold crossvalidation
  if (k > 1) {      
    for (k in 1:k) {
      test                     <- subset(funcenv$modeldat, funcenv$modeldat$K == k) # Create the test dataset
      train                    <- subset(funcenv$modeldat, funcenv$modeldat$K != k) # Create the train dataset
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
    
    AICc_cv_avg                    <- AICc_cvtotal / k # Determine the average AICc value of the climate model from cross validations
    AICc_cv_baseline_avg           <- AICc_cv_basetotal / k # Determine the average AICc value of the null model from cross validations
    deltaAICc                      <- AICc_cv_avg - AICc_cv_baseline_avg # Calculate delta AICc
    funcenv$DAICc[[funcenv$modno]] <- deltaAICc 
    funcenv$par_shape[[funcenv$modno]]    <- par[1]
    funcenv$par_scale[[funcenv$modno]]    <- par[2]
    funcenv$par_location[[funcenv$modno]] <- par[3]
    
  } else {
    
    modeloutput                           <- update(modeloutput, .~., data = funcenv$modeldat)   # rerun regression model using new weather index
    deltaAICc                             <- AICc(modeloutput) - nullmodel
    funcenv$DAICc[[funcenv$modno]]        <- deltaAICc
    funcenv$par_shape[[funcenv$modno]]    <- par[1]
    funcenv$par_scale[[funcenv$modno]]    <- par[2]
    funcenv$par_location[[funcenv$modno]] <- par[3]
    
  }
  
  # plot the weight function and corresponding weather index being evaluated
  par(mfrow = c(3, 2))
  plot((weight/sum(weight)), type = "l", ylab = "weight", xlab = "time step (e.g days)", main = "Output of current weighted window being tested")
  plot(as.numeric(funcenv$par_shape), type = "l", ylab = "shape parameter", xlab = "convergence step", main = "Weibull parameter values being tested")
  plot(as.numeric(funcenv$DAICc), type = "l", ylab = expression(paste(Delta, "AICc")), xlab = "convergence step")
  plot(as.numeric(funcenv$par_scale), type = "l", ylab = "scale parameter", xlab = "convergence step")
  plot(x = funcenv$modeldat$climate, y = funcenv$modeldat$yvar, type = "p", ylab = "yvar", xlab = "weighted mean of weather")
  #plot(funcenv$modeldat$climate[1:duration], type = "s", ylab = "weighted mean of weather", xlab = "time step (e.g days)")
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

skim <- function(winoutput, duration, cutoff) {
  winoutput$Duration <- winoutput$WindowOpen - winoutput$WindowClose
  winoutput$Filter   <- winoutput$WindowOpen * 0
  winoutput$Filter[which(winoutput$WindowOpen >= cutoff &  winoutput$WindowClose >= cutoff & winoutput$Duration < duration)] <- 1
  winoutput<-subset(winoutput, winoutput$Filter == 0)
  return(winoutput)
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

withOptions <- function(optlist, expr){
  oldopt <- options(optlist)
  on.exit(options(oldopt))
  expr <- substitute(expr)
  eval.parent(expr)
}

#################################################################################

#Theme for ggplots
theme_climwin <- function(base_size = 12, base_family = "",
                          base_line_size = base_size / 20,
                          base_rect_size = base_size / 20,
                          legend = "none") {
  half_line <- base_size / 2
  
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others. These set the defaults for line, rectangle and text elements.
    line = element_line(
      colour = "black", linewidth = base_line_size,
      linetype = 1, lineend = "round"
    ),
    rect =               element_rect(
      fill = "white", colour = "black",
      linewidth = base_rect_size, linetype = 1
    ),
    text =               element_text(
      family = base_family, face = "plain",
      colour = "black", size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin(), debug = FALSE
    ),
    
    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.8), colour = "black", face = "bold"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_line(colour = "black", lineend = "round", linewidth = 1),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =       element_text(
      margin = margin(t = half_line * 1.5),
      vjust = 1,
      face = "bold"
    ),
    axis.title.x.top =   element_text(
      margin = margin(b = half_line),
      vjust = 0,
      face = "bold"
    ),
    axis.title.y =       element_text(
      angle = 90,
      margin = margin(r = half_line * 1.5),
      vjust = 0,
      face = "bold"
    ),
    axis.title.y.right = element_text(
      angle = -90,
      margin = margin(l = half_line),
      vjust = 0,
      face = "bold"
    ),
    
    legend.background = element_rect(colour = NA),
    legend.spacing = unit(2 * half_line, "pt"),
    legend.spacing.x = NULL, 
    legend.spacing.y = NULL,
    legend.margin = margin(half_line, half_line, half_line, half_line),
    legend.key = element_rect(fill = "grey95", colour = "white"),
    legend.key.size = unit(1.2, "lines"), 
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.position = legend,
    legend.text = element_text(family = base_family, size = rel(1)),
    
    panel.background =   element_rect(fill = "white", colour = NA),
    panel.border =       element_rect(colour = "black", fill = NA, linewidth = 1.5),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,
    
    strip.background =   element_rect(fill = NA, colour = "black"),
    strip.text =         element_text(
      colour = "black",
      size = rel(0.8),
      margin = margin(half_line, half_line, half_line, half_line)
    ),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    
    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text(
      size = rel(1.2),
      hjust = 0.5, vjust = 1,
      margin = margin(b = half_line * 1.2)
    ),
    plot.subtitle =      element_text(
      size = rel(0.9),
      hjust = 0.5, vjust = 1,
      margin = margin(b = half_line * 0.9)
    ),
    plot.caption =       element_text(
      size = rel(0.9),
      hjust = 0.5, vjust = 1,
      margin = margin(t = half_line * 0.9)
    ),
    plot.margin =        margin(half_line, half_line, half_line, half_line),
    
    complete = TRUE
  )
}

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
#'
#'@importFrom evd dgev
#'@import numDeriv
#'@export

old_weightwin <- function(n = 1, xvar, cdate, bdate, baseline, range, k = 0,
                         func = "lin", type, refday = NULL, nrandom = 0, centre = NULL,
                         weightfunc = "W", cinterval = "day", cmissing = FALSE,
                         cohort = NULL, spatial = NULL,
                         par = c(3, 0.2, 0),
                         control = list(maxit = 100),
                         method = "L-BFGS-B",
                         cutoff.day = NULL, cutoff.month = NULL,
                         furthest = NULL, closest = NULL, grad = FALSE) {

  ## Validate xvar input
  if (!is.list(xvar)) stop("xvar should be an object of type list")
  if (is.null(names(xvar))) names(xvar) <- paste0("climate", seq_along(xvar))
  xvar_name <- names(xvar)[1]

  ## Warn when weightfunc = "G" — now explicitly the Gumbel distribution
  if (is.character(weightfunc) && weightfunc == "G") {
    warning("weightfunc = 'G' uses the Gumbel distribution via evd::dgumbel.")
  }

  ## Truncate par to 2 for standard built-in weightfuncs.
  ## Old API used 3 parameters (shape, scale, location); new API uses 2.
  if (is.character(weightfunc) && weightfunc %in% c("W", "G", "F")) {
    par <- par[seq_len(min(2L, length(par)))]
  }

  ## Validate cmissing argument
  if (!identical(cmissing, FALSE) && !cmissing %in% c("method1", "method2")) {
    stop("'cmissing' must be FALSE, 'method1', or 'method2'")
  }

  ## Convert refday: c(day, month) -> "DD/MM/YYYY"
  if (!is.null(refday) && length(refday) >= 2 && !any(is.na(refday))) {
    refday_str <- sprintf("%02d/%02d/2000", as.integer(refday[1]), as.integer(refday[2]))
  } else {
    refday_str <- NULL
  }

  ## Convert range: c(furthest, closest) -> seq(closest, furthest)
  new_range <- seq(range[2], range[1])

  ## Build climate data frame from the cdate vector and xvar values
  climate_df <- data.frame(Date = as.character(cdate), stringsAsFactors = FALSE)
  climate_df[[xvar_name]] <- xvar[[xvar_name]]

  ## Handle missing values in xvar per cmissing
  if (anyNA(climate_df[[xvar_name]])) {
    if (identical(cmissing, FALSE)) {
      stop(sprintf(
        "Missing values found in '%s'. Set cmissing = 'method1' or 'method2' to impute.",
        xvar_name
      ))
    } else if (cmissing == "method1") {
      climate_df <- repair_climate(climate_df, cdate = "Date", xvar = xvar_name,
                                   method = function(x) imputeTS::na_ma(x, k = 2))
    } else {
      temp_dates <- as.Date(climate_df$Date, format = "%d/%m/%Y")
      full_dates <- seq.Date(min(temp_dates, na.rm = TRUE),
                             max(temp_dates, na.rm = TRUE), by = "day")
      dm_key <- format(full_dates, "%m-%d")
      method2_fn <- local({
        dm <- dm_key
        function(x) {
          for (i in which(is.na(x))) {
            same_dm <- which(dm == dm[i] & !is.na(x))
            if (length(same_dm) > 0L) x[i] <- mean(x[same_dm])
          }
          x
        }
      })
      climate_df <- repair_climate(climate_df, cdate = "Date", xvar = xvar_name,
                                   method = method2_fn)
    }
  }

  ## Build biological data frame from the baseline model frame + date column
  bio_df           <- model.frame(baseline)
  bio_df[["Date"]] <- as.character(bdate)

  ## Add cohort column if provided
  cohort_col <- NULL
  if (!is.null(cohort)) {
    bio_df[["cohort_var"]] <- cohort
    cohort_col <- "cohort_var"
  }

  ## Add spatial columns if provided (old API: list of two vectors)
  spatial_col <- NULL
  if (!is.null(spatial)) {
    bio_df[["spatial_var"]]     <- spatial[[1]]
    climate_df[["spatial_var"]] <- spatial[[2]]
    spatial_col <- "spatial_var"
  }

  ## Pre-aggregate climate data for non-daily intervals
  if (cinterval != "day") {
    climate_df <- trans_clim_interval(climate_df,
                                      xvar      = xvar_name,
                                      cdate     = "Date",
                                      cinterval = cinterval)
  }

  ## Build new model formula by adding the climate predictor based on func
  base_formula <- formula(baseline)
  if (func == "lin") {
    new_formula <- update.formula(base_formula, . ~ . + climate)
  } else if (func == "quad") {
    new_formula <- update.formula(base_formula, . ~ . + poly(climate, 2))
  } else if (func == "cub") {
    new_formula <- update.formula(base_formula, . ~ . + poly(climate, 3))
  } else if (func == "inv") {
    new_formula <- update.formula(base_formula, . ~ . + I(1 / climate))
  } else if (func == "log") {
    new_formula <- update.formula(base_formula, . ~ . + log(climate))
  } else {
    new_formula <- update.formula(base_formula, . ~ . + climate)
  }

  ## Build model call: replace formula and data in baseline's call
  model_call         <- baseline$call
  model_call$formula <- new_formula
  model_call$data    <- quote(bio_data)

  ## Delegate to run_weightwin
  run_weightwin(
    n                = n,
    range            = new_range,
    bio_data         = bio_df,
    climate_data     = climate_df,
    basemodel        = model_call,
    cdate            = "Date",
    bdate            = "Date",
    xvar             = xvar_name,
    par              = par,
    type             = type,
    refday           = refday_str,
    weightfunc       = weightfunc,
    method           = method,
    control          = control,
    plot_every       = NULL,
    cinterval        = cinterval,
    .basemodelIsCall = TRUE
  )
}

#' Old weightwin syntax with new internals.
#'
#' @param n 
#' @param xvar 
#' @param cdate 
#' @param bdate 
#' @param baseline 
#' @param range 
#' @param k 
#' @param func 
#' @param type 
#' @param refday 
#' @param nrandom 
#' @param centre 
#' @param weightfunc 
#' @param cinterval 
#' @param cmissing 
#' @param cohort 
#' @param spatial 
#' @param par 
#' @param control 
#' @param method 
#' @param cutoff.day 
#' @param cutoff.month 
#' @param furthest 
#' @param closest 
#' @param grad 
#'
#' @returns
#' @export
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

#' Plot deltaAICc of models
#' 
#'Create a colour plot of model deltaAICc values.
#'@param dataset A dataframe containing information on all fitted climate 
#' windows. Output from \code{\link{slidingwin}}.
#'@param plotall Used in conjunction with function \code{\link{plotall}}. 
#' Should not be changed manually.
#'@param plotallenv Used in conjunction with function \code{\link{plotall}}.
#' Should not be changed manually.
#'@param arrow TRUE or FALSE. Add arrows to plots to pinpoint best window.
#'@param ThreeD TRUE or FALSE. Generate a 3-dimensional plot of the deltaAICc landscape.
#'@return Returns a colour plot of model deltaAICc values (larger negative
#' values indicate stronger models). DeltaAICc is the difference between AICc
#' of each climate window and a null model.
#'@author Liam D. Bailey and Martijn van de Pol
#'@import ggplot2
#'@importFrom grDevices colorRampPalette
#'@export

plotdelta <- function(dataset, arrow = FALSE, plotall = FALSE, plotallenv, ThreeD = FALSE){
  
  if(ThreeD == TRUE){
    
    stop("3D plotting temporarily disabled due to issues with the rgl package.")
    
    #Matrix_3d <- matrix(nrow = max(dataset$WindowOpen), ncol = max(dataset$WindowOpen), data = 0)
    #for(i in 1:nrow(dataset)){
    
    #  Matrix_3d[dataset$WindowOpen[i], dataset$WindowClose[i]] <- dataset$deltaAICc[i]
    
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
    #rgl.surface(x, y, matrix(1, nrow(z), ncol(z)), color = "grey", alpha = 0.5, back = "fill")
    
  } else {
    
    with(dataset, {
      if(arrow == FALSE){
        ARR <-   ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = deltaAICc))+
          geom_tile(aes(fill = deltaAICc))+
          scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "")+
          theme_climwin()+
          theme(legend.position = c(0.75, 0.3))+
          ggtitle(expression(paste(Delta, "AICc (compared to null model)")))+
          ylab("Window open")+
          xlab("Window close")
        if(plotall == TRUE){
          plotallenv$delta <- ARR
        } else {
          ARR
        }
      } else if(arrow == TRUE){
        
        CIRC <- circle(centre = c(dataset$WindowClose[1], dataset$WindowOpen[1]), diameter = 5, npoints = 100)
        colnames(CIRC) <- c("WindowClose", "WindowOpen")
        CIRC$deltaAICc <- 0
        
        ARR <-   ggplot(dataset, aes(x = WindowClose, y = WindowOpen, z = deltaAICc))+
          geom_tile(aes(fill = deltaAICc))+
          scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "")+
          theme_climwin()+
          theme(legend.position = c(0.75, 0.3))+
          ggtitle(expression(paste(Delta, "AICc (compared to null model)")))+
          ylab("Window open") +
          xlab("Window close") +
          geom_path(data = CIRC, aes(x = WindowClose, y = WindowOpen), size = 1.2, colour = "black")+
          geom_segment(aes(x = WindowClose[1], y = 0, xend = WindowClose[1], yend = (WindowOpen[1] - 2.5)), 
                       size = 1, linetype = "dashed") +
          geom_segment(aes(x = 0, y = WindowOpen[1], xend = (WindowClose[1] - 2.5), yend = WindowOpen[1]),
                       size = 1, linetype = "dashed")
        
        if(plotall == TRUE){
          plotallenv$delta <- ARR
        } else {
          ARR
        }
      }
      
    }
    )
  }
}