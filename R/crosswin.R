#'Test the correlation between two climate variables
#'
#'Test the correlation between two climate variables across all considered climate
#'windows.
#'@param Xvar The first climate variable of interest. Please specify the parent 
#'  environment and variable name (e.g. Climate$Temp).
#'@param Xvar2 The second climate variable of interest. Please specify the parent 
#'  environment and variable name (e.g. Climate$Temp).
#'@param CDate The climate date variable (dd/mm/yyyy). Please specify the parent
#'  environment and variable name (e.g. Climate$Date).
#'@param BDate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param furthest The furthest number of time intervals (set by CINTERVAL) back
#'  from the cutoff date or biological record that you want to include in your
#'  climate window search.
#'@param closest The closest number of time intervals (set by CINTERVAL) back 
#'  from the cutoff date or biological record that you want to include in your 
#'  climate window search.
#'@param STAT The aggregate statistic used to analyse the climate data. Can 
#'  currently use basic R statistics (e.g. mean, min), as well as slope. 
#'  Additional aggregate statistics can be created using the format function(x) 
#'  (...). See FUN in \code{\link{apply}} for more detail.
#'@param FIXED TRUE or FALSE, whether you wish the climate window to be variable
#'  (i.e. the number of days before each biological record is measured) or fixed
#'  (i.e. number of days before a set point in time).
#'@param cutoff.day,cutoff.month If FIXED is TRUE, the day and month of the year
#'  from which the fixed window analysis will start.
#'@param CMISSING TRUE or FALSE, determines what should be done if there are 
#'  missing climate data. If FALSE, the function will not run if missing climate
#'  data is encountered. If TRUE, any records affected by missing climate data 
#'  will be removed from climate window analysis.
#'@param CINTERVAL The resolution at which climate window analysis will be 
#'  conducted. May be days ("D"), weeks ("W"), or months ("M"). Note the units 
#'  of parameters 'furthest' and 'closest' will differ depending on the choice 
#'  of CINTERVAL.
#'@return Will return a dataframe containing the correlation between the two
#'  climate variables.
#'@author Liam D. Bailey and Martijn van de Pol
#' @examples
#' \dontrun{
#' # Test correlation between temperature and rainfall in the MassClimate dataset.
#' 
#' data(Mass)
#' data(MassClimate)
#' 
#' cross <- crosswin(Xvar = MassClimate$Temp, Xvar2 = MassClimate$Rain, 
#'                   CDate = MassClimate$Date, BDate = Mass$Date, 
#'                   furthest = 365, closest = 0,
#'                   STAT = "mean", FIXED = FALSE,
#'                   CMISSING = FALSE, CINTERVAL = "D")
#'                   
#' # View the output
#' head(cross)
#' 
#' # Plot the output
#' plotcor(cross, TYPE = "C")
#' 
#' }
#' 
#'@export

#LAST EDITED: 18/02/2015
#EDITED BY: LIAM
#NOTES: Tidy up code

crosswin <- function(Xvar, Xvar2, CDate, BDate, furthest, closest, 
                  STAT, FIXED, cutoff.day, cutoff.month,
                  CINTERVAL = "D", CMISSING = FALSE){
  
  print("Initialising, please wait...")
  duration <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2 
  cont     <- DateConverter(BDate = BDate, CDate = CDate, Xvar = Xvar, Xvar2 = Xvar2, 
                            CINTERVAL = CINTERVAL, FIXED = FIXED, 
                            cutoff.day = cutoff.day, cutoff.month = cutoff.month, cross = TRUE)   # create new climate dataframe with continuous daynumbers, leap days are not a problem
  MODNO    <- 1  #Create a model number variable that will count up during the loop#
  MODLIST  <- list()   # dataframes to store ouput
  CMatrix1 <- matrix(ncol = (duration), nrow = length(BDate))  # matrix that stores the weather data for variable or fixed windows
  CMatrix2 <- matrix(ncol = (duration), nrow = length(BDate))  # matrix that stores the weather data for variable or fixed windows
  
  for (i in 1:length(BDate)){
    for (j in closest:furthest){
      k <- j - closest + 1
      CMatrix1[i, k] <- cont$Xvar[which(cont$CIntNo == cont$BIntNo[i] - j)]  #Create a matrix which contains the climate data from furthest to furthest from each biological record#
      CMatrix2[i, k] <- cont$Xvar2[match(cont$BIntNo[i] - j,cont$CIntNo)]
    }
  }
  
  if (CMISSING == FALSE && length(which(is.na(CMatrix1))) > 0){
    .GlobalEnv$Missing <- as.Date(cont$CIntNo[is.na(cont$Xvar)], origin = min(as.Date(CDate, format = "%d/%m/%Y")) - 1)
    stop(c("Climate data Xvar should not contain NA values: ", length(.GlobalEnv$Missing),
           " NA value(s) found. Please add missing climate data or set CMISSING=TRUE.
           See object Missing for all missing climate data"))
  }  
  
  if (CMISSING == FALSE && length(which(is.na(CMatrix2))) > 0){
    .GlobalEnv$Missing2 <- as.Date(cont$CIntNo[is.na(cont$Xvar2)], origin = min(as.Date(CDate, format = "%d/%m/%Y")) - 1)
    stop(c("Climate data Xvar2 should not contain NA values: ", length(.GlobalEnv$Missing),
           " NA value(s) found. Please add missing climate data or set CMISSING=TRUE.
           See object Missing2 for all missing climate data"))
  }
  
  if (CMISSING == TRUE){ 
    CMatrix1  <- CMatrix1[complete.cases(CMatrix1), ]
    CMatrix2  <- CMatrix2[complete.cases(CMatrix2), ]
  }
  
  temporary1 <- matrix(ncol = 1, nrow = nrow(CMatrix1), 1)
  temporary2 <- matrix(ncol = 1, nrow = nrow(CMatrix2), 1)
  
  pb <- txtProgressBar(min = 0, max = MaxMODNO, style = 3, char = "|")
  
  for (m in closest:furthest){
    for (n in 1:duration){
      if ( (m - n) >= (closest - 1)){  # do not use windows that overshoot the closest possible day in window   
        if (STAT != "slope" || n > 1){
          windowopen  <- m - closest + 1
          windowclose <- windowopen - n + 1
          if (STAT == "slope"){ 
            time <- seq(1, n, 1)
            temporary1 <- apply(CMatrix1[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
            temporary2 <- apply(CMatrix2[, windowclose:windowopen], 1, FUN = function(x) coef(lm(x ~ time))[2])
          } else { 
            if (n == 1) {
              temporary1 <- CMatrix1[, windowclose:windowopen]
              temporary2 <- CMatrix2[, windowclose:windowopen]
            } else {
              temporary1 <- apply(CMatrix1[, windowclose:windowopen], 1, FUN = STAT) 
              temporary2 <- apply(CMatrix2[, windowclose:windowopen], 1, FUN = STAT)
            }
          }
          # Run the model
          modeloutput <- cor(temporary1, temporary2)
          # Add model parameters to list#
          MODLIST$cor[[MODNO]]         <- modeloutput
          MODLIST$WindowOpen[[MODNO]]  <- m
          MODLIST$WindowClose[[MODNO]] <- m - n + 1
          MODNO                        <- MODNO + 1 # Increase ModNo#
        }
      }
    }  
    #Fill progress bar
    setTxtProgressBar(pb, MODNO - 1)
  }
  MODLIST$furthest    <- furthest
  MODLIST$closest     <- closest
  MODLIST$Statistics  <- STAT
  MODLIST$FIXED       <- FIXED
  
  if (FIXED == TRUE){
    MODLIST$cutoff.day   <- cutoff.day
    MODLIST$cutoff.month <- cutoff.month
  }
  return(as.data.frame(MODLIST))
}