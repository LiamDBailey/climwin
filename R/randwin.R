#'Climate window analysis for randomised data
#'
#'Will randomise biological data and carry out a climate window analysis. Used
#'to help determine the chance of obtaining an observed result at random.
#'@param repeats The number of times that data will be randomised and analysed 
#'  for climate windows
#'@param Xvar The climate variable of interest. Please specify the parent 
#'  environment and variable (e.g. Climate$Temp)
#'@param CDate The climate date variable (dd/mm/yyyy). Please specify the parent
#'  environment and variable name (e.g. Climate$Date).
#'@param BDate The biological date variable (dd/mm/yyyy). Please specify the 
#'  parent environment and variable name (e.g. Biol$Date).
#'@param baseline The baseline model structure used for testing correlation. 
#'  Currently known to support lm, glm, lmer and glmer objects.
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
#'@param FUNC The function of the climate variable.
#'@param FIXED TRUE or FALSE, whether you wish the climate window to be variable
#'  (i.e. the number of days before a biological record is measured) or fixed 
#'  (i.e. number of days before a set point in time).
#'@param cutoff.day,cutoff.month If FIXED is TRUE, the day and month of the year
#'  from which the fixed window analysis will be set.
#'@param CMISSING TRUE or FALSE, determines what should be done if there is 
#'  missing climate data. If false, the function will not run if missing climate
#'  data is encountered. If true, any records affected by missing climate data 
#'  will be deleted from climate window analysis.
#'@param CINTERVAL The resolution at which climate window analysis can be 
#'  conducted. Can be days ("D"), weeks ("W"), or months ("M"). Note the units 
#'  of parameters 'furthest' and 'closest' will differ depending on the chouice 
#'  of CINTERVAL.
#'@return Will return a dataframe containing information on all fitted climate
#'  windows. See \code{\link{MassRand}} as an example.
#'@author Liam D. Bailey and Martijn van de Pol
#' @examples
#' \dontrun{
#' # Test climate windows for random data using Mass dataset
#' 
#' data(Mass)
#' data(MassClimate)
#' 
#' # Randomise data twice
#' # Note all other parameters are fitted in the same way as the ClimateWindow function.
#' 
#' rand <- randwin(repeats = 2, Xvar = MassClimate$Temp, 
#'                 CDate = MassClimate$Date, BDate = Mass$Date,
#'                 baseline = lm(Mass ~ 1, data = Mass), furthest = 100, closest = 0,
#'                 STAT = "mean", FUNC = "L", FIXED = TRUE, 
#'                 cutoff.day = 20, cutoff.month = 5,
#'                 CMISSING = FALSE, CINTERVAL = "D")
#'                 
#' # View output #
#' 
#' head(rand)    
#'        }
#'
#'@export

#LAST EDITED: 18/02/2015
#EDITED BY: LIAM
#NOTES: Renamed to be RandomiSed rather than RandomiZed. Help keep American/UK spelling standardised.
#TIDY CODE

randwin <- function(repeats = 1, Xvar, CDate, BDate, baseline, 
                    furthest, closest, STAT,  
                    FUNC, FIXED, cutoff.day, cutoff.month,
                    CMISSING = FALSE, CINTERVAL = "D"){
  for (r in 1:repeats){
    print (c("randomization number ", r))
    BDateNew        <- sample(BDate)
    WindowOutputRep <- climatewin(Xvar = Xvar, CDate = CDate, BDate = BDateNew, 
                                  baseline = baseline, furthest = furthest,
                                  closest = closest, STAT = STAT, 
                                  FUNC = FUNC, FIXED = FIXED,
                                  cutoff.day = cutoff.day, cutoff.month = cutoff.month,
                                  nrandom = repeats, CMISSING = CMISSING, CINTERVAL = CINTERVAL)
    WindowOutputRep$Repeat <- r
    if(r == 1){ 
      WindowOutputRand <- WindowOutputRep 
    } else { 
      WindowOutputRand <- rbind(WindowOutputRand, WindowOutputRep)
    }
    rm(WindowOutputRep)
  }
  return(as.data.frame(WindowOutputRand))
} 