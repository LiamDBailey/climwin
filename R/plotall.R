#'Visualise climate window data
#'
#'Creates a panel of plots to help visualise climate window data.
#'@param Dataset A dataframe containing information on all fitted climate 
#'  windows. Output from \code{\link{climatewin}}.
#'@param DatasetRand A dataframe containing information on all fitted climate 
#'  windows using randomised data. Output from \code{\link{randwin}}.
#'@param BestModel A model object. The strongest climate window model. Returned 
#'  from \code{\link{singlewin}} or \code{\link{climatewin}}.
#'@param BestModelData A dataframe containing the biological and climate data
#'  used to fit the strongest climate window model. Output from
#'  \code{\link{singlewin}} or \code{\link{climatewin}}.
#'@param CW1,CW2,CW3 Cumulative weight levels used to visualise model weight 
#'  distribution. See \code{\link{plotweights}} for more detail.
#'@param HISTQ If DatasetRand is provided. The quantile of the randomised data 
#'  to be compared with non-randomised data. Can be used to determine the 
#'  likelihood of finding a climate window model of a given AICc value by
#'  chance.
#'@param Title Main title of the plot panel.
#'@return Will return a panel of 6-8 plots:
#'  
#'  \itemize{ \item DeltaAICc: A colour plot of model deltaAICc values (larger
#'  negative values indicate stronger models). DeltaAICc is the difference
#'  between AICc of each climate window model and the baseline model.
#'  
#'  \item Model weight: A colour plot showing the distribution of cumulative
#'  model weights. Gradient levels determined by parameters CW1,CW2 and CW3.
#'  
#'  \item Model betas: A colour plot of model beta estimates. Where applicable,
#'  2nd order coefficients (quadratic) and 3rd order coefficients (cubic) will
#'  be plotted seperately.
#'  
#'  \item Histogram(s): If DatasetRand is provided, plotall will create two 
#'  stacked histograms to compare the deltaAICc of non-randomised and randomised
#'  data. This can help determine the likelihood of obtaining a deltaAICc value 
#'  for a fitted climate window model at random. Without DatasetRand, plotall
#'  will create a single histogram of deltaAICc values for all fitted climate 
#'  windows.
#'  
#'  \item Boxplots: Two boxplots showing the opening and closing day for a 
#'  subset of best climate windows. Best climate windows make up the
#'  cumulative model weight equivalent to the largest value of CW1, CW2 and CW3.
#'  
#'  \item Best Model: If BestModel and BestModelData are provided, plotall will 
#'  create a scatterplot to show the fit of the best model through the data. }
#'  
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'
#'# Visualise a fixed climate window generated for dataframes Mass and MassClimate
#'
#'data(MassOutput)
#'data(Mass)
#'data(MassClimate)
#'
#'single <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
#'                    baseline = lm(Mass$Mass ~ 1), furthest = 72, closest = 15, 
#'                    STAT = "mean", FUNC = "L", 
#'                    FIXED = TRUE, cutoff.day = 20, cutoff.month = 5, 
#'                    CMISSING = FALSE, CINTERVAL = "D")
#'            
#' plotall(Dataset = MassOutput, BestModel = single[[1]], 
#'         BestModelData = single[[2]],
#'         CW1 = 0.95, CW2 = 0.5, CW3 = 0.25, HISTQ = 0.99)
#'         
#'          
#'@import gridExtra
#'@import ggplot2
#'@export
 



#LAST EDITED: 18/02/2015
#EDITED BY: LIAM
#NOTES: Tidy code

#Distinguish between Q and C
plotall <- function(Dataset, DatasetRand = NULL,
                    BestModel = NULL, BestModelData = NULL,
                    CW1 = 0.95, CW2 = 0.5, CW3 = 0.25, HISTQ = 0.99,
                    Title = NULL){
  
  a       <- c(CW1, CW2, CW3)
  b       <- a[order (-a)]
  CWa     <- b[1]
  CWb     <- b[2]
  CWc     <- b[3]
  plotenv <- environment()
  
  plotbetas(Dataset = Dataset, plotall = TRUE, plotallenv = plotenv)
  
  DELTA  <- plotdelta(Dataset = Dataset)
  
  CW     <- plotweights(Dataset = Dataset, CW1 = CWa, CW2 = CWb, CW3 = CWc)
  
  WINDOW <- plotwin(Dataset = Dataset, CW = CWa)
  
  HIST   <- plothist(Dataset = Dataset, DatasetRand = DatasetRand, HISTQ = HISTQ)
  if(is.null(BestModel) == FALSE && is.null(BestModelData) == FALSE){
  BEST   <- plotbest(Dataset = Dataset, BestModel = BestModel, BestModelData = BestModelData)
  
  if (Dataset$Function[1] == "L"){
    gridExtra::grid.arrange(DELTA, CW, plotenv$BETA, HIST, WINDOW, BEST, nrow = 2, ncol = 3, top = paste(Title))
  } else if (Dataset$Function[1] == "Q"){
    gridExtra::grid.arrange(DELTA, CW, plotenv$BETA, plotenv$BETA2, HIST, WINDOW, BEST, nrow = 2, ncol = 4, top = paste(Title))
  } else if(Dataset$Function[1] == "C"){
    gridExtra::grid.arrange(DELTA, CW, plotenv$BETA, plotenv$BETA2, HIST, WINDOW, BEST, plotenv$BETA3, nrow = 2, ncol = 4, top = paste(Title))
  } else {
    gridExtra::grid.arrange(plotenv$BETA, DELTA, CW, HIST, WINDOW, BEST, nrow = 2, top = paste(Title))
  }
  } else {
    if (Dataset$Function[1] == "L"){
      gridExtra::grid.arrange(DELTA, CW, plotenv$BETA, HIST, WINDOW, nrow = 2, ncol = 3, top = paste(Title))
    } else if (Dataset$Function[1] == "Q"){
      gridExtra::grid.arrange(DELTA, CW, plotenv$BETA, plotenv$BETA2, HIST, WINDOW, nrow = 2, ncol = 4, top = paste(Title))
    } else if(Dataset$Function[1] == "C"){
      gridExtra::grid.arrange(DELTA, CW, plotenv$BETA, plotenv$BETA2, HIST, WINDOW, plotenv$BETA3, nrow = 2, ncol = 4, top = paste(Title))
    } else {
      gridExtra::grid.arrange(plotenv$BETA, DELTA, CW, HIST, WINDOW, nrow = 2, top = paste(Title))
    } 
  }
}