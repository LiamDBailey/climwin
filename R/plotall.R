#'Visualise climate window data
#'
#'Creates a panel of plots to help visualise climate window data.
#'@param dataset A dataframe containing information on all fitted climate 
#'  windows. Output from \code{\link{slidingwin}}.
#'@param datasetrand A dataframe containing information on all fitted climate 
#'  windows using randomised data. Output from \code{\link{randwin}}.
#'@param bestmodel A model object. The strongest climate window model. Returned 
#'  from \code{\link{singlewin}} or \code{\link{slidingwin}}.
#'@param bestmodeldata A dataframe containing the biological and climate data
#'  used to fit the strongest climate window model. Output from
#'  \code{\link{singlewin}} or \code{\link{slidingwin}}.
#'@param cw1,cw2,cw3 Cumulative weight levels used to visualise model weight 
#'  distribution. See \code{\link{plotweights}} for more detail.
#'@param title Title of the plot panel.
#'@param arrow TRUE or FALSE. Add arrows to plots to pinpoint best window.
#'@param verbose TRUE or FALSE. Should messages and warnings be printed while running
#'function? Default: TRUE
#'@return Will return a panel of 6-8 plots:
#'  
#'  \itemize{
#'  \item DeltaAICc: A colour plot of model deltaAICc values (larger
#'  negative values indicate stronger models). DeltaAICc is the difference
#'  between AICc of each climate window model and the baseline model containing
#'  no climate data.
#'  
#'  \item Model weight: A plot showing the distribution of cumulative
#'  model weights. Gradient levels determined by parameters cw1, cw2 and cw3.
#'  Darker areas have a higher chance of containing the best climate window.
#'  Also returns the percentage of models within the 95% confidence set (C).
#'  
#'  \item Model betas: A colour plot of model beta estimates. Where applicable,
#'  2nd order coefficients (quadratic) and 3rd order coefficients (cubic) will
#'  be plotted separately.
#'  
#'  \item Histogram(s): If datasetrand is provided, plotall will return a 
#'  histogram showing the deltaAICc of randomised data. 
#'  This can help determine the likelihood of obtaining a deltaAICc value 
#'  for a fitted climate window model at random. plotall will also use
#'  \code{\link{pvalue}} to return values of Pc and PdeltaAICc.
#'  
#'  \item Boxplots: Two boxplots showing the start and end time for a 
#'  subset of best climate windows. Best climate windows make up the
#'  cumulative model weight equivalent to the largest value of cw1, cw2 and cw3.
#'  Values above boxplots represent the median values.
#'  
#'  \item Best Model: If bestmodel and bestmodeldata are provided, plotall will 
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
#'single <- singlewin(xvar = list(Temp = MassClimate$Temp), 
#'                    cdate = MassClimate$Date, bdate = Mass$Date, 
#'                    baseline = lm(Mass ~ 1, data = Mass), 
#'                    range = c(72, 15), 
#'                    stat = "mean", func = "lin", 
#'                    type = "absolute", refday = c(20, 5), 
#'                    cmissing = FALSE, cinterval = "day")
#'            
#'plotall(dataset = MassOutput, bestmodel = single$BestModel, 
#'        bestmodeldata = single$BestModelData,
#'        cw1 = 0.95, cw2 = 0.5, cw3 = 0.25, title = "Mass")
#'         
#'          
#'@import gridExtra
#'@import ggplot2
#'@export

plotall <- function(dataset, datasetrand = NULL,
                    bestmodel = NULL, bestmodeldata = NULL,
                    cw1 = 0.95, cw2 = 0.5, cw3 = 0.25,
                    title = NULL, arrow = FALSE,
                    verbose = TRUE){

    if (class(bestmodel)[length(class(bestmodel))] == "coxph" | class(bestmodel)[1] == "lme") {
      bestmodel = NULL
      if (verbose){
        warning("plotall cannot yet plot the best model panel for a coxph or nlme models") 
      }
    }
  
  a       <- c(cw1, cw2, cw3)
  b       <- a[order (-a)]
  cwa     <- b[1]
  cwb     <- b[2]
  cwc     <- b[3]
  plotenv <- environment()
  
  if(length(levels(dataset$Custom.mod)) == 1){
    
    plotdelta(dataset = dataset, arrow = arrow, plotall = TRUE, plotallenv = plotenv)
    
    plotweights(dataset = dataset, cw1 = cwa, cw2 = cwb, cw3 = cwc, arrow = arrow, plotall = TRUE, plotallenv = plotenv)
    
    window <- plotwin(dataset = dataset, cw = cwa)
    
    message("Plot of model coefficients and best model output is unavailable when using custom models.")
    
    gridExtra::grid.arrange(plotenv$delta, plotenv$cw, window, nrow = 1, ncol = 3, top = paste(title))
    
  } else {
  
  plotbetas(dataset = dataset, arrow = arrow, plotall = TRUE, plotallenv = plotenv)
  
  plotdelta(dataset = dataset, arrow = arrow, plotall = TRUE, plotallenv = plotenv)
  
  plotweights(dataset = dataset, cw1 = cwa, cw2 = cwb, cw3 = cwc, arrow = arrow, plotall = TRUE, plotallenv = plotenv)
  
  window <- plotwin(dataset = dataset, cw = cwa)

  if(!is.null(datasetrand)){
    hist   <- plothist(dataset = dataset, datasetrand = datasetrand, verbose = verbose)    
    
  if(is.null(bestmodel) == FALSE && is.null(bestmodeldata) == FALSE){
    
  best  <- plotbest(dataset = dataset, bestmodel = bestmodel, bestmodeldata = bestmodeldata)
  
  if (dataset$Function[1] == "lin"){
    gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, hist, window, best, nrow = 2, ncol = 3, top = paste(title))
  } else if (dataset$Function[1] == "quad"){
    gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, plotenv$beta2, hist, window, best, nrow = 2, ncol = 4, top = paste(title))
  } else if(dataset$Function[1] == "cub"){
    gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, plotenv$beta2, hist, window, best, plotenv$beta3, nrow = 2, ncol = 4, top = paste(title))
  } else if(dataset$Function[1] == "centre"){
    if(is.null(dataset$WithinGrpDev) == TRUE){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgmean, hist, window, best, nrow = 2, ncol = 4, top = paste(title))
    } else if(is.null(dataset$WithinGrpMean) == TRUE){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgdev, hist, window, best, nrow = 2, ncol = 4, top = paste(title))
    } else {
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgmean, plotenv$wgdev, hist, window, best, nrow = 2, ncol = 4, top = paste(title))
    }
  } else {
    gridExtra::grid.arrange(plotenv$beta, plotenv$delta, plotenv$cw, hist, window, best, nrow = 2, top = paste(title))
  }
  } else {
    
    if (dataset$Function[1] == "lin"){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, hist, window, nrow = 2, ncol = 3, top = paste(title))
    } else if (dataset$Function[1] == "quad"){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, plotenv$beta2, hist, window, nrow = 2, ncol = 4, top = paste(title))
    } else if(dataset$Function[1] == "cub"){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, plotenv$beta2, hist, window, plotenv$beta3, nrow = 2, ncol = 4, top = paste(title))
    } else if(dataset$Function[1] == "centre"){
      if(is.null(dataset$WithinGrpDev) == TRUE){
        gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgmean, hist, window, nrow = 2, ncol = 4, top = paste(title))
      } else if(is.null(dataset$WithinGrpMean) == TRUE){
        gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgdev, hist, window, nrow = 2, ncol = 4, top = paste(title))
      } else {
        gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgmean, plotenv$wgdev, hist, window, nrow = 2, ncol = 4, top = paste(title))
      }
    } else {
      gridExtra::grid.arrange(plotenv$beta, plotenv$delta, plotenv$cw, hist, window, nrow = 2, top = paste(title))
    } 
  }
} else {
  
  if(!is.null(bestmodel) && !is.null(bestmodeldata)){
    best  <- plotbest(dataset = dataset, bestmodel = bestmodel, bestmodeldata = bestmodeldata)
    
    if (dataset$Function[1] == "lin"){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, window, best, nrow = 2, ncol = 3, top = paste(title))
    } else if (dataset$Function[1] == "quad"){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, plotenv$beta2, window, best, nrow = 2, ncol = 4, top = paste(title))
    } else if(dataset$Function[1] == "cub"){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, plotenv$beta2, window, best, plotenv$beta3, nrow = 2, ncol = 4, top = paste(title))
    } else if(dataset$Function[1] == "centre"){
      if(is.null(dataset$WithinGrpDev) == TRUE){
        gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgmean, window, best, nrow = 2, ncol = 4, top = paste(title))
      } else if(is.null(dataset$WithinGrpMean) == TRUE){
        gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgdev, window, best, nrow = 2, ncol = 4, top = paste(title))
      } else {
        gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgmean, plotenv$wgdev, window, best, nrow = 2, ncol = 4, top = paste(title))
      }
    } else {
      gridExtra::grid.arrange(plotenv$beta, plotenv$delta, plotenv$cw, window, best, nrow = 2, top = paste(title))
    }
  } else {
    
    if (dataset$Function[1] == "lin"){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, window, nrow = 2, ncol = 3, top = paste(title))
    } else if (dataset$Function[1] == "quad"){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, plotenv$beta2, window, nrow = 2, ncol = 4, top = paste(title))
    } else if(dataset$Function[1] == "cub"){
      gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$beta, plotenv$beta2, window, plotenv$beta3, nrow = 2, ncol = 4, top = paste(title))
    } else if(dataset$Function[1] == "centre"){
      if(is.null(dataset$WithinGrpDev) == TRUE){
        gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgmean, window, nrow = 2, ncol = 4, top = paste(title))
      } else if(is.null(dataset$WithinGrpMean) == TRUE){
        gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgdev, window, nrow = 2, ncol = 4, top = paste(title))
      } else {
        gridExtra::grid.arrange(plotenv$delta, plotenv$cw, plotenv$wgmean, plotenv$wgdev, window, nrow = 2, ncol = 4, top = paste(title))
      }
    } else {
      gridExtra::grid.arrange(plotenv$beta, plotenv$delta, plotenv$cw, window, nrow = 2, top = paste(title))
    } 
  } 
}
  }
}