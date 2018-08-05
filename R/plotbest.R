#'Visualise the best climate window
#'
#'Create a scatterplot showing the fit of the best climate window model through
#'the biological data.
#'@param dataset A dataframe containing information on all fitted climate 
#'  windows. Output from \code{\link{slidingwin}}.
#'@param bestmodel A model object. The strongest climate window model. Output 
#'  from \code{\link{singlewin}} or \code{\link{slidingwin}}.
#'@param bestmodeldata A dataframe with the data used to 
#'  fit the strongest climate window model. Output from \code{\link{singlewin}} 
#'  or \code{\link{slidingwin}}.
#'@return Returns a scatterplot with a fitted line to show the fit of the best 
#'  model through the data.
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'# Visualise the best climate window from the datasets Mass and MassClimate
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
#'plotbest(dataset = MassOutput, bestmodel = single$BestModel,
#'         bestmodeldata = single$BestModelData)
#'              
#'@import ggplot2
#'@export

plotbest <- function(dataset, bestmodel, bestmodeldata){
  
  #Change the name of the first column in the best model data as "Yvar".
  names(bestmodeldata)[1] <- "Yvar"
  
  #If climate data has been scaled, change the name back to just 'climate'
  names(bestmodeldata)[2] <- ifelse(names(bestmodeldata)[2] == "scale(climate)",
                                    "climate", names(bestmodeldata)[2])
  
  #If you are using a nlme model, it won't produce a plot.
  if(class(bestmodel)[length(class(bestmodel))] == "coxph" | attr(bestmodel, "class")[1] == "lme"){
    
    stop("plotbest is currently not available with nlme of coxph models. Consider plotting best model manually.")
    
  }
  
  bestmodeldata$model_pred <- predict(bestmodel, re.form = NA, type = "response")
  
  xlab_name = "Climate variable"
  
  if(dataset$Function[1] == "log"){
    
    names(bestmodeldata)[(ncol(bestmodeldata) - 1)] <- "climate"
    
    xlab_name = "Log of climate variable"
    
  }
  
  if(dataset$Function[1] == "inv"){
    
    names(bestmodeldata)[ncol(bestmodeldata) - 1]   <- "climate"
    class(bestmodeldata[, ncol(bestmodeldata) - 1]) <- class(bestmodeldata[, ncol(bestmodeldata) - 1])[-match("AsIs", class(bestmodeldata[, ncol(bestmodeldata) - 1]))]
    
    xlab_name = "Inverse of climate variable"
    
  }
  
  #If data has been within group centred...
  if(!is.null(bestmodeldata$WGdev)){
    
    #With bestmodel data (to removed the global undefined variable error)
    return(with(bestmodeldata, {
      
      #Produce a plot
      ggplot(bestmodeldata, aes(y = Yvar, x = climate))+
        geom_point(size = 3, alpha = 0.5, shape = 21, fill = "dark grey")+
        geom_abline(intercept = coef(bestmodel)[1], slope = coef(bestmodel)[2], colour = "red")+
        geom_abline(intercept = coef(bestmodel)[1], slope = coef(bestmodel)[3], colour = "blue")+
        theme_climwin() +
        ggtitle("Output of best model") +
        ylab("Biological variable")
      
    }))
  
  #Otherwise (if they don't calculate within group deviance...)    
  } else {
    
    with(bestmodeldata, {
      
      return(ggplot(bestmodeldata)+
        geom_point(aes(x = climate, y = Yvar), size = 3, alpha = 0.5, shape = 21, fill = "dark grey")+
        geom_line(aes(x = climate, y = model_pred), size = 1)+
        theme_climwin()+
        labs(title = "Output of best model", x = xlab_name, y = "Biological response"))
        
    })

  }
}