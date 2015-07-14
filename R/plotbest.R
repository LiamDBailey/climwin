#'Visualise the best climate window
#'
#'Create a scatterplot showing the fit of the best climate window model through
#'the biological data.
#'@param Dataset A dataframe containing information on all fitted climate 
#'  windows. Output from \code{\link{climatewin}}.
#'@param BestModel A model object. The strongest climate window model. Output 
#'  from \code{\link{singlewin}} or \code{\link{climatewin}}.
#'@param BestModelData A dataframe with the data used to 
#'  fit the strongest climate window model. Output from \code{\link{singlewin}} 
#'  or \code{\link{climatewin}}.
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
#'single <- singlewin(Xvar = MassClimate$Temp, Cdate = MassClimate$Date, Bdate = Mass$Date, 
#'                    baseline = lm(Mass$Mass ~ 1),furthest = 72, closest = 15, 
#'                    stat = "mean", func = "lin", 
#'                    type = "fixed", cutoff.day = 20, cutoff.month = 5, 
#'                    Cmissing = FALSE, Cinterval = "day")
#'            
#'plotbest(Dataset = MassOutput, BestModel = single$BestModel,
#'         BestModelData = single$BestModelData)
#'              
#'@import ggplot2
#'@export

#last edited 18/2/15 by LB tidy code

plotbest <- function(Dataset, BestModel, BestModelData){
  
names(BestModelData)[1] <- "Yvar"
    
if(is.null(BestModelData$WGdev) == FALSE){
  ggplot(BestModelData, aes(y = Yvar, x = climate))+
    geom_point(size = 1, alpha = 0.5)+
    geom_abline(intercept = coef(OffspringWin[[1]]$BestModel)[1], slope = coef(OffspringWin[[1]]$BestModel)[2], colour = "red")+
    geom_abline(intercept = coef(OffspringWin[[1]]$BestModel)[1], slope = coef(OffspringWin[[1]]$BestModel)[3], colour = "blue")
} else {
  if (Dataset$Function[1] == "log"){
    names(BestModelData)[(ncol(BestModelData)-1)] <- "climate"
  }
  if (Dataset$Function[1] == "inv"){
    names(BestModelData)[ncol(BestModelData)-1] <- "climate"
    class(BestModelData[, ncol(BestModelData)-1]) <- class(BestModelData[, ncol(BestModelData)-1])[-match("AsIs", class(BestModelData[, ncol(BestModelData)-1]))]
    #WHEN WE USE INVERSE FUNCTION 'climate' becomes class AsIs which the graphs can't deal with
    #With this class change, we turn the 'climate' value in to a basic numeric.
  }
  
  #TEST IF THERE ARE MODEL WEIGHTS
  if(is.null(weights(BestModel)) == TRUE || sum(weights(BestModel)) == nrow(BestModelData)){
    #TEST IF THERE ARE ADDITIONAL COVARIATES IN THE MODEL
    if(ncol(BestModelData) == 2 || Dataset$Function[1] == "quad" & ncol(BestModelData) == 3 || Dataset$Function[1] == "cub" & ncol(BestModelData) == 4){
      with(BestModelData, {
        ggplot(BestModelData, aes(x = climate, y = Yvar), environment = environment()) +
          geom_point(size = 1, alpha = 1) +
          geom_line(data = cbind(BestModelData, pred = predict(BestModel, type = "response", allow.new.levels = TRUE)), aes(y = pred)) +
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                plot.title = element_text(size = 18)) +
          ggtitle("Output of best model") +
          ylab("Biological variable") +    
          if (Dataset$Function[1] == "log"){
            xlab("Log of climate variable")
          } else if (Dataset$Function[1] == "inv"){
            xlab("Inverse of climate variable")
          } else {
            xlab("Climate variable")
          }
      }
      )       
    } else {
      col = 1
      if(Dataset$Function[1] == "quad"){
        col = 2
      } 
      if(Dataset$Function[1] == "cub"){
        col = 3
      }
      xval <- seq(from = min(BestModelData$climate), to = max(BestModelData$climate),
                  by = (max(BestModelData$climate) - min(BestModelData$climate)) / (nrow(BestModelData)))
      #When we have additional covariates, we need to integrate them in to the dataset for the predictions
      #However, we need to determine the mean (or reference category) for each of these variables
      newdat <- matrix(ncol = ncol(BestModelData) - col, nrow = nrow(BestModelData) + 1)
      #Create a matrix which has columns for all variables (bar Yvar because this will be calculated with predict)
      #nrow is 1 larger than predicted due to the length of xval
      newdat <- as.data.frame(newdat)
      newdat[, 1] <- xval
      #The first column of the matrix will always be the same as the xval
      for(cols in 2:(ncol(BestModelData) - col)){ #This will go through every column except for Yvar
        if(is.character(BestModelData[, cols]) == FALSE){ #If the variable is not categorical then take the mean
          newdat[, cols] <- mean(BestModelData[, cols])
        } else { #If it is categorical, simply take the first category
          newdat[, cols] = BestModelData[1, cols]          
        }
      }
      names(newdat) <- c("climate", names(BestModelData)[2:(ncol(BestModelData) - col)])  #Then change the names so they match what would be in the model
      pred <- predict(BestModel, newdata = newdat, type = "response", allow.new.levels = TRUE)
      with(BestModelData, {
        ggplot(BestModelData, aes(x = climate, y = Yvar), environment = environment()) +
          geom_point(size = 1, alpha = 1) +
          geom_line(data = newdat, aes(y = pred)) +
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                plot.title = element_text(size = 18)) +
          ggtitle("Output of best model") +
          ylab("Biological variable") +    
          if (Dataset$Function[1] == "log"){
            xlab("Log of climate variable")
          } else if (Dataset$Function[1] == "inv"){
            xlab("Inverse of climate variable")
          } else {
            xlab("Climate variable")
          }
      }
      )  
    }
  } else {
    if(ncol(BestModelData) == 3 || Dataset$Function[1] == "quad" & ncol(BestModelData) == 4 || Dataset$Function[1] == "cub" & ncol(BestModelData) == 5){ 
      if (Dataset$Function[1] == "log" || Dataset$Function[1] == "inv"){
        names(BestModelData)[ncol(BestModelData) - 1] <- "climate"  
      }
      with(BestModelData, {
        ggplot(BestModelData, aes(x = climate, y = Yvar), environment = environment()) +
          geom_point(size = 1, alpha = 1) +
          geom_line(data = cbind(BestModelData, pred = predict(BestModel, type = "response", allow.new.levels = TRUE)), aes(y = pred)) +
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                plot.title = element_text(size = 18)) +
          ggtitle("Output of best model") +
          ylab("Biological variable") +    
          if (Dataset$Function[1] == "log"){
            xlab("Log of climate variable")
          } else if (Dataset$Function[1] == "inv"){
            xlab("Inverse of climate variable")
          } else {
            xlab("Climate variable")
          }
      }
      )
    } else {
      col = 1
      if(Dataset$Function[1] == "quad"){
        col = 2
      } 
      if(Dataset$Function[1] == "cub"){
        col = 3
      }
      xval <- seq(from = min(BestModelData$climate), to = max(BestModelData$climate),
                  by = (max(BestModelData$climate) - min(BestModelData$climate)) / (nrow(BestModelData)))
      #When we have additional covariates, we need to integrate them in to the dataset for the predictions
      #However, we need to determine the mean (or reference category) for each of these variables
      newdat <- matrix(ncol = ncol(BestModelData) - col, nrow = nrow(BestModelData) + 1)
      #Create a matrix which has columns for all variables (bar Yvar because this will be calculated with predict)
      #nrow is 1 larger than predicted due to the length of xval
      newdat <- as.data.frame(newdat)
      newdat[, 1] <- xval
      #The first column of the matrix will always be the same as the xval
      for(cols in 2:(ncol(BestModelData) - col)){ #This will go through every column except for Yvar
        if(is.character(BestModelData[, cols]) == FALSE){ #If the variable is not categorical then take the mean
          newdat[, cols] <- mean(BestModelData[, cols])
        } else { #If it is categorical, simply take the first category
          newdat[, cols] = BestModelData[1, cols]          
        }
      }
      names(newdat) <- c("climate", names(BestModelData)[2:(ncol(BestModelData) - col)])  #Then change the names so they match what would be in the model
      pred <- predict(BestModel, newdata = newdat,  type = "response", allow.new.levels = TRUE)
      with(BestModelData, {
        ggplot(BestModelData, aes(x = climate, y = Yvar), environment = environment()) +
          geom_point(size = 1, alpha = 1) +
          geom_line(data = newdat, aes(y = pred)) +
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                plot.title = element_text(size = 18)) +
          ggtitle("Output of best model") +
          ylab("Biological variable") +    
          if (Dataset$Function[1] == "log"){
            xlab("Log of climate variable")
          } else if (Dataset$Function[1] == "inv"){
            xlab("Inverse of climate variable")
          } else {
            xlab("Climate variable")
          }
      }
      )
    }
  } 
}
  }