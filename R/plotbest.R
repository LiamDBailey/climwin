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
#'single <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
#'                    baseline = lm(Mass$Mass ~ 1),furthest = 72, closest = 15, 
#'                    STAT = "mean", FUNC = "L", 
#'                    FIXED = TRUE, cutoff.day = 20, cutoff.month = 5, 
#'                    CMISSING = FALSE, CINTERVAL = "D")
#'            
#'plotbest(Dataset = MassOutput, BestModel = single[[1]],
#'         BestModelData = single[[2]])
#'              
#'@import ggplot2
#'@export

#last edited 18/2/15 by LB tidy code

plotbest <- function(Dataset, BestModel, BestModelData){
  names(BestModelData)[1] <- "Yvar"
  
#  if (Dataset$Function[1] == "LOG" || Dataset$Function[1] == "I"){
#    names(BestModelData)[max(ncol(BestModelData))] <- "temporary"  
#  }
    
if(BestModel$weights == NULL){
  if(ncol(BestModelData) == 2 || Dataset$Function[1] == "Q" & ncol(BestModelData) == 3 || Dataset$Function[1] == "C" & ncol(BestModelData == 4)){
      with(BestModelData, {
        ggplot(BestModelData, aes(x = temporary, y = Yvar)) +
          geom_point(size = 1, alpha = 1) +
          geom_line(data = cbind(BestModelData, predELEV = predict(BestModel, type = "response")), aes(y = predELEV)) +
          theme_classic() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(size = 0.25, colour = "black"),
                plot.title = element_text(size = 18)) +
          ggtitle("Output of best model") +
          ylab("Biological variable") +    
          if (Dataset$Function[1] == "LOG"){
            xlab("Log of climate variable")
          } else if (Dataset$Function[1] == "I"){
            xlab("Inverse of climate variable")
          } else {
            xlab("Climate variable")
          }
      }
      )
    } else {
      col = 1
      if(Dataset$Function[1] == "Q"){
        col = 2
      }
      if(Dataset$Function[1] == "C"){
        col = 3
      }
      xval <- seq (from = min(BestModelData$temporary), to = max(BestModelData$temporary),
                    by = (max(BestModelData$temporary) - min(BestModelData$temporary)) / (nrow(BestModelData)))
      #When we have additional covariates, we need to integrate them in to the dataset for the predictions
      #However, we need to determine the mean (or reference category) for each of these variables
      newdat <- matrix(ncol = ncol(BestModelData) - 1, nrow = nrow(BestModelData) + 1)
      #Create a matrix which has columns for all variables (bar Yvar because this will be calculated with predict)
      #nrow is 1 larger than predicted due to the length of xval
      newdat[, 1] <- xval
      #The first column of the matrix will always be the same as the xval
      for(cols in 2:(ncol(BestModelData) - col)) #This will go through every column except for {
        if(is.factor(BestModelData[cols]) == FALSE){ #If the variable is not categorical than take the mean
          newdat[, cols] <- mean(BestModelData[cols])
        } else {
          
        }
           
      
      }
      newdat <- as.data.frame(newdat)
      names(newdat) <- names(BestModelData)
    }
  }
  
#  link <- family(BestModel)[2]
#  
#  if(length(attr(class(BestModel),"package")) > 0 && attr(class(BestModel),"package") == "lme4"){
#    int  <- fixef(BestModel)[1]
#    a    <- length(fixef(BestModel))
#    if (Dataset$Function[1] == "L" || Dataset$Function[1] == "LOG" || Dataset$Function[1] == "I"){
#      beta_l <- fixef(BestModel)[a]
#      beta_q <- 0
#      beta_c <- 0  
#    } else if (Dataset$Function[1] == "Q"){
#      beta_l <- fixef(BestModel)[a - 1]
#      beta_q <- fixef(BestModel)[a]  
#      beta_c <- 0 
#    } else if (Dataset$Function[1] == "C"){
#      beta_l <- fixef(BestModel)[a - 2]
#      beta_q <- fixef(BestModel)[a - 1] 
#      beta_c <- fixef(BestModel)[a] 
#    } else {
#      stop ("define FUNC to be L, Q,  C, LOG, or I")
#    }  
#  } else {
#  int  <- coef(BestModel)[1]
#  a    <- length(coef(BestModel))
#  if (Dataset$Function[1] == "L" || Dataset$Function[1] == "LOG" || Dataset$Function[1] == "I"){
#    beta_l <- coef(BestModel)[a]
#    beta_q <- 0
#    beta_c <- 0  
#  } else if (Dataset$Function[1] == "Q"){
#    beta_l <- coef(BestModel)[a - 1]
#    beta_q <- coef(BestModel)[a]  
#    beta_c <- 0 
#  } else if (Dataset$Function[1] == "C"){
#    beta_l <- coef(BestModel)[a - 2]
#    beta_q <- coef(BestModel)[a - 1] 
#    beta_c <- coef(BestModel)[a] 
#  } else {
#    stop ("define FUNC to be L, Q,  C, LOG, or I")
#  }
#  }
#  
#  if (link == "identity"){
#    yval <- data.frame(yval = (int+beta_l * xval + beta_q * xval * xval + beta_c * xval * xval * xval))
#  } else if (link == "log"){
#    yval <- data.frame(yval = (exp(int + beta_l * xval + beta_q * xval * xval + beta_c * xval * xval * xval)))
#  } else if (link == "logit"){
#    yval <- data.frame(yval = (1 / (1 + exp(-1 * (int + beta_l * xval + beta_q * xval * xval + beta_c * xval * xval * xval)))))
#  } else {
#    print("Sorry, scatter plot with model fit currently only works for identity, log and logit link functions")
#  }
#  
#  LineData           <- cbind(xval, yval)
#  colnames(LineData) <- c("temporary", "Yvar")
#  
#  with(BestModelData, {
#    ggplot(BestModelData, aes(x = temporary, y = Yvar)) +
#    geom_point(size = 1, alpha = 1) +
#    geom_line(data = LineData, aes(y = Yvar)) +
#    theme_classic() +
#    theme(panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),
#          axis.line = element_line(size = 0.25, colour = "black"),
#          plot.title = element_text(size = 18)) +
#    ggtitle("Output of best model") +
#    ylab("Biological variable") +    
#  if (Dataset$Function[1] == "LOG"){
#    xlab("Log of climate variable")
#  } else if (Dataset$Function[1] == "I"){
#    xlab("Inverse of climate variable")
#  } else {
#    xlab("Climate variable")
#      }
#    }
#  )
#}