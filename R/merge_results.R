#'Merge two slidingwin analyses.
#'
#'Merges outputs of two separate slidingwin analyses.
#'@param dataset1,dataset2 The slidingwin outputs to be merged. Note that all elements 
#'(i.e. Dataset, BestModel, BestModelData) will be merged and do not need to be specified.
#'@return A list object, identical to that produced by \code{\link{slidingwin}},
#'containing all records from both outputs.
#'@author Liam D. Bailey and Martijn van de Pol
#'@examples
#'
#'#Simple test example
#'#Create data from a subset of our test dataset
#'#Just use two years
#'biol_data <- Mass[1:2, ]
#'clim_data <- MassClimate[grep(pattern = "1979|1986", x = MassClimate$Date), ]
#'
#'output <- slidingwin(xvar = list(Temp = clim_data$Temp),
#'                     cdate = clim_data$Date, 
#'                     bdate = biol_data$Date, 
#'                     baseline = lm(Mass ~ 1, data = biol_data),
#'                     range = c(1, 0), 
#'                     type = "relative", stat = "mean", 
#'                     func = c("lin"), cmissing = FALSE, cinterval = "day")
#'
#'#Merge MassOutput
#'merge_results(output, output)
#'
#'\dontrun{
#'  
#'data(Offspring) 
#'data(OffspringClimate)
#'
#'# Test a linear functions
#'
#'OffspringWin_lin <- slidingwin(xvar = list(Temp = OffspringClimate$Temperature), 
#'                               cdate = OffspringClimate$Date, 
#'                               bdate = Offspring$Date, 
#'                               baseline = glm(Offspring ~ 1, data = Offspring, family = poisson),
#'                               range = c(150, 0), 
#'                               type = "relative", stat = "mean", 
#'                               func = c("lin"), cmissing = FALSE, cinterval = "day")
#'
#'# Test a quadratic functions
#'
#'OffspringWin_quad <- slidingwin(xvar = list(Temp = OffspringClimate$Temperature), 
#'                                cdate = OffspringClimate$Date, 
#'                                bdate = Offspring$Date, 
#'                                baseline = glm(Offspring ~ 1, data = Offspring, family = poisson),
#'                                range = c(150, 0), 
#'                                type = "relative", stat = "mean", 
#'                                func = c("quad"), cmissing = FALSE, cinterval = "day")
#'                                
#'# Combine these outputs
#'
#'OffspringWin_comb <- merge_results(dataset1 = OffspringWin_lin, dataset2 = OffspringWin_quad)
#'
#'#View analyses contained in the new output
#'
#'OffspringWin_comb$combos
#'
#'#View output from linear analysis
#'
#'head(OffspringWin_comb[[1]]$Dataset)
#'
#'}
#'        
#'@export

merge_results <- function(dataset1, dataset2){
  
  new_combos <- rbind(dataset1$combos, dataset2$combos)
  rownames(new_combos) <- seq(length = nrow(new_combos))
  
  dataset1[[length(dataset1)]] <- NULL
  dataset2[[length(dataset2)]] <- NULL
  
  new_dataset <- c(dataset1, dataset2)
  new_dataset$combos <- new_combos
  
  return(new_dataset)
  
}