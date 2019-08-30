.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.5) return()
  
  intro <- c("To learn how to use climwin see our vignette. \n",
             "See help documentation and release notes for details on changes."
  )
  packageStartupMessage(intro)
}


#############

#Prototype recursive function that could be used to 
#reduce model fitting by comparing to the old model
#This is probably only relevant for cases where max/min
#are used as the summary stats.
#Ideally, we would run this so that short windows
#are run in order (because short models are more likely to be similar)

# recursive_func <- function(old_model, row){
#   
#   model_data <- iris
#   model_data$Sepal.Length <- model_data$Sepal.Length * round(runif(n = 1, min = 1, max = 3))
#   
#   if(row == 0){
#     
#     if(identical(model_data$Sepal.Length, model.frame(old_model)$Sepal.Length)){
#       
#       model_row <- data.frame(intercept = coef(old_model)[1],
#                               slope = coef(old_model)[2])
#       
#       return(dplyr::bind_rows(model_row))
#       
#     } else {
#       
#       new_model <- eval(getCall(old_model))
#       
#       model_row <- data.frame(intercept = coef(new_model)[1],
#                               slope = coef(new_model)[2])
#       
#       return(dplyr::bind_rows(model_row))
#       
#     }
#     
#   } else {
#     
#     if(identical(model_data$Sepal.Length, model.frame(old_model)$Sepal.Length)){
#       
#       model_row <- data.frame(intercept = coef(old_model)[1],
#                               slope = coef(old_model)[2])
#       
#       return(dplyr::bind_rows(model_row, recursive_func(old_model, row = row - 1)))
#       
#     } else {
#       
#       new_model <- eval(getCall(old_model))
#       
#       model_row <- data.frame(intercept = coef(new_model)[1],
#                               slope = coef(new_model)[2])
#       
#       return(dplyr::bind_rows(model_row, recursive_func(new_model, row = row - 1)))
#       
#     }
#     
#   }
#   
# }