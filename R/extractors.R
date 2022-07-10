get_dataset <- function(object, n){
  
  if (missing(object) | !inherits(object, "climwin")) {
    
    stop("Please provide a climwin results object")
    
  }
  
  #There will always be 2 objects because of combos
  if (length(object) == 2) {
    
    n <- 1
    
  } else if (missing(n)){
    
    stop("Please specify which set of climwin results to extract using argument `n`")
    
  }
  
  return(object[[n]]$Dataset)
  
}

get_model <- function(object, n){
  
  if (missing(object) | !inherits(object, "climwin")) {
    
    stop("Please provide a climwin results object")
    
  }
  
  #There will always be 2 objects because of combos
  if (length(object) == 2) {
    
    n <- 1
    
  } else if (missing(n)){
    
    stop("Please specify which set of climwin results to extract using argument `n`")
    
  }
  
  return(object[[n]]$BestModel)
  
}
