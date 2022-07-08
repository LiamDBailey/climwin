print.climwin <- function(x){
  
  cat("\nSummary:", "--------------------", sep = "\n")
  print(x$combos)
  
  cat("\nTop model(s):", "--------------------", sep = "\n")
  if (length(x) == 2) {
    print(x[[1]]$BestModel) 
  } else {
    cat(paste(length(x) - 1, "best windows identified were identified\nUse `summary()` to see summary of models."))
  }
}

summary.climwin <- function(x){
  
  for(i in 1:nrow(x$combos)) {
    
    cat("\n--------------------", paste0("Model #", i), "--------------------", sep = "\n")
    print(x$combos[i, ])
    print(summary(x[[i]]$BestModel))
    
  }
  
}

plot.climwin <- function(x, type = "all", n){
  
  if (length(x) == 2) {
    n <- 1
  } else if (length(x) > 2 & missing(n)) {
    stop("Please select a window to plot using the argument `n`.")
  }
  
  data <- get_dataset(x, n = n)
  
  if (type == "all") {
    plotall(data)
  } else if (type == "delta") {
    plotdelta(data)
  } else if (type == "beta") {
    plotbetas(data)
  } else if (type == "best") {
    plotbest(data)
  } else if (type == "weights") {
    plotweights(data)
  } else if (type == "win") {
    plotwin(data)
  }
  
}
