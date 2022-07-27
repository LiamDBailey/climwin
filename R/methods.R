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

summary.climwinfit <- function(x){
  
  data <- x$Dataset[1, ]
  
  if (data$Type == "absolute") {
    
    WindowOpen_date  <- as.Date(paste(year(Sys.Date()), data$Reference.month, data$Reference.day, sep = "-")) - data$WindowOpen
    WindowClose_date <- as.Date(paste(year(Sys.Date()), data$Reference.month, data$Reference.day, sep = "-")) - data$WindowClose
    
    WindowOpen_date  <- format.Date(WindowOpen_date, "%B-%d")
    WindowClose_date <- format.Date(WindowClose_date, "%B-%d")
    
  } else {
    
    WindowOpen_date  <- NA
    WindowClose_date <- NA
    
  }
  
  cat("\nBest window:", "--------------------", sep = "\n")
  cat(paste0("Window open: ", data$WindowOpen, " (Date: ", WindowOpen_date, ")\n"))
  cat(paste0("Window close: ", data$WindowClose, " (Date: ", WindowClose_date, ")\n"))
  cat(paste0("\U0394", "AICc: ", round(data$deltaAICc, 2), "\n"))
  
  cat("\nTop model:\n", "--------------------", sep = "")
  print(x$BestModel)
  
}

summary.climwin <- function(x){
  
  for(i in 1:nrow(x$combos)) {
    
    cat("\n--------------------", paste0("Model #", i), "--------------------", sep = "\n")
    print(x$combos[i, ])
    summary.climwinfit(x[[1]])
    
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
