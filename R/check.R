check_bdate <- function(bdate, date_func = lubridate::dmy) {
  
  if (missing(bdate)) {
    stop("Please provide a vector of Date class or character string dd/mm/yyyy to bdate.")
  }
  
  error_msg <- "Some dates provided to bdate failed to parse. Consider converting bdate to Date class first."
  
  #Check if we have been provided a date
  if (!inherits(bdate, "Date")) {
    
    #If not, try to convert to a date using given function
    bdate <- tryCatch(date_func(bdate),
                      error = function(cond) {
                        stop(error_msg)
                      },
                      warning = function(cond) {
                        if (grepl(cond$message, pattern = "failed to parse")) {
                          stop(error_msg)
                        }
                      })
    
  }
  
  if (any(is.na(bdate))) {
    stop("Some values of bdate are NA.")    
  }
  
  return(bdate)
  
}
