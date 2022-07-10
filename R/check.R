check_date <- function(date, arg_name, date_func = lubridate::dmy) {
  
  ## NOTE: This function isn't a full check of cdate (i.e. we need range, refday, spatial info)
  ## but it is a good early test. Allows us to terminate code if there is anything obviously wrong
  
  if (missing(date)) {
    stop(paste("Please provide a vector of Date class or character string dd/mm/yyyy to", arg_name))
  }
  
  error_msg <- paste("Some dates provided to", arg_name, "failed to parse. Consider converting", arg_name, "to Date class first.")
  
  #Check if we have been provided a date
  if (!inherits(date, "Date")) {
    
    #If not, try to convert to a date using given function
    date <- tryCatch(date_func(date),
                      error = function(cond) {
                        stop(error_msg)
                      },
                      warning = function(cond) {
                        if (grepl(cond$message, pattern = "failed to parse")) {
                          stop(error_msg)
                        }
                      })
    
  }
  
  if (any(is.na(date))) {
    stop(paste("Some values of", arg_name, "are NA."))
  }
  
  return(date)
  
}

check_spatial <- function(spatial, length_b, length_c){
  
  ## TODO: THIS IS CURRENTLY DUMMY UNTIL WE FIX #22 (climate/biol data is a dataframe)
  ## ONCE THIS HAS BEEN DONE WE'LL TAKE 2 DATAFRAMES AND COMPARE
  if (is.null(spatial)) {
    
    spatial <- list(rep("A", times = length_b),
                    rep("A", times = length_c))
    
  }
  
  return(spatial)
  
}