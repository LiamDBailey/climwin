subset_data <- function(data, end_date, start_date){
  data[data$date_int >= end_date & data$date_int <= start_date, ]
}