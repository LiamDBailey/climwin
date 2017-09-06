# Test the outcomes of the daily_data function #

# Test that it works for monthly data#
test_that("daily_data works for monthly data", {
  
  #Load data
  data(Monthly_data)
  
  #Convert data into a daily data format.
  newdat <- daily_data(date = Monthly_data$Date, climate = Monthly_data$Temp, scale = "month")
  
  #Test that a dataframe is created
  expect_true(is.data.frame(newdat))
  
  #Test that it is at a daily scale
  expect_true((as.Date(newdat$Date[2], format = "%d/%m/%Y") - as.Date(newdat$Date[1], format = "%d/%m/%Y")) == 1)
  
})

# Test that it works for weekly data#
test_that("daily_data works for weekly data", {
  
  #Generate weekly data
  fulldates <- data.frame(date = seq(as.Date("01/01/1960", format = "%d/%m/%Y"), as.Date("01/01/2000", format = "%d/%m/%Y"), "weeks"))
  
  #Convert to correct date format
  fulldat <- data.frame(Date = paste(lubridate::day(fulldates$date), "/", lubridate::month(fulldates$date), "/", lubridate::year(fulldates$date), sep = ""))
  
  #Generate temperature data
  fulldat$Temp <- round(runif(nrow(fulldat), 20, 35), 2)
  
  #Convert data into a daily data format.
  newdat <- daily_data(date = fulldat$Date, climate = fulldat$Temp, scale = "week")
  
  #Test that a dataframe is created
  expect_true(is.data.frame(newdat))
  
  #Test that it is at a daily scale
  expect_true((as.Date(newdat$Date[2], format = "%d/%m/%Y") - as.Date(newdat$Date[1], format = "%d/%m/%Y")) == 1)
  
})