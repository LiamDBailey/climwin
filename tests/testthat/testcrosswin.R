# Test the outcome of crosswin #
test_that("crosswin produces output", {
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- crosswin(xvar = list(Temp = MassClimate$Temp), 
                   xvar2 = list(Rain = MassClimate$Rain), 
                   cdate = MassClimate$Date,
                   bdate = Mass$Date, range = c(2, 1), 
                   stat = "max", stat2 = "max", type = "relative",
                   cmissing = FALSE, cinterval = "day")
  
  furthest = 2
  closest = 1
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that crosswin has created a data frame object
  expect_true(is.data.frame(test))
  
  # Test that there are no NAs in the dataframe
  expect_equal(length(which(is.na(test))), 0)
  
  # Test that there are at least 7 columns
  expect_true(ncol(test) >= 7)
  
  # Test that the right number of models were fitted
  expect_equal(maxmodno, nrow(test))
  
})