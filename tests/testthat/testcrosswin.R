# Test the outcome of crosswin #

# Test that crosswin has created a correct CrossWinOutput object
# Expect that an object CrossWinOutput exists
# Expect that there are no NA values
# Expect that the number of columns is at least 7 (will vary with values of FIXED) 
# Expect that the number of rows is equal to the number of possible windows
test_that("crosswin produces output", {
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- crosswin(Xvar = MassClimate$Temp, Xvar2 = MassClimate$Rain, Cdate = MassClimate$Date,
           Bdate = Mass$Date, furthest = 2, closest = 1, 
           stat = "max", stat2 = "max", type = "variable",
           Cmissing = FALSE, Cinterval = "day")
  
  furthest = 2
  closest = 1
  duration  <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2
  
  expect_true(is.data.frame(test))
  expect_equal(length(which(is.na(test))), 0)
  expect_true(ncol(test) >= 7)
  expect_equal(MaxMODNO, nrow(test))
  
})