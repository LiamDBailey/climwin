context("autowin function")

# Test the outcomes of autowin #

# Test that autowin has created a correct AutoWinOutput object
# Expect that an object AutoWinOutput exists
# Expect that there are no NA values
# Expect that the number of columns is at least 7 (will vary with values of FIXED) 
# Expect that the number of rows is equal to the number of possible windows
test_that("AutoWinOutput has created an output", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  single <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
                      baseline = lm(Mass$Mass~1), furthest = 1, closest = 1,
                      STAT = "mean", FUNC = "L",
                      FIXED = FALSE, CMISSING = FALSE, CINTERVAL = "D")
  
  furthest  <- 2
  closest   <- 1
  STAT <- "max"
  
  test <- autowin(reference = single[[2]]$temporary,
          Xvar  = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
          furthest = 2, closest = 1, STAT = "mean",
          FIXED = FALSE, CMISSING = FALSE, CINTERVAL = "D")
  
  duration  <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2
  
  expect_true(exists("test"))
  expect_equal(length(which(is.na(test))), 0)
  expect_true(ncol(test) >= 7)
  expect_equal(MaxMODNO, nrow(test))
})