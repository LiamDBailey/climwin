# Test randwin function #
test_that("Check randwin output", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 2
  closest = 1
  stat = "max"
  
  rand <- randwin(repeats = 2, xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
                  bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass), 
                  range = c(2, 1), 
                  type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1)) / 2
  
  # Test that randwin produces an output
  expect_true(is.list(rand))
  
  # Test that there are no NA values in output
  expect_equal(length(which(is.na(rand[[1]][, 4]))), 0)
  
  # Test that the randomised output has the right number of columns
  expect_true(ncol(rand[[1]]) == 17)
  
  # Test that the right number of models has been fitted
  expect_equal(maxmodno, nrow(subset(rand[[1]], Repeat == 1)))
  
  # Test that data has been stored as randomised
  expect_true((rand[[1]]["Randomised"])[1,] == "yes")
  
})