# Test weightwin function #
test_that("weightwin works properly", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- weightwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
                    bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                    range = c(2, 1), func = "lin",
                    type = "relative", weightfun = "W", cinterval = "day",
                    par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
                    method = "L-BFGS-B")
  
  # Test that an error is returned when shape is less than 0 Weibull
  expect_error(weightwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
                         bdate = Mass$Date, baseline = lm(Mass$Mass ~ 1),
                         range = c(2, 1), func = "lin",
                         type = "relative", weightfun = "W", cinterval = "day",
                         par = c(-1, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
                         method = "L-BFGS-B"))
  
  # Test that an error is returned when scale is <=0 Weibull
  expect_error(weightwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
                         bdate = Mass$Date, baseline = lm(Mass$Mass ~ 1),
                         range = c(2, 1), func = "lin",
                         type = "relative", weightfun = "W", cinterval = "day",
                         par = c(3, 0, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
                         method = "L-BFGS-B"))
  
  # Test that an error is returned when location is greater than 0 Weibull
  expect_error(weightwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
                         bdate = Mass$Date, baseline = lm(Mass$Mass ~ 1),
                         range = c(2, 1), func = "lin",
                         type = "relative", weightfun = "W", cinterval = "day",
                         par = c(3, 0.2, 1), control = list(ndeps = c(0.01, 0.01, 0.01)),
                         method = "L-BFGS-B"))
  
  # Test that an error occurs when scale is <0 GEV
  expect_error(weightwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
                         bdate = Mass$Date, baseline = lm(Mass$Mass ~ 1),
                         range = c(2, 1), func = "lin",
                         type = "relative", weightfun = "G", cinterval = "day",
                         par = c(3, -1, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
                         method = "L-BFGS-B"))
  

  # Test that weightwin produces an object
  expect_true(is.list(test))
  
  # Test that intercept and slope are generated
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test best model data contains at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  # Test that list of optimisation is created
  expect_true(is.list(test[[3]]))
  
  # Test that optimisation data contains no NAs
  expect_equal(length(which(is.na(test[[3]]))), 0)
  
})
