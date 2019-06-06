# Test weightwin function #
test_that("weightwin works properly", {
  
  set.seed(666)
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as previous R version
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == 1.3)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == -0.4)
  
})

#################################################################

# Test weightwin function #
test_that("weightwin works properly with k", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- weightwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
                    bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                    range = c(2, 1), func = "lin", k = 5,
                    type = "relative", weightfun = "W", cinterval = "day",
                    par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
                    method = "L-BFGS-B")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 28-09 (kfold not available in previous version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == -1.9)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == -0.4)
  
})

#################################################################

# Test weightwin function #
test_that("weightwin works properly with k and GEV", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- weightwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
                    bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                    range = c(2, 1), func = "lin", k = 5,
                    type = "relative", weightfun = "G", cinterval = "day",
                    par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
                    method = "L-BFGS-B")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 28-09 (kfold not available in previous version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == -2.7)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == -0.7)
  
})

#################################################################

# Test that spatial replication works with weightwin #
test_that("Spatial replication works with weightwin", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  
  test <- weightwin(xvar = list(Temp = Clim$Temp), cdate = Clim$Date,
                    bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                    range = c(2, 1), func = "lin",
                    type = "relative", weightfun = "W", cinterval = "day",
                    par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
                    method = "L-BFGS-B", spatial = list(Mass$Plot, Clim$Plot))
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as previous R version
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == 1.3)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == -0.4)
  
})

######

#Test that weightwin works with cmissing.

# Test when cmissing is FALSE and NAs are present (daily)#
test_that("Errors return when cmissing FALSE and NA present at a daily scale", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  expect_error(weightwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", func = "lin", cmissing = FALSE))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

# Test when cmissing is FALSE and NAs are present (weekly)#
test_that("Errors return when cmissing FALSE and NA present at a weekly scale", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(491:505), ]
  expect_error(weightwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", func = "lin", cmissing = FALSE,
                          cinterval = "week"))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

# Test when cmissing is FALSE and NAs are present (monthly)#
test_that("Errors return when cmissing FALSE and NA present at a monthly scale", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(480:520), ]
  expect_error(weightwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", func = "lin", cmissing = FALSE,
                          cinterval = "month"))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

# Test cmissing = FALSE with spatial replication (daily)
test_that("cmissing = FALSE with NAs and spatial replication (daily)", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim  <- rbind(MassClimate, MassClimate2)
  Clim2 <- Clim[-which(Clim$Plot == "B" & Clim$Date == "8/05/1966"), ]
  
  expect_error(weightwin(xvar = list(Clim2$Temp), cdate = Clim2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", 
                          func = "lin", cmissing = FALSE,
                          spatial = list(Mass$Plot, Clim2$Plot)))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

# Test cmissing = FALSE with spatial replication (weekly)
test_that("cmissing = FALSE with NAs and spatial replication (weekly)", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim  <- rbind(MassClimate, MassClimate2)
  Clim2 <- Clim[-c(18025:18015), ]
  
  expect_error(weightwin(xvar = list(Clim2$Temp), cdate = Clim2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", 
                          func = "lin", cmissing = FALSE,
                          spatial = list(Mass$Plot, Clim2$Plot),
                          cinterval = "week"))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

# Test cmissing = FALSE with spatial replication (monthly)
test_that("cmissing = FALSE with NAs and spatial replication (monthly)", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim  <- rbind(MassClimate, MassClimate2)
  Clim2 <- Clim[-c(18000:18050), ]
  
  expect_error(weightwin(xvar = list(Clim2$Temp), cdate = Clim2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", 
                          func = "lin", cmissing = FALSE,
                          spatial = list(Mass$Plot, Clim2$Plot),
                          cinterval = "month"))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

# Test when cmissing is method1 and no NA is present #
test_that("No errors return when cmissing method1 and full dataset", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- weightwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 2), 
                     type = "relative", func = "lin", cmissing = "method1")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 26-09 (cmissing is a new feature, so not available in R version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == -2.7)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == -0.7)
  
})

# Test when cmissing is method2 and no NA is present #
test_that("No errors return when cmissing method2 and full dataset", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- weightwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 2), 
                     type = "relative", func = "lin", cmissing = "method2")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 26-09 (cmissing is a new feature, so not available in R version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == -2.7)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == -0.7)
  
})

# Test when cmissing is method1 and NA is present (daily) #
test_that("No errors return when cmissing method1 with NAs (daily)", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  test <- weightwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", func = "lin", cmissing = "method1")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 26-09 (cmissing is a new feature, so not available in R version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == 1.9)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == 0.2)
  
})

# Test when cmissing is method2 and NA is present (daily)#
test_that("No errors return when cmissing method2 with NAs (daily)", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  test <- weightwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", func = "lin", cmissing = "method2")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 26-09 (cmissing is a new feature, so not available in R version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == 1.9)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == 0.2)
  
})

# Test when cmissing is method1 and NA is present (cinterval = "week") #
test_that("No errors returned when cmissing method1 with NAs, cinterval = week", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(491:505), ]
  
  # Test that an error is returned
  test <- weightwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", func = "lin", cinterval = "week",
                     cmissing = "method1")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 26-09 (cmissing is a new feature, so not available in R version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == 2.3)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == 0.0)
  
})

# Test when cmissing is method2 and NA is present (cinterval = "week") #
test_that("No error returned when cmissing method2 with NAs, cinterval = week", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())

  MassClimate2 <- MassClimate[-c(491:505), ]
  
  # Test that an error is returned
  test <- weightwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date,
                    bdate = Mass$Date,
                    baseline = lm(Mass ~ 1, data = Mass), range = c(50, 0),
                    type = "relative", func = "lin", cinterval = "week",
                    cmissing = "method2")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 26-09 (cmissing is a new feature, so not available in R version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == -31.1)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == -5.7)
  
})

# Test when cmissing is method1 and NA is present (cinterval = "month") #
test_that("Error returned when cmissing method1 with NAs, cinterval = month", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(1900:2000), ]
  
  # Test that an error is returned #
  test <- weightwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", func = "lin", cinterval = "month",
                     cmissing = "method1")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 26-09 (cmissing is a new feature, so not available in R version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == -0.9)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == -1.2)
  
})

# Test when cmissing is method2 and NA is present (cinterval = "month") #
test_that("Error returned when cmissing method2 with NAs, cinterval = month", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(1000:2000), ]
  
  # Test that an error is returned #
  test <- weightwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", func = "lin", cinterval = "month",
                     cmissing = "method2")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as GitHub 26-09 (cmissing is a new feature, so not available in R version)
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == -0.6)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == -1.2)
  
})

##############################################################

#Test that code works with weighted linear model

test_that("weightwin produces the right output when using weights in baseline model", {
  
  data("Offspring", envir = environment())
  data("OffspringClimate", envir = environment())
  
  furthest = 2
  closest = 0
  
  test <- weightwin(xvar = list(Temp = OffspringClimate$Temp), cdate = OffspringClimate$Date,
                    bdate = Offspring$Date, baseline = lm(Offspring ~ 1, data = Offspring, weights = Order),
                    range = c(2, 1), func = "lin",
                    type = "relative", weightfun = "W", cinterval = "day",
                    par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
                    method = "L-BFGS-B")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as previous R version
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == -36.7)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == 0)
  
})

##############################################################

#Test that code works when climate data reaches exactly to the refday (i.e. max(cdate) == max(bdate))

test_that("weightwin works when cmax(cdate) == max(bdate)", {
  
  data("Offspring", envir = environment())
  data("OffspringClimate", envir = environment())
  
  furthest = 2
  closest = 0
  
  test <- weightwin(xvar = list(Temp = OffspringClimate$Temp), cdate = OffspringClimate$Date,
                    bdate = Offspring$Date, baseline = lm(Offspring ~ 1, data = Offspring, weight = Order),
                    range = c(2, 1), func = "lin",
                    type = "absolute", refday = c(31, 1), weightfun = "W", cinterval = "day",
                    par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)),
                    method = "L-BFGS-B")
  
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
  expect_equal(length(which(is.na(test[[3]]$ModelBeta))), 0)
  
  #Test that results are the same as previous R version
  expect_true(round(test$WeightedOutput$deltaAICc, 1) == 1.7)
  expect_true(round(test$WeightedOutput$ModelBeta, 1) == 0)
  
})
