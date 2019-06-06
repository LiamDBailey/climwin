# Test the outcomes of slidingwin #

#Need to vary:
# baseline model type: glm, lme4, glmer
# cinterval: day, week, month
# stat: mean, min, max, slope
# func: lin, quad, cub, inv, log
# upper: range
# lower: range
# binary: TRUE or FALSE
# k: 0 and >1
# centre: value

##########################################################

# Test regular output #
test_that("slidingwin produces the right output", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 2
  closest = 0
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that a list has been produced
  expect_true(is.list(test))
  
  # Test that a best model was returned
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 columns
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test that there are no NAs in the output dataset
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that all columns were created in the dataset
  expect_true(ncol(test[[1]]$Dataset) == 17)
  
  # Test that the correct number of models were recorded in the dataset
  expect_equal(maxmodno, nrow(test[[1]]$Dataset))
  
  # Test that data was not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -2.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.7)
  
})

# Test output with multiple combos #
test_that("slidingwin produces multiple combos", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- slidingwin(xvar = list(Temp = MassClimate$Temp, Rain = MassClimate$Rain), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", stat = c("max", "min"), func = c("lin", "quad"), cmissing = FALSE)
  
  # Test that all combos of stat, func and xvar were created
  expect_equal(nrow(test$combos), 8)
  
})

##########################################################

# Test that upper and lower work #

# Test with upper and binary:
test_that("slidingwin produces binary values with upper and binary = TRUE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                     upper = 10, binary = TRUE)
  
  # Test that the minimum value of climate was set at 0
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  
  # Test that the maximum value of climate was set at 1
  expect_equal(max(test[[1]]$BestModelData$climate), 1)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 1.2)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 0)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 1.9)
  
})

# Test with upper and without binary
test_that("slidingwin produces non-binary values with upper and binary = FALSE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                     upper = 10, binary = FALSE)
  
  # Test that the minimum value of climate was set at 0
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  
  # Test that the maximum value of climate in greater than 1
  expect_true(max(test[[1]]$BestModelData$climate) > 1)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 1.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.1)
  
})

# Test with lower and without binary
test_that("slidingwin produces non-binary values with lower and binary = FALSE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                     lower = 10, binary = FALSE)
  
  # Test that the minumum value of climate is set at 0
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  
  # Test that the maximum value of climate is greater than 1
  expect_true(max(test[[1]]$BestModelData$climate) > 1)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 1.5)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 0)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.1)
  
})

# With lower and binary
test_that("slidingwin produces binary values with lower and binary = TRUE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                     lower = 10, binary = TRUE)
  
  # Test that the minimum value of climate is set at 0
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  
  # Test that the maximum value of climate is set at 1
  expect_equal(max(test[[1]]$BestModelData$climate), 1)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 1.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 0)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 1.1)
  
})

# Test with upper, lower and binary
test_that("slidingwin produces binary values with lower/upper and binary = TRUE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                     lower = 10, upper = 15, binary = TRUE)
  
  # Test that the minimum value of climate is set at 0
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  
  # Test that the maximum value of climate is set at 1
  expect_equal(max(test[[1]]$BestModelData$climate), 1)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 0.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 0)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 2.2)
  
})

# With upper and lower but without binary
test_that("slidingwin produces non-binary values with lower/upper and binary = FALSE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                     lower = 10, upper = 15, binary = FALSE)
  
  # Test that the minimum value of climate is set at 0
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  
  # Test that the maximum value of climate is greater than 1
  expect_true(max(test[[1]]$BestModelData$climate) > 1)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 1.6)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.4)
  
})

##########################################################

# Test different settings of cmissing #

# Test when cmissing is FALSE and NAs are present (daily)#
test_that("Errors return when cmissing FALSE and NA present at a daily scale", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  expect_error(slidingwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

# Test when cmissing is FALSE and NAs are present (weekly)#
test_that("Errors return when cmissing FALSE and NA present at a weekly scale", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(491:505), ]
  expect_error(slidingwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                          cinterval = "week"))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

# Test when cmissing is FALSE and NAs are present (monthly)#
test_that("Errors return when cmissing FALSE and NA present at a monthly scale", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(480:520), ]
  expect_error(slidingwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", stat = "max", func = "lin", cmissing = FALSE,
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
  
  expect_error(slidingwin(xvar = list(Clim2$Temp), cdate = Clim2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", 
                     stat = "max", func = "lin", cmissing = FALSE,
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
  
  expect_error(slidingwin(xvar = list(Clim2$Temp), cdate = Clim2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", 
                          stat = "max", func = "lin", cmissing = FALSE,
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
  
  expect_error(slidingwin(xvar = list(Clim2$Temp), cdate = Clim2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", 
                          stat = "max", func = "lin", cmissing = FALSE,
                          spatial = list(Mass$Plot, Clim2$Plot),
                          cinterval = "month"))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

# Test when cmissing is method1 and no NA is present #
test_that("No errors return when cmissing method1 and full dataset", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 2), 
                     type = "relative", stat = "max", func = "lin", cmissing = "method1")
  
  #Test that slidingwin ran without an error
  expect_true(is.list(test))
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -2.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.7)

})

# Test when cmissing is method2 and no NA is present #
test_that("No errors return when cmissing method2 and full dataset", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 2), 
                     type = "relative", stat = "max", func = "lin", cmissing = "method2")
  
  # Test that slidingwin ran without an error
  expect_true(is.list(test))
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -2.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.7)
  
})

# Test when cmissing is method1 and NA is present (daily) #
test_that("No errors return when cmissing method1 with NAs (daily)", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  test <- slidingwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = "method1")
  
  # Test that slidingwin ran without an error
  expect_true(is.list(test))
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -2.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.7)
  
})

# Test when cmissing is method2 and NA is present (daily)#
test_that("No errors return when cmissing method2 with NAs (daily)", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  test <- slidingwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = "method2")
  
  # Test that slidingwin ran without an error
  expect_true(is.list(test))
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -2.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.7)
  
})

# Test when cmissing is method1 and NA is present (cinterval = "week") #
test_that("No errors returned when cmissing method1 with NAs, cinterval = week", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(491:505), ]
  
  # Test that an error is returned
  test <- slidingwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "relative", stat = "max", func = "lin", cinterval = "week",
                          cmissing = "method1")
  
  # Test that slidingwin ran without an error
  expect_true(is.list(test))
  
  #Test the values we get stay the same as github version (7-9-17). 
  #N.B. We don't use the old R version because we have changed how we calculate weeks (i.e. use lubridate::week rather than dividing by 7)
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 0.2)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.6)
  
})

# Test when cmissing is method2 and NA is present (cinterval = "week") #
test_that("No error returned when cmissing method2 with NAs, cinterval = week", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(491:505), ]
  
  # Test that an error is returned
  test <- slidingwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin", cinterval = "week",
                     cmissing = "method2")
  
  # Test that slidingwin ran without an error
  expect_true(is.list(test))
  
  #Test the values we get stay the same as github version (7-9-17). 
  #N.B. We don't use the old R version because we have changed how we calculate weeks (i.e. use lubridate::week rather than dividing by 7)
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 0.2)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.6)
  
})

# Test when cmissing is method1 and NA is present (cinterval = "month") #
test_that("No error returned when cmissing method1 with NAs, cinterval = month", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(1900:2000), ]
  
  # Test that an error is returned #
  test <- slidingwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                          type = "relative", stat = "max", func = "lin", cinterval = "month",
                          cmissing = "method1")
  
  # Test that slidingwin ran without an error
  expect_true(is.list(test))
  
  #Test the values we get stay the same as github version (7-9-17). 
  #N.B. We don't use the old R version because we have changed how we calculate months slightly
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -3.9)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -1.8)
  
})

# Test when cmissing is method2 and NA is present (cinterval = "month") #
test_that("No error returned when cmissing method2 with NAs, cinterval = month", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-c(1000:2000), ]
  
  # Test that an error is returned #
  test <- slidingwin(xvar = list(MassClimate2$Temp), cdate = MassClimate2$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "relative", stat = "max", func = "lin", cinterval = "month",
                     cmissing = "method2")
  
  # Test that slidingwin ran without an error
  expect_true(is.list(test))
  
  #Test the values we get stay the same as github version (7-9-17). 
  #N.B. We don't use the old R version because we have changed how we calculate months slightly
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -1.9)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -1.6)
  
})

##########################################################

# Test different types of models #

# Test glm models #
test_that("glm models can run in slidingwin", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = glm(Mass ~ 1, data = Mass, family = poisson), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that best model was created
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NA values in best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that there are atleast 2 variables in the best model data
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 1.3)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.0)
  
})

# Test mixed effects models (lme4) #
test_that("lmer [lme4] models can run in slidingwin", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                     bdate = Offspring$Date, 
                     baseline = lmer(Offspring ~ 1 + (1|BirdID), data = Offspring, REML = F),  
                     range = c(2, 0), type = "relative", 
                     stat = "max", func = "lin", cmissing=FALSE)
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that lmer model produced an intercept
  expect_false(is.na(fixef(test[[1]]$BestModel)[1]))
  
  # Test that lmer model produced a climate beta estimate
  expect_false(is.na(fixef(test[[1]]$BestModel)[2]))
  
  # Test that best model data doesn't contain NAs
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -55.3)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.0)
  
})

# Test mixed effects models (lme4) #
test_that("lmer [lme4] models can run in slidingwin with cross-validation", {
  
  set.seed(666)
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                     bdate = Offspring$Date, 
                     baseline = lmer(Offspring ~ 1 + (1|BirdID), data = Offspring, REML = F),  
                     range = c(2, 1), type = "relative", 
                     stat = "max", func = "lin", cmissing=FALSE, k = 2)
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that lmer model produced an intercept
  expect_false(is.na(fixef(test[[1]]$BestModel)[1]))
  
  # Test that lmer model produced a climate beta estimate
  expect_false(is.na(fixef(test[[1]]$BestModel)[2]))
  
  # Test that best model data doesn't contain NAs
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -26.3)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.0)
  
})

# Test glmer models (lme4) #
test_that("glmer [lme4] models can run in slidingwin", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  # Warnings created due to convergence issues with such a small data set
  suppressWarnings(test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                     bdate = Offspring$Date, 
                     baseline = glmer(Offspring ~ 1 + (1|Order), data = Offspring, family = "poisson"),  
                     range = c(1, 0), type = "relative", 
                     stat = "max", func = "lin", cmissing=FALSE))
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that glmer model produced an intercept
  expect_false(is.na(fixef(test[[1]]$BestModel)[1]))
  
  # Test that glmer model produced a beta estimate for climate
  expect_false(is.na(fixef(test[[1]]$BestModel)[2]))
  
  # Test there are no NA values in best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has atleast two parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -14.0)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.0)
  
})

# Test glmer models (lme4) #
test_that("glmer [lme4] models can run in slidingwin with cross-validation", {
  
  set.seed(666)
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  # Warnings created due to convergence issues with such a small data set
  suppressWarnings(test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                                      bdate = Offspring$Date, 
                                      baseline = glmer(Offspring ~ 1 + (1|Order), data = Offspring, family = "poisson"),  
                                      range = c(1, 0), type = "relative", 
                                      stat = "max", func = "lin", cmissing=FALSE, k = 2))
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that glmer model produced an intercept
  expect_false(is.na(fixef(test[[1]]$BestModel)[1]))
  
  # Test that glmer model produced a beta estimate for climate
  expect_false(is.na(fixef(test[[1]]$BestModel)[2]))
  
  # Test there are no NA values in best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has atleast two parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -6.5)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.0)
  
})

# Test mixed effects models (nlme) #
test_that("basic lme [nlme] models can run in slidingwin", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                     bdate = Offspring$Date, 
                     baseline = lme(Offspring ~ 1, random = ~ 1 | BirdID, data = Offspring, method = "ML"),  
                     range = c(2, 0), type = "relative", 
                     stat = "max", func = "lin", cmissing = FALSE)
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that lmer model produced an intercept
  expect_false(is.na(fixef(test[[1]]$BestModel)[1]))
  
  # Test that lmer model produced a climate beta estimate
  expect_false(is.na(fixef(test[[1]]$BestModel)[2]))
  
  # Test that best model data doesn't contain NAs
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as github version (7-9-17).
  #N.B. Earlier R version didn't have nlme compatability
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -55.3)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.0)
  
})

# Test mixed effects models (nlme) #
test_that("lme [nlme] model with VarIdent can run in slidingwin", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  assign("vI", varIdent(value = 10, form =~ Order), envir=globalenv())
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                     bdate = Offspring$Date, 
                     baseline = lme(Offspring ~ Order, random = ~ 1 | BirdID, data = Offspring, weights = vI, method = "ML"),  
                     range = c(2, 0), type = "relative", 
                     stat = "max", func = "lin", cmissing = FALSE)
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that lmer model produced an intercept
  expect_false(is.na(fixef(test[[1]]$BestModel)[1]))
  
  # Test that lmer model produced a climate beta estimate
  expect_false(is.na(fixef(test[[1]]$BestModel)[2]))
  
  # Test that best model data doesn't contain NAs
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as github version (7-9-17).
  #N.B. Earlier R version didn't have nlme compatability
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -22.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.0)
  
  rm("vI", envir = .GlobalEnv)
  
})

# Test mixed effects models (nlme) #
test_that("lme [nlme] model with VarExp can run in slidingwin", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  assign("vE", varExp(form =~ Order), envir=globalenv())
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                     bdate = Offspring$Date, 
                     baseline = lme(Offspring ~ Order, random = ~ 1 | BirdID, data = Offspring, weights = vE, method = "ML"),  
                     range = c(2, 0), type = "relative", 
                     stat = "max", func = "lin", cmissing = FALSE)
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that lmer model produced an intercept
  expect_false(is.na(fixef(test[[1]]$BestModel)[1]))
  
  # Test that lmer model produced a climate beta estimate
  expect_false(is.na(fixef(test[[1]]$BestModel)[2]))
  
  # Test that best model data doesn't contain NAs
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as github version (7-9-17).
  #N.B. Earlier R version didn't have nlme compatability
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -23.5)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.0)
  
  rm("vE", envir = .GlobalEnv)
  
})

### TEST COXPH MODELS!!!!!!!!! STILL NEEDS TO BE DONE! ###

##########################################################

# Test absolute windows #

# Test absolute window #
test_that("absolute window works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "absolute", refday = c(20, 5), 
                     stat = "max", func = "lin", cmissing=FALSE)
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that a best model has been fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -4.2)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.8)
  
})

##########################################################

# Test slope stat #
test_that("slope stat work", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "slope", func = "lin", cmissing=FALSE)

  # Test that slidingwin produces an output
  expect_true(is.list(test))
  
  # Test that a best model is created
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test there are no NAs in best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as github (7-9-17)
  #N.B. Don't use original R version because we have reversed the sign of slopes
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -4.0)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 0)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -1.4)
  
})

# Test that slope and log stat cannot be used together #
test_that("slope and log return error", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  # Test that an error is produced
  expect_error(slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date,
                          bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                          range = c(2, 1), type = "relative", stat = "slope",
                          func = "log"))  
  
})

# Test that slope and inverse cannot be used together #
test_that("slope and inv return error", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  # Test that an error is produced
  expect_error(slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date,
                          bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                          range = c(2, 1), type = "relative", stat = "slope",
                          func = "inv"))  
  
})

##########################################################

# Test different functions for fitting climate #

# Test quadratic function #
test_that("Quadratic function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "quad", cmissing=FALSE)
  
  furthest = 2
  closest = 0
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that a best model was fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least THREE parameters (i.e. linear and quadratic term)
  expect_true(ncol(test[[1]]$BestModelData) >= 3)
  
  # Test there are no NAs in the dataset
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 5]))), 0)
  
  # Test that the quad function has been used
  expect_true(test[[1]]$Dataset[1, 10] == "quad")
  
  # Test that the dataset is atleast 17 columns (one extra column for quad SE)
  expect_true(ncol(test[[1]]$Dataset) == 18)
  
  # Test that the right number of models was fitted
  expect_equal(maxmodno, nrow(test[[1]]$Dataset))
  
  # Test that data is not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -1.0)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -2.8)
  
})

# Test cubic function #
test_that("Cubic function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "cub", cmissing=FALSE)
  
  furthest = 2
  closest = 0
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that a best model was fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least FOUR parameters (i.e. linear, quadratic and cubic term)
  expect_true(ncol(test[[1]]$BestModelData) >= 4)
  
  # Test there are no NAs in the dataset
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 6]))), 0)
  
  # Test that the cub function has been used
  expect_true(test[[1]]$Dataset[1, 11] == "cub")
  
  # Test that the dataset is atleast 18 columns (extra columns for quad and cub SE)
  expect_true(ncol(test[[1]]$Dataset) == 19)
  
  # Test that the right number of models was fitted
  expect_equal(maxmodno, nrow(test[[1]]$Dataset))
  
  # Test that data is not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 1.3)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 1.7)
 
})

# Test log function #
test_that("Log function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "log", cmissing=FALSE)
  
  furthest = 2
  closest = 0
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that a best model was fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least two parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test there are no NAs in the dataset
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that log function has been used
  expect_true(test[[1]]$Dataset[1, 9] == "log")
  
  # Test that the dataset is atleast 16 columns (no extra columns)
  expect_true(ncol(test[[1]]$Dataset) == 17)
  
  # Test that the right number of models was fitted
  expect_equal(maxmodno, nrow(test[[1]]$Dataset))
  
  # Test that data is not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -3.1)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -8.3)
  
})

# Test inverse function #
test_that("Inverse function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "inv", cmissing=FALSE)
  
  furthest = 2
  closest = 0
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that a best model was fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least two parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test there are no NAs in the dataset
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that the inv function has been used
  expect_true(test[[1]]$Dataset[1, 9] == "inv")
  
  # Test that the dataset is atleast 16 columns (no extra columns)
  expect_true(ncol(test[[1]]$Dataset) == 17)
  
  # Test that the right number of models was fitted
  expect_equal(maxmodno, nrow(test[[1]]$Dataset))
  
  # Test that data is not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -3.1)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 88.7)
  
})

##########################################################

# Test different cinterval values #

# Test cinterval = week #
test_that("Weekly interval works", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                   baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                   type = "relative", stat = "max", func = "lin",
                   cmissing=FALSE, cinterval = "week")

# Test that slidingwin produced an output
expect_true(is.list(test))

# Test that a best model was fitted
expect_false(is.na((test[[1]]$BestModel)[1]))

# Test that there are no NAs in the best model data
expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)

# Test that best model data has at least two parameters
expect_true(ncol(test[[1]]$BestModelData) >= 2)

#Test the values we get stay the same as github version (7-9-17). 
#N.B. We don't use the old R version because we have changed how we calculate weeks (i.e. use lubridate::week rather than dividing by 7)
expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 0.1)
expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.7)

})

# Test cinterval = month #
test_that("Monthly interval works", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())
  
test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                   baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                   type = "relative", stat = "max", func = "lin",
                   cmissing=FALSE, cinterval = "month")

# Test that slidingwin produced an output
expect_true(is.list(test))

# Test that a best model was fitted
expect_false(is.na((test[[1]]$BestModel)[1]))

# Test that there are no NAs in the best model data
expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)

# Test that best model data has at least two parameters
expect_true(ncol(test[[1]]$BestModelData) >= 2)

#Test the values we get stay the same as github version (7-9-17). 
#N.B. We don't use the old R version because we have changed how we calculate month
expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -8.3)
expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -1.8)

})

################################################################

# Error when you have NAs in the biological data #
test_that("slidingwin gives error when NAs are present in biological data", {
  
  data(MassClimate, envir = environment())
  Mass <- data.frame(Date = c("01/01/2014", "01/02/2014"), Mass = c(NA, 1))
  
  # Test that an error occurs #
  expect_error(slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 2), 
                          type = "relative", stat = "max", func = "lin",
                          cmissing = FALSE, cinterval = "day"))  

  })

################################################################

# Test that cross validation works #
test_that("Does cross validation work?", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin",
                     cmissing = FALSE, cinterval = "day", k = 2)
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that a best model was fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least two parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test that values of k are documented in the dataset
  expect_equal(test[[1]]$Dataset$K[1], 2)
  
})

################################################################

# Test mean centring #

# Test centring with both wgmean and wgdev used #
test_that("Mean centring is functioning", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  Offspring$Year <- lubridate::year(as.Date(Offspring$Date, format = "%d/%m/%Y"))
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                     bdate = Offspring$Date, baseline = lm(Offspring ~ 1, data = Offspring), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin",
                     cmissing = FALSE, cinterval = "day", centre = list(Offspring$Year, "both"))
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that a best model was fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least THREE parameters (i.e. wgdev, wgmean and climate)
  expect_true(ncol(test[[1]]$BestModelData) >= 3)
  
  # Test that there are no NAs in wgmean
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that there are no NAs in wgdev
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 6]))), 0)
  
  # Test that there are 16 columns in the dataset
  expect_true(ncol(test[[1]]$Dataset) == 17)
  
  # Test that dataset is not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -39.0)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$WithinGrpMean[1], 1) == 0.0)
  expect_true(round(test[[1]]$Dataset$WithinGrpDev[1], 1) == 0.0)
  
})

# Test centring with only wgmean #
test_that("Mean centring is functioning with only wgmeans", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  Offspring$Year <- lubridate::year(as.Date(Offspring$Date, format = "%d/%m/%Y"))
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                     bdate = Offspring$Date, baseline = lm(Offspring ~ 1, data = Offspring), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin",
                     cmissing = FALSE, cinterval = "day", centre = list(Offspring$Year, "mean"))
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that a best model was fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least two parameters (i.e. wgmean and climate)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test that there are no NAs in wgmean
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that there are no NAs in wgdev
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 6]))), 0)
  
  # Test that there are 14 columns in the dataset (remove wgdev and SE)
  expect_true(ncol(test[[1]]$Dataset) == 15)
  
  # Test that dataset is not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -2.8)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$WithinGrpMean[1], 1) == 0.0)
  
})

# Test centring with only wgdev #
test_that("Mean centring is functioning with only wgdev", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  Offspring$Year <- lubridate::year(as.Date(Offspring$Date, format = "%d/%m/%Y"))
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, 
                     bdate = Offspring$Date, baseline = lm(Offspring ~ 1, data = Offspring), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin",
                     cmissing = FALSE, cinterval = "day", centre = list(Offspring$Year, "dev"))
  
  # Test that slidingwin produced an output
  expect_true(is.list(test))
  
  # Test that a best model was fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that best model data has at least two parameters (i.e. wgdev and climate)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test that there are no NAs in wgmean
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that there are no NAs in wgdev
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 6]))), 0)
  
  # Test that there are 14 columns in the dataset (remove wgmean and SE)
  expect_true(ncol(test[[1]]$Dataset) == 15)
  
  # Test that dataset is not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -36.1)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$WithinGrpDev[1], 1) == 1.3)
  
})

################################################################

# Test cohort parameter #
test_that("absolute window works with a cohort parameter", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  
  test <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "absolute", refday = c(20, 5), 
                     stat = "max", func = "lin", cmissing = FALSE,
                     cohort = Mass$Plot)
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that a best model has been fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == 2.3)
  #expect_true(test[[1]]$Dataset$WindowOpen[1] == 1 & test[[1]]$Dataset$WindowClose[1] == 1)
  #expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 1.6)
  
})

###############################################################

# Test spatial replication
test_that("spatial replication works in slidingwin", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim  <- rbind(MassClimate, MassClimate2)
  
  test <- slidingwin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "absolute", refday = c(20, 5), 
                     stat = "max", func = "lin", cmissing = FALSE,
                     spatial = list(Mass$Plot, Clim$Plot))
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that a best model has been fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -4.2)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.8)
  
})

test_that("spatial replication works in slidingwin with week", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  
  test <- slidingwin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "absolute", refday = c(20, 5), 
                     cinterval = "week",
                     stat = "max", func = "lin", cmissing=FALSE,
                     spatial = list(Mass$Plot, Clim$Plot))
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that a best model has been fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as github (7-9-17)
  #Don't use old R because we changed how we estimate week slightly
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -8)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.9)
  
})

test_that("spatial replication works in slidingwin with month", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  
  test <- slidingwin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "absolute", refday = c(20, 5), 
                     cinterval = "month",
                     stat = "max", func = "lin", cmissing=FALSE,
                     spatial = list(Mass$Plot, Clim$Plot))
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that a best model has been fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as github (7-9-17)
  #Don't use old R because we changed how we estimate month slightly
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -32.5)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -3.2)
  
})

test_that("spatial replication works in slidingwin with upper", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  
  test <- slidingwin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "absolute", refday = c(20, 5), 
                     cinterval = "day", upper = 15,
                     stat = "max", func = "lin", cmissing=FALSE,
                     spatial = list(Mass$Plot, Clim$Plot))
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that a best model has been fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -0.0)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 0)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.2)
  
})

test_that("spatial replication with slidingwin returns an error with NAs and cmissing FALSE", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  Clim <- Clim[-which(Clim$Date == "20/05/1979"), ]
  
  expect_error(slidingwin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0), 
                     type = "absolute", refday = c(20, 5), 
                     cinterval = "day",
                     stat = "max", func = "lin", cmissing = FALSE,
                     spatial = list(Mass$Plot, Clim$Plot)))
  
  expect_true(exists("missing"))
  
  rm("missing", envir = .GlobalEnv)
  
})

test_that("spatial replication works with slidingwin with NAs and cmissing method1", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  Clim <- Clim[-which(Clim$Date == "20/05/1979"), ]
  
  test <- slidingwin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "absolute", refday = c(20, 5), 
                          cinterval = "day",
                          stat = "max", func = "lin", cmissing = "method1",
                          spatial = list(Mass$Plot, Clim$Plot))
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that a best model has been fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -4.2)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.8)
  
})

test_that("spatial replication works with slidingwin with NAs and cmissing method2", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  Clim <- Clim[-which(Clim$Date == "20/05/1979"), ]
  
  test <- slidingwin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "absolute", refday = c(20, 5), 
                     cinterval = "day",
                     stat = "max", func = "lin", cmissing = "method2",
                     spatial = list(Mass$Plot, Clim$Plot))
  
  # Test that slidingwin has produced an output
  expect_true(is.list(test))
  
  # Test that a best model has been fitted
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 parameters
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -4.2)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 1)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.8)
  
})

##################################################################

#Test that code works with climate in a monthly format

test_that("slidingwin produces the right output with monthly climate data", {
  
  data(Mass, envir = environment())
  data(Monthly_data, envir = environment())
  
  furthest = 2
  closest = 0
  
  test <- slidingwin(xvar = list(Monthly_data$Temp), cdate = Monthly_data$Date, bdate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                     cinterval = "month")
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that a list has been produced
  expect_true(is.list(test))
  
  # Test that a best model was returned
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 columns
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test that there are no NAs in the output dataset
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that all columns were created in the dataset
  expect_true(ncol(test[[1]]$Dataset) == 17)
  
  # Test that the correct number of models were recorded in the dataset
  expect_equal(maxmodno, nrow(test[[1]]$Dataset))
  
  # Test that data was not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -0.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == -0.3)
  
})

##############################################################

#Test that code works with weighted linear model

test_that("slidingwin produces the right output when using weights in model", {
  
  data("Offspring", envir = environment())
  data("OffspringClimate", envir = environment())
  
  furthest = 2
  closest = 0
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, bdate = Offspring$Date, 
                     baseline = lm(Offspring ~ 1, data = Offspring, weights = Order), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                     cinterval = "month")
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that a list has been produced
  expect_true(is.list(test))
  
  # Test that a best model was returned
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 columns
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test that there are no NAs in the output dataset
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that all columns were created in the dataset
  expect_true(ncol(test[[1]]$Dataset) == 17)
  
  # Test that the correct number of models were recorded in the dataset
  expect_equal(maxmodno, nrow(test[[1]]$Dataset))
  
  # Test that data was not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -88.6)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.1)
  
})

test_that("slidingwin produces the right output when using equal weights in model", {
  
  data("Offspring", envir = environment())
  data("OffspringClimate", envir = environment())
  Offspring$weight <- 1
  
  furthest = 2
  closest = 0
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, bdate = Offspring$Date, 
                     baseline = lm(Offspring ~ 1, data = Offspring, weights = weight), range = c(2, 0), 
                     type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                     cinterval = "month")
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that a list has been produced
  expect_true(is.list(test))
  
  # Test that a best model was returned
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 columns
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test that there are no NAs in the output dataset
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that all columns were created in the dataset
  expect_true(ncol(test[[1]]$Dataset) == 17)
  
  # Test that the correct number of models were recorded in the dataset
  expect_equal(maxmodno, nrow(test[[1]]$Dataset))
  
  # Test that data was not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -88.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0.1)
  
})

##############################################################

#Test that code works when climate data reaches exactly to the refday (i.e. max(cdate) == max(bdate))

test_that("slidingwin works when cmax(cdate) == max(bdate)", {
  
  data("Offspring", envir = environment())
  data("OffspringClimate", envir = environment())
  
  furthest = 2
  closest = 0
  
  test <- slidingwin(xvar = list(OffspringClimate$Temp), cdate = OffspringClimate$Date, bdate = Offspring$Date, 
                     baseline = lm(Offspring ~ 1, data = Offspring, weights = Order), range = c(2, 0), 
                     type = "absolute", refday = c(31, 01), stat = "max", func = "lin", cmissing = FALSE,
                     cinterval = "day")
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1))/2
  
  # Test that a list has been produced
  expect_true(is.list(test))
  
  # Test that a best model was returned
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  # Test that there are no NAs in the best model data
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  
  # Test that the best model data has at least 2 columns
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  # Test that there are no NAs in the output dataset
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 4]))), 0)
  
  # Test that all columns were created in the dataset
  expect_true(ncol(test[[1]]$Dataset) == 19)
  
  # Test that the correct number of models were recorded in the dataset
  expect_equal(maxmodno, nrow(test[[1]]$Dataset))
  
  # Test that data was not randomised
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test[[1]]$Dataset$deltaAICc[1], 1) == -12.7)
  expect_true(test[[1]]$Dataset$WindowOpen[1] == 2 & test[[1]]$Dataset$WindowClose[1] == 2)
  expect_true(round(test[[1]]$Dataset$ModelBeta[1], 1) == 0)
  
})

##############################################################

#Test that an error is returned if there are duplicate climate records but no spatial argument.

test_that("Duplicate climate data returns an error (without spatial)", {
  
  data("Mass", envir = environment())
  data("MassClimate", envir = environment())
  
  MassClimate <- rbind(MassClimate, MassClimate)
  
  expect_error(slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(2, 0), 
                          type = "absolute", refday = c(31, 01), stat = "max", func = "lin", cmissing = FALSE,
                          cinterval = "day"))
  
  
})
