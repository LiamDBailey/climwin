# Test singlewin function #
test_that("singlewin creates an output when cinterval == day", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date,
                    baseline = lm(Mass$Mass~1), range = c(72, 15),
                    stat = "mean", func = "lin",
                    type = "relative", cmissing = FALSE, cinterval = "day")
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -14.2)
  expect_true(round(test$Dataset$ModelAICc, 1) == 274.1)
  
})

# Test that cinterval = week works
test_that("singlewin creates an output when cinterval = week", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date,
                    baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0),
                    stat = "mean", func = "lin",
                    type = "relative", cmissing = FALSE, cinterval = "week")
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from github (7-9-17)
  #N.B. The way we calculate week has changed slightly
  expect_true(round(test$Dataset$deltaAICc, 1) == 1.3)
  expect_true(round(test$Dataset$ModelAICc, 1) == 289.7)
  
})

# Test that cinterval = month works
test_that("singlewin creates an output when cinterval = month", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date,
                    baseline = lm(Mass ~ 1, data = Mass), range = c(1, 0),
                    stat = "mean", func = "lin",
                    type = "relative", cmissing = FALSE, cinterval = "month")
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -4.9)
  expect_true(round(test$Dataset$ModelAICc, 1) == 283.5)
  
})

###############################################################################################

#Test weights work

test_that("singlewin works with weights", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  Mass$weight <- runif(nrow(Mass), 0, 1)
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date,
                    baseline = lm(Mass ~ 1, weights = weight, data = Mass), range = c(72, 15),
                    stat = "mean", func = "lin",
                    type = "relative", cmissing = FALSE, cinterval = "day")
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -25.4)
  expect_true(round(test$Dataset$ModelAICc, 1) == 284)
  
})

test_that("singlewin works with equal weights", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  Mass$weight <- 1
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date,
                    baseline = lm(Mass ~ 1, weights = weight, data = Mass), range = c(72, 15),
                    stat = "mean", func = "lin",
                    type = "relative", cmissing = FALSE, cinterval = "day")
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -14.2)
  expect_true(as.integer(round(test$Dataset$ModelAICc, 1)) == 274)
  
})

###############################################################################################

# Test different settings of cmissing #

# Test when cmissing is method1 and no NA is present#
test_that("No errors return when cmissing method1 and full dataset", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 2), 
                    type = "relative", stat = "max", 
                    func = "lin", cmissing = "method1")
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -2.7)
  expect_true(round(test$Dataset$ModelAICc, 1) == 285.6)
  
})

# Test when cmissing is method1 and no NA is present#
test_that("No errors return when cmissing method2 and full dataset", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 2), 
                    type = "relative", stat = "max", 
                    func = "lin", cmissing = "method2")
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -2.7)
  expect_true(round(test$Dataset$ModelAICc, 1) == 285.6)
  
})

#Test when cmissing is method1 and NA is present#
test_that("No errors return when cmissing method1 with NAs", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  test <- singlewin(xvar = list(Temp = MassClimate2$Temp), 
                    cdate = MassClimate2$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 0), 
                    type = "relative", stat = "max", 
                    func = "lin", cmissing = "method1")
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == 1.9)
  expect_true(round(test$Dataset$ModelAICc, 1) == 290.2)
  
})

#Test when cmissing is method1 and NA is present#
test_that("No errors return when cmissing method2 with NAs", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-300, ]
  test <- singlewin(xvar = list(Temp = MassClimate2$Temp), 
                    cdate = MassClimate2$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 0), 
                    type = "relative", stat = "max", 
                    func = "lin", cmissing = "method2")
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == 1.9)
  expect_true(round(test$Dataset$ModelAICc, 1) == 290.2)
  
})

#Test when cmissing is FALSE and NA is present#
test_that("Errors are returned when cmissing is FALSE with NAs", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  
  # Test that an error is returned
  expect_error(singlewin(xvar = list(Temp = MassClimate2$Temp), 
                         cdate = MassClimate2$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), 
                         range = c(2, 2), 
                         type = "relative", stat = "max", func = "lin", 
                         cmissing = FALSE))
  
})


##########################################################

# Test different types of models #

# Test glm models
test_that("glm models can run in singlewin", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date, 
                    baseline = glm(Mass ~ 1, data = Mass, family = poisson), 
                    range = c(2, 2), 
                    type = "relative", stat = "max", 
                    func = "lin", cmissing = FALSE)
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that singlewin best model is created
  expect_false(is.na(test[[1]][1]))
  
  # Test that best model data contains no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == 1.3)
  expect_true(round(test$Dataset$ModelAICc, 1) == 327.1)
  
})

# Test lmer models
test_that("lmer models can run in singlewin", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = OffspringClimate$Temp), 
                    cdate = OffspringClimate$Date, 
                    bdate = Offspring$Date, 
                    baseline = lmer(Offspring ~ 1 + (1|BirdID), data = Offspring),  
                    range = c(2, 2), type = "relative", 
                    stat = "max", func = "lin", cmissing = FALSE)
  
  # Test that singlewin produces an output
  expect_true(is.list(test))
  
  # Test that singlewin creates an intercept value
  expect_false(is.na(fixef(test[[1]])[1]))
  
  # Test that singlewin creates a beta estimate for climate
  expect_false(is.na(fixef(test[[1]])[2]))
  
  # Test that best model data has no NAs
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data includes at least 2 parameters
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from github (7-9-17)
  #N.B. Have to use different approach as we now force mixed models to use ML
  expect_true(round(test$Dataset$deltaAICc, 1) == -59.3)
  expect_true(round(test$Dataset$ModelAICc, 1) == 4994.6)
  
})

# Test glmer models 
test_that("glmer models can run in singlewin", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  # Warnings created due to convergence issues with such a small data set
  suppressWarnings(test <- singlewin(xvar = list(Temp = OffspringClimate$Temp), 
                                cdate = OffspringClimate$Date, 
                                bdate = Offspring$Date, 
                                baseline = glmer(Offspring ~ 1 + (1|Order), data = Offspring, family = "poisson"),  
                                range = c(2, 2), type = "relative", 
                                stat = "max", func = "lin", cmissing = FALSE))
  
  # Test that climatewin has produced an output
  expect_true(is.list(test))
  
  # Test that glmer model produced an intercept
  expect_false(is.na(fixef(test$BestModel)[1]))
  
  # Test that glmer model produced a beta estimate for climate
  expect_false(is.na(fixef(test$BestModel)[2]))
  
  # Test there are no NA values in best model data
  expect_equal(length(which(is.na(test$BestModelData))), 0)
  
  # Test that best model data has atleast two parameters
  expect_true(ncol(test$BestModelData) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -23.3)
  expect_true(round(test$Dataset$ModelAICc, 1) == 4896.9)
  
})

## COXPH ##

##########################################################

# Test absolute windows #
test_that("Absolute window works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 2), 
                    type = "absolute", refday = c(20, 5), 
                    stat = "max", func = "lin", cmissing = FALSE)
  
  # Test that singlewin produces an object
  expect_true(is.list(test))
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -2.2)
  expect_true(round(test$Dataset$ModelAICc, 1) == 286.2)
  
})

##########################################################

# Test slope stat #
test_that("slope stats work", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 1), 
                    type = "relative", stat = "slope", 
                    func = "lin", cmissing = FALSE)
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that the best model has no NAs
  expect_false(is.na(test[[1]][1]))
  
  # Test that there are no NAs in the best model data 
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has atleast 2 columns
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -1.3)
  expect_true(round(test$Dataset$ModelAICc, 1) == 287.1)
  
})

##########################################################

#Test different functions for fitting climate#

# Test quadratic function
test_that("Quadratic function works in singlewin", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 2), 
                    type = "relative", stat = "max", 
                    func = "quad", cmissing = FALSE)
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that the best model has no NAs
  expect_false(is.na(test[[1]][1]))
  
  # Test that there are no NAs in the best model data 
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has atleast 3 columns
  expect_true(ncol(test[[2]]) >= 3)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -1.0)
  expect_true(round(test$Dataset$ModelAICc, 1) == 287.3)
  
})

#Test cubic function
test_that("Cubic function works in singlewin", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 2), 
                    type = "relative", stat = "max", 
                    func = "cub", cmissing = FALSE)
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that the best model has no NAs
  expect_false(is.na(test[[1]][1]))
  
  # Test that there are no NAs in the best model data 
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has atleast 4 columns
  expect_true(ncol(test[[2]]) >= 4)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == 1.3)
  expect_true(round(test$Dataset$ModelAICc, 1) == 289.7)
  
})

# Test log function
test_that("Log function works in singlewin", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 2), 
                    type = "relative", stat = "max", 
                    func = "log", cmissing = FALSE)
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that the best model has no NAs
  expect_false(is.na(test[[1]][1]))
  
  # Test that there are no NAs in the best model data 
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has atleast 2 columns
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -3.1)
  expect_true(round(test$Dataset$ModelAICc, 1) == 285.3)
  
})

#Test log function#
test_that("Inverse function works in singlewin", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                    cdate = MassClimate$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 2), 
                    type = "relative", stat = "max", 
                    func = "inv", cmissing = FALSE)
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that the best model has no NAs
  expect_false(is.na(test[[1]][1]))
  
  # Test that there are no NAs in the best model data 
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has atleast 2 columns
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -3.1)
  expect_true(round(test$Dataset$ModelAICc, 1) == 285.2)
  
})

################################################################

#Test that an error is returned when you have NAs in the biological data
test_that("singlewin gives error when NAs are present in biological data", {
  
  data(MassClimate, envir = environment())
  Mass <- data.frame(Date = c("01/01/2014", "01/02/2014"), Mass = c(NA, 1))
  
  # Test that an error occurs
  expect_error(singlewin(xvar = list(Temp = MassClimate$Temp), 
                         cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), 
                         range = c(2, 2), 
                         type = "relative", stat = "max", 
                         func = "lin", cmissing = FALSE))
  
})

################################################################

#Test spatial replication#
test_that("spatial replication works with singlewin", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  
  test <- singlewin(xvar = list(Temp = Clim$Temp), 
                    cdate = Clim$Date, bdate = Mass$Date, 
                    baseline = lm(Mass ~ 1, data = Mass), 
                    range = c(2, 2), 
                    type = "relative", stat = "max", 
                    func = "lin", cmissing = FALSE,
                    spatial = list(Mass$Plot, Clim$Plot))
  
  # Test that singlewin produces an output
  expect_true(is.list(test))  
  
  # Test that the best model has no NAs
  expect_false(is.na(test[[1]][1]))
  
  # Test that there are no NAs in the best model data 
  expect_equal(length(which(is.na(test[[2]]))), 0)
  
  # Test that best model data has atleast 2 columns
  expect_true(ncol(test[[2]]) >= 2)
  
  #Test that values match those from previous R version
  expect_true(round(test$Dataset$deltaAICc, 1) == -2.7)
  expect_true(round(test$Dataset$ModelAICc, 1) == 285.6)
  
})
