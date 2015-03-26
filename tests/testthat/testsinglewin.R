# Test singlewin function #

# Test that singlewin outputs SingleBestModel and SingleBestModelData
# Check that coefficients of SingleBestModel are not NA
# Check that there are no NAs in SingleBestModelData
# Check that SingleBestModelData has at least 2 columns

test_that("singlewin creates an output", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
                    baseline = lm(Mass$Mass~1), furthest = 72, closest = 15,
                    STAT = "mean", FUNC = "L",
                    FIXED = FALSE, CMISSING = FALSE, CINTERVAL = "D")
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

test_that("CINTERVAL W works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
                    baseline = lm(Mass$Mass~1), furthest = 16, closest = 15,
                    STAT = "mean", FUNC = "L",
                    FIXED = FALSE, CMISSING = FALSE, CINTERVAL = "W")
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

test_that("CINTERVAL M works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
                    baseline = lm(Mass$Mass~1), furthest = 16, closest = 15,
                    STAT = "mean", FUNC = "L",
                    FIXED = FALSE, CMISSING = FALSE, CINTERVAL = "M")
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

###############################################################################################

# Test different settings of CMISSING #

#When CMISSING is TRUE and no NA is present#
test_that("No errors return when CMISSING TRUE and full dataset", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "L", CMISSING = TRUE)
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

#When CMISSING is TRUE and NA is present#
test_that("No errors return when CMISSING TRUE with NAs", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  test <- singlewin(Xvar = MassClimate2$Temp, CDate = MassClimate2$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 0, 
                     FIXED = FALSE, STAT = "max", FUNC = "L", CMISSING = TRUE)
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

#When CMISSING is FALSE and NA is present#
test_that("No errors return when CMISSING FALSE with NAs", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  expect_error(singlewin(Xvar = MassClimate2$Temp, CDate = MassClimate2$Date, BDate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                          FIXED = FALSE, STAT = "max", FUNC = "L", 
                          CMISSING = FALSE))
  
})


##########################################################

# Test different types of models #

# Test glm models #
test_that("glm models can run", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = glm(Mass ~ 1, data = Mass, family = poisson), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "L", CMISSING = FALSE)
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]])[1]))
  
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

test_that("lmer models can run", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  test <- singlewin(Xvar = OffspringClimate$Temp, CDate = OffspringClimate$Date, 
                     BDate = Offspring$Date, 
                     baseline = lmer(Offspring ~ 1 + (1|BirdID), data = Offspring),  
                     furthest = 2, closest = 2, FIXED = FALSE, 
                     STAT = "max", FUNC = "L", CMISSING = FALSE)
  
  expect_true(is.list(test))
  expect_false(is.na(fixef(test[[1]])[1]))
  expect_false(is.na(fixef(test[[1]])[2]))
  
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

##########################################################

# Test fixed and variable #

# Test fixed window#
test_that("Fixed window works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = TRUE, cutoff.day = 20, cutoff.month = 5, 
                     STAT = "max", FUNC = "L", CMISSING = FALSE)
  
  expect_true(is.list(test))
  
})

##########################################################

# Test slope stat #
test_that("slope stats work", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 1, 
                     FIXED = FALSE, STAT = "slope", FUNC = "L", CMISSING = FALSE)
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

##########################################################

#Test different functions for fitting climate#

#Test quadratic function#
test_that("Quadratic function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "Q", CMISSING = FALSE)
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

#Test cubic function#
test_that("Cubic function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "C", CMISSING = FALSE)
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

#Test log function#
test_that("Log function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "LOG", CMISSING = FALSE)
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

#Test log function#
test_that("Inverse function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "I", CMISSING = FALSE)
  
  expect_true(is.list(test))  
  expect_false(is.na(test[[1]][1]))
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

################################################################

#Error when you have NAs in the biological data
test_that("singlewin gives error when NAs are present in biological data", {
  
  data(MassClimate, envir = environment())
  Mass <- data.frame(Date = c("01/01/2014", "01/02/2014"), Mass = c(NA, 1))
  
  expect_error(singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass), 
            furthest = 2, closest = 2, FIXED = FALSE, STAT = "max", FUNC = "L", CMISSING = FALSE))
  
})
