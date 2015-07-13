# Test the outcomes of climatewin #

#Need to vary:
# baseline model type: glm, lme4, glmer
# CINTERVAL: D, W, M
# STAT: mean, min, max, slope
# FUNC: L, Q, C, I, LOG
# uppers: range
# lowers: range
# thresh: TRUE or FALSE

# Test that climatewin has created a BestModel, BestModelData and WindowOutput
# Expect that an object BestModel exists
# Expect that the coefficients of BestModel are not NAs
# Check there are no NAs in BestModelData or WindowOutput
# Check there are at least 2 columns in BestModelData
# Check there are at least 15 columns in WindowOutput
# Check that the number of rows in the same as the number of windows
# Check that randomised is "no"

##########################################################

# Test regular output

test_that("climatewin produces the right output", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 2
  closest = 2
  
  test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING = FALSE)
  
  duration  <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 5]))), 0)
  expect_true(ncol(test[[1]]$Dataset) >= 15)
  expect_equal(MaxMODNO, nrow(test[[1]]$Dataset))
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
})

test_that("climatewin produces multiple combos", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- climatewin(Xvars = list(Temp = MassClimate$Temp, Rain = MassClimate$Rain), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 1, closest = 0, 
                     FIXED = FALSE, STATS = c("max", "min"), FUNCS = c("L", "Q"), CMISSING = FALSE)
  
  expect_equal(nrow(test$combos), 8)
  
})

##########################################################

# Test that uppers and lowers work

test_that("climatewin produces binary values with uppers and thresh = TRUE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- climatewin(Xvars = list(Temp = MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 1, closest = 0, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING = FALSE,
                     uppers = 10, thresh = TRUE)
  
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  expect_equal(max(test[[1]]$BestModelData$climate), 1)
  
})

test_that("climatewin produces non-binary values with uppers and thresh = FALSE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- climatewin(Xvars = list(Temp = MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 1, closest = 0, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING = FALSE,
                     uppers = 10, thresh = FALSE)
  
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  expect_true(max(test[[1]]$BestModelData$climate)>1)
  
})

test_that("climatewin produces non-binary values with lowers and thresh = FALSE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- climatewin(Xvars = list(Temp = MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 1, closest = 0, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING = FALSE,
                     lowers = 10, uppers = 15, thresh = FALSE)
  
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  expect_true(max(test[[1]]$BestModelData$climate)>1)
  
})

test_that("climatewin produces binary values with lowers and thresh = TRUE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- climatewin(Xvars = list(Temp = MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 1, closest = 0, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING = FALSE,
                     lowers = 10, thresh = TRUE)
  
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  expect_equal(max(test[[1]]$BestModelData$climate), 1)
  
})

test_that("climatewin produces binary values with lowers/uppers and thresh = TRUE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- climatewin(Xvars = list(Temp = MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 1, closest = 0, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING = FALSE,
                     lowers = 10, uppers = 15, thresh = TRUE)
  
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  expect_equal(max(test[[1]]$BestModelData$climate), 1)
  
})

test_that("climatewin produces non-binary values with lowers/uppers and thresh = FALSE", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 1
  closest = 0
  
  test <- climatewin(Xvars = list(Temp = MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 1, closest = 0, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING = FALSE,
                     lowers = 10, uppers = 15, thresh = FALSE)
  
  expect_equal(min(test[[1]]$BestModelData$climate), 0)
  expect_true(max(test[[1]]$BestModelData$climate)>1)
  
})

##########################################################

# Test different settings of CMISSING #

#When CMISSING is TRUE and no NA is present#
test_that("No errors return when CMISSING TRUE and full dataset", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING=TRUE)
  
  expect_true(is.list(test))

})

#When CMISSING is TRUE and NA is present#
test_that("No errors return when CMISSING TRUE with NAs", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  test <- climatewin(Xvars = list(MassClimate2$Temp), CDate = MassClimate2$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 0, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING = TRUE)
  
  expect_true(is.list(test))
  
})

#When CMISSING is FALSE and NA is present#
#Test that an error occurs
#Test that object Missing is made
#Test that object Missing has length 1 (only 1 Date has been removed)

test_that("Error returned when CMISSING FALSE with NAs, CINTERVAL = D", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  expect_error(climatewin(Xvars = list(MassClimate2$Temp), CDate = MassClimate2$Date, BDate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                          FIXED = FALSE, STATS = "max", FUNCS = "L", 
                          CMISSING=FALSE))
  
  expect_true(exists("Missing"))
  expect_equal(length(Missing), 1)
  rm("Missing", envir = .GlobalEnv)
  
  })

test_that("Error returned when CMISSING FALSE with NAs, CINTERVAL = W", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  expect_error(climatewin(Xvars = list(MassClimate2$Temp), CDate = MassClimate2$Date, BDate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 0, 
                          FIXED = FALSE, STATS = "max", FUNCS = "L", CINTERVAL = "W",
                          CMISSING=FALSE))
  
  expect_true(exists("Missing"))
  expect_equal(length(Missing), 1)
  rm("Missing", envir = .GlobalEnv)
  
})

test_that("Error returned when CMISSING FALSE with NAs, CINTERVAL = M", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  expect_error(climatewin(Xvars = list(MassClimate2$Temp), CDate = MassClimate2$Date, BDate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), furthest = 1, closest = 0, 
                          FIXED = FALSE, STATS = "max", FUNCS = "L", CINTERVAL = "M",
                          CMISSING=FALSE))
  
  expect_true(exists("Missing"))
  expect_equal(length(Missing), 1)
  rm("Missing", envir = .GlobalEnv)
  
})

##########################################################

# Test different types of models #

# Test glm models #
test_that("glm models can run", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = glm(Mass ~ 1, data = Mass, family = poisson), furthest = 2, closest = 2, 
                     FIXED = FALSE, STATS = "max", FUNCS = "L", CMISSING=FALSE)
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
})

test_that("lmer models can run", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  test <- climatewin(Xvars = list(OffspringClimate$Temp), CDate = OffspringClimate$Date, 
                     BDate = Offspring$Date, 
                     baseline = lmer(Offspring ~ 1 + (1|BirdID), data = Offspring),  
                     furthest = 2, closest = 2, FIXED = FALSE, 
                     STATS = "max", FUNCS = "L", CMISSING=FALSE)
  
  expect_true(is.list(test))
  expect_false(is.na(fixef(test[[1]]$BestModel)[1]))
  expect_false(is.na(fixef(test[[1]]$BestModel)[2]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
})

test_that("glmer models can run", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  suppressWarnings(test <- climatewin(Xvars = list(OffspringClimate$Temp), CDate = OffspringClimate$Date, 
                     BDate = Offspring$Date, 
                     baseline = glmer(Offspring ~ 1 + (1|Order), data = Offspring, family = "poisson"),  
                     furthest = 1, closest = 0, FIXED = FALSE, 
                     STATS = "max", FUNCS = "L", CMISSING=FALSE))
  
  expect_true(is.list(test))
  expect_false(is.na(fixef(test[[1]]$BestModel)[1]))
  expect_false(is.na(fixef(test[[1]]$BestModel)[2]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
})

##########################################################

# Test fixed and variable windows#

# Test fixed window#
test_that("Fixed window works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = TRUE, cutoff.day = 20, cutoff.month = 5, 
                     STATS = "max", FUNCS = "L", CMISSING=FALSE)
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
})

##########################################################

# Test slope stat #
test_that("slope stats work", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 1, 
                     FIXED = FALSE, STATS = "slope", FUNCS = "L", CMISSING=FALSE)

  expect_true(is.list(test))
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
})

test_that("slope and LOG return error", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  expect_error(cliamtewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date,
                          BDate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                          furthest = 2, closest = 1, FIXED = FALSE, STATS = "slope",
                          FUNCS = "LOG"))  
  
})

test_that("slope and I return error", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  expect_error(cliamtewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date,
                          BDate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass),
                          furthest = 2, closest = 1, FIXED = FALSE, STATS = "slope",
                          FUNCS = "I"))  
  
})

##########################################################

#Test different functions for fitting climate#

#Test quadratic function#
test_that("Quadratic function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STATS = "max", FUNCS = "Q", CMISSING=FALSE)
  
  furthest = 2
  closest = 2
  duration  <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 3)
  
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 6]))), 0)
  expect_true(test[[1]]$Dataset[, 12] == "Q")
  expect_true(ncol(test[[1]]$Dataset) >= 15)
  expect_equal(MaxMODNO, nrow(test[[1]]$Dataset))
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
})

#Test cubic function#
test_that("Cubic function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STATS = "max", FUNCS = "C", CMISSING=FALSE)
  
  furthest = 2
  closest = 2
  duration  <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 4)
  
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 7]))), 0)
  expect_true(test[[1]]$Dataset[, 12] == "C")
  expect_true(ncol(test[[1]]$Dataset) >= 15)
  expect_equal(MaxMODNO, nrow(test[[1]]$Dataset))
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
 
})

#Test log function#
test_that("Log function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STATS = "max", FUNCS = "LOG", CMISSING=FALSE)
  
  furthest = 2
  closest = 2
  duration  <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 5]))), 0)
  expect_true(test[[1]]$Dataset[, 12] == "LOG")
  expect_true(ncol(test[[1]]$Dataset) >= 15)
  expect_equal(MaxMODNO, nrow(test[[1]]$Dataset))
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
})

#Test inverse function#
test_that("Inverse function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STATS = "max", FUNCS = "I", CMISSING=FALSE)
  
  furthest = 2
  closest = 2
  duration  <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]]$BestModel)[1]))
  
  expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
  expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
  expect_equal(length(which(is.na(test[[1]]$Dataset[, 5]))), 0)
  expect_true(test[[1]]$Dataset[, 12] == "I")
  expect_true(ncol(test[[1]]$Dataset) >= 15)
  expect_equal(MaxMODNO, nrow(test[[1]]$Dataset))
  expect_true((test[[1]]$Dataset["Randomised"])[1, ] == "no")
  
})

##########################################################

#Test different CINTERVAL values#

#Test CINTERVAL = W
test_that("Weekly interval works", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                   baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                   FIXED = FALSE, STATS = "max", FUNCS = "L",
                   CMISSING=FALSE, CINTERVAL = "W")

expect_true(is.list(test))
expect_false(is.na((test[[1]]$BestModel)[1]))

expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
expect_true(ncol(test[[1]]$BestModelData) >= 2)

})

#Test CINTERVAL = M
test_that("Monthly interval works", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())
  
test <- climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                   baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                   FIXED = FALSE, STATS = "max", FUNCS = "L",
                   CMISSING=FALSE, CINTERVAL = "M")

expect_true(is.list(test))
expect_false(is.na((test[[1]]$BestModel)[1]))

expect_equal(length(which(is.na(test[[1]]$BestModelData))), 0)
expect_true(ncol(test[[1]]$BestModelData) >= 2)
  
})

################################################################

#Error when you have NAs in the biological data
test_that("climatewin gives error when NAs are present in biological data", {
  
  data(MassClimate, envir = environment())
  Mass <- data.frame(Date = c("01/01/2014", "01/02/2014"), Mass = c(NA, 1))
  
  expect_error(climatewin(Xvars = list(MassClimate$Temp), CDate = MassClimate$Date, BDate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                          FIXED = FALSE, STATS = "max", FUNCS = "L",
                          CMISSING=FALSE, CINTERVAL = "D"))
  
})