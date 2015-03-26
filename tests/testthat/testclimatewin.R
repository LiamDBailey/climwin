# Test the outcomes of climatewin #

#Need to vary:
# baseline model type: lme4, glmer
# CINTERVAL: D, W, M

# Test that climatewin has created a BestModel, BestModelData and WindowOutput
# Expect that an object BestModel exists
# Expect that the coefficients of BestModel are not NAs
# Check there are no NAs in BestModelData or WindowOutput
# Check there are at least 2 columns in BestModelData
# Check there are at least 12 columns in WindowOutput
# Check that the number of rows in the same as the number of windows
# Check that randomised is "no"
test_that("climatewin produces the right output", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 2
  closest = 2
  STAT = "max"
  
  test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "L", CMISSING = FALSE)
  
  MaxMODNO <- 0
  duration <- (furthest-closest) + 1
  for (m in closest:furthest){
    for (n in 1:duration){
      if ( (m-n) >= (closest - 1)){  
        if (STAT != "slope" || n > 1){
          MaxMODNO <- MaxMODNO + 1
        }
      }
    }
  }
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]])[1]))
  
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
  expect_equal(length(which(is.na(test[[3]]))), 0)
  expect_true(ncol(test[[3]]) >= 12)
  expect_equal(MaxMODNO, nrow(test[[3]]))
  expect_true((test[[3]]["Randomised"])[1, ] == "no")
  
})

##########################################################

# Test different settings of CMISSING #

#When CMISSING is TRUE and no NA is present#
test_that("No errors return when CMISSING TRUE and full dataset", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "L", CMISSING=TRUE)
  
  expect_true(is.list(test))

})

#When CMISSING is TRUE and NA is present#
test_that("No errors return when CMISSING TRUE with NAs", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  test <- climatewin(Xvar = MassClimate2$Temp, CDate = MassClimate2$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 0, 
                     FIXED = FALSE, STAT = "max", FUNC = "L", CMISSING = TRUE)
  
  expect_true(is.list(test))
  
})

#When CMISSING is FALSE and NA is present#
test_that("No errors return when CMISSING FALSE with NAs", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimate2 <- MassClimate[-491, ]
  expect_error(climatewin(Xvar = MassClimate2$Temp, CDate = MassClimate2$Date, BDate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                          FIXED = FALSE, STAT = "max", FUNC = "L", 
                          CMISSING=FALSE))
  
  })


##########################################################

# Test different types of models #

# Test glm models #
test_that("glm models can run", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = glm(Mass ~ 1, data = Mass, family = poisson), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "L", CMISSING=FALSE)
  
  expect_true(is.list(test))
  expect_false(is.na((test[[1]])[1]))
  
  expect_equal(length(which(is.na(test[[2]]))), 0)
  expect_true(ncol(test[[2]]) >= 2)
  
})

test_that("lmer models can run", {
  
  data(Offspring, envir = environment())
  data(OffspringClimate, envir = environment())
  
  test <- climatewin(Xvar = OffspringClimate$Temp, CDate = OffspringClimate$Date, 
                     BDate = Offspring$Date, 
                     baseline = lmer(Offspring ~ 1 + (1|BirdID), data = Offspring),  
                     furthest = 2, closest = 2, FIXED = FALSE, 
                     STAT = "max", FUNC = "L", CMISSING=FALSE)
  
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
  
  test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = TRUE, cutoff.day = 20, cutoff.month = 5, 
                     STAT = "max", FUNC = "L", CMISSING=FALSE)
  
  expect_true(is.list(test))
  
})

##########################################################

# Test slope stat #
test_that("slope stats work", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 1, 
                     FIXED = FALSE, STAT = "slope", FUNC = "L", CMISSING=FALSE)
  
  expect_true(is.list(test))
  expect_true((test[[3]]["Statistics"])[1] == "slope")
  
})

##########################################################

#Test different functions for fitting climate#

#Test quadratic function#
test_that("Quadratic function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "Q", CMISSING=FALSE)
  
  expect_true(is.list(test))
  expect_true((test[[3]]["Function"])[1] == "Q")
  expect_false(is.na(test[[3]]["ModelBetaQ"]))
  expect_true(ncol(test[[3]]) >= 13)
  
})

#Test cubic function#
test_that("Cubic function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "C", CMISSING=FALSE)
  
  expect_true(is.list(test))
  expect_true((test[[3]]["Function"])[1] == "C")
  expect_false(is.na(test[[3]]["ModelBetaQ"])) 
  expect_false(is.na(test[[3]]["ModelBetaC"]))
  expect_true(ncol(test[[3]]) >= 14)
 
})

#Test log function#
test_that("Log function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "LOG", CMISSING=FALSE)
  
  expect_true(is.list(test))
  expect_true((test[[3]]["Function"])[1] == "LOG")
  expect_true(ncol(test[[3]]) >= 12)
  
})

#Test log function#
test_that("Inverse function works", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                     baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                     FIXED = FALSE, STAT = "max", FUNC = "I", CMISSING=FALSE)
  
  expect_true(is.list(test))
  expect_true((test[[3]]["Function"])[1] == "I")
  expect_true(ncol(test[[3]]) >= 12)
  
})

##########################################################

#Test different CINTERVAL values#

#Test CINTERVAL = W
test_that("Weekly interval works", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                   baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                   FIXED = FALSE, STAT = "max", FUNC = "L",
                   CMISSING=FALSE, CINTERVAL = "W")

expect_true(is.list(test))
expect_true(ncol(test[[3]]) >= 12)

})

#Test CINTERVAL = M
test_that("Monthly interval works", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())
  
test <- climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                   baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                   FIXED = FALSE, STAT = "max", FUNC = "L",
                   CMISSING=FALSE, CINTERVAL = "M")
  
expect_true(is.list(test))
expect_true(ncol(test[[3]]) >= 12)
  
})

################################################################

#Error when you have NAs in the biological data
test_that("climatewin gives error when NAs are present in biological data", {
  
  data(MassClimate, envir = environment())
  Mass <- data.frame(Date = c("01/01/2014", "01/02/2014"), Mass = c(NA, 1))
  
  expect_error(climatewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 2, 
                          FIXED = FALSE, STAT = "max", FUNC = "L",
                          CMISSING=FALSE, CINTERVAL = "D"))
  
})