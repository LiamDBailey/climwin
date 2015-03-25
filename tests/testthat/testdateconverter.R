# Test outcomes of DateConverter #

# Test that no NAs are produced #
# Test that CIntNo starts at 1 #
# Test that BIntNo and CIntNo have the same range #
# Test that error is returned when CINTERVAL is wrong #
# Test that error is returned when there are duplicate climate days #
# Test that the length of CIntNo and Xvar are equal #
# Test for all possible combos of CINTERVAL and cross TRUE or FALSE #
test_that("DateConverter works (D False)", {

data(Mass, envir = environment())
data(MassClimate, envir = environment())

MassClimatedup <- MassClimate
MassClimatedup[17533,] <- MassClimatedup[17532,]

test <- DateConverter(BDate = Mass$Date, CDate = MassClimate$Date, Xvar = MassClimate$Temp,
                      CINTERVAL = "D", FIXED = FALSE)

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_true((max(test$BIntNo) %in% test$CIntNo))
expect_error(DateConverter(BDate = Mass$Date, CDate = MassClimate$Date,
                           CINTERVAL = "R", FIXED = FALSE))
expect_error(DateConverter(BDate = Mass$Date, CDate = MassClimatedup$Date,
                           CINTERVAL = "D", FIXED = FALSE))

})

test_that("DateConverter works (W False)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(BDate = Mass$Date, CDate = MassClimate$Date, Xvar = MassClimate$Temp,
                      CINTERVAL = "W", FIXED = FALSE)

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})

test_that("DateConverter works (M False)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(BDate = Mass$Date, CDate = MassClimate$Date, Xvar = MassClimate$Temp,
                      CINTERVAL = "M", FIXED = FALSE)

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})

test_that("DateConverter works (D True)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(BDate = Mass$Date, CDate = MassClimate$Date, Xvar = MassClimate$Temp,
                      Xvar2 = MassClimate$Rain, CINTERVAL = "D", FIXED = FALSE, cross = TRUE)

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_equal(length(test$CIntNo), length(test$Xvar2))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})

test_that("DateConverter works (W True)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(BDate = Mass$Date, CDate = MassClimate$Date, Xvar = MassClimate$Temp,
                      Xvar2 = MassClimate$Rain, CINTERVAL = "W", FIXED = FALSE, cross = TRUE)

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_equal(length(test$CIntNo), length(test$Xvar2))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})

test_that("DateConverter works (M True)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(BDate = Mass$Date, CDate = MassClimate$Date, Xvar = MassClimate$Temp,
                      Xvar2 = MassClimate$Rain, CINTERVAL = "M", FIXED = FALSE, cross = TRUE)

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_equal(length(test$CIntNo), length(test$Xvar2))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})
