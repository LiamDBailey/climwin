# Test outcomes of DateConverter #

# Test that no NAs are produced #
# Test that CIntNo starts at 1 #
# Test that BIntNo and CIntNo have the same range #
# Test that error is returned when Cinterval is wrong #
# Test that error is returned when there are duplicate climate days #
# Test that the length of CIntNo and Xvar are equal #
# Test for all possible combos of Cinterval and cross TRUE or FALSE #
test_that("DateConverter works (D False)", {

data(Mass, envir = environment())
data(MassClimate, envir = environment())

MassClimatedup <- MassClimate
MassClimatedup[17533,] <- MassClimatedup[17532,]

test <- DateConverter(Bdate = Mass$Date, Cdate = MassClimate$Date, Xvar = MassClimate$Temp,
                      Cinterval = "day", type = "variable")

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_true((max(test$BIntNo) %in% test$CIntNo))
expect_error(DateConverter(Bdate = Mass$Date, Cdate = MassClimate$Date,
                           Cinterval = "R", type = "variable"))
expect_error(DateConverter(Bdate = Mass$Date, Cdate = MassClimatedup$Date,
                           Cinterval = "day", type = "variable"))

})

test_that("DateConverter works (W False)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(Bdate = Mass$Date, Cdate = MassClimate$Date, Xvar = MassClimate$Temp,
                      Cinterval = "week", type = "variable")

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})

test_that("DateConverter works (M False)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(Bdate = Mass$Date, Cdate = MassClimate$Date, Xvar = MassClimate$Temp,
                      Cinterval = "month", type = "variable")

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})

test_that("DateConverter works (D True)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(Bdate = Mass$Date, Cdate = MassClimate$Date, Xvar = MassClimate$Temp,
                      Xvar2 = MassClimate$Rain, Cinterval = "day", type = "variable", cross = TRUE)

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_equal(length(test$CIntNo), length(test$Xvar2))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})

test_that("DateConverter works (W True)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(Bdate = Mass$Date, Cdate = MassClimate$Date, Xvar = MassClimate$Temp,
                      Xvar2 = MassClimate$Rain, Cinterval = "week", type = "variable", cross = TRUE)

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_equal(length(test$CIntNo), length(test$Xvar2))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})

test_that("DateConverter works (M True)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- DateConverter(Bdate = Mass$Date, Cdate = MassClimate$Date, Xvar = MassClimate$Temp,
                      Xvar2 = MassClimate$Rain, Cinterval = "month", type = "variable", cross = TRUE)

expect_equal(length(which(is.na(test))), 0)
expect_equal(test$CIntNo[1], 1)
expect_equal(length(test$CIntNo), length(test$Xvar))
expect_equal(length(test$CIntNo), length(test$Xvar2))
expect_true((max(test$BIntNo) %in% test$CIntNo))

})
