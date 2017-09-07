# Test outcomes of convertdate #

#############################################

# Test convertdate where cinterval = "day"
test_that("convertdate works for days", {

data(Mass, envir = environment())
data(MassClimate, envir = environment())

MassClimatedup         <- MassClimate
MassClimatedup[17533,] <- MassClimatedup[17532,]

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    cinterval = "day", type = "relative", spatial = NULL, upper = NA, lower = NA, stat = "mean")

# Test that no NA dates were produced
expect_equal(length(which(is.na(test))), 0)

# Test that the first climate date value is 1
expect_equal(test$cintno[1], 1)

# Test that the number of climate date values produced is equal to the original dataset
expect_equal(length(test$cintno), length(test$xvar))

# Test that the largest biological date is encompassed in the climate data
expect_true((max(test$bintno) %in% test$cintno))

# Test for an error when cintreval is incorrect
expect_error(convertdate(bdate = Mass$Date, cdate = MassClimate$Date,
                         cinterval = "R", type = "relative"))

# Test for an error when there is duplicate climate data
expect_error(convertdate(bdate = Mass$Date, cdate = MassClimatedup$Date,
                         cinterval = "day", type = "relative"))

#Compare to results from previous R version
expect_true(round(mean(test$xvar), 1) == 9.8)
expect_true(min(test$bintno) == 493)
expect_true(max(test$bintno) == 17290)
expect_true(max(test$cintno) == 17532)

})

# Test convertdate with cinterval = week
test_that("convertdate works for weeks", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    cinterval = "week", type = "relative", spatial = NULL, upper = NA, lower = NA, stat = "mean")

# Test that no NA dates were produced
expect_equal(length(which(is.na(test))), 0)

# Test that the first climate date value is 1
expect_equal(test$cintno[1], 1)

# Test that the length of climate values is equal the original climate dataset
expect_equal(length(test$cintno), length(test$xvar))

# Test that biological dates are in climate dates
expect_true((max(test$bintno) %in% test$cintno))

#Compare to results from previous R version
expect_true(round(mean(test$xvar), 1) == 9.9)
expect_true(min(test$bintno) == 71)
expect_true(max(test$bintno) == 2462)
expect_true(max(test$cintno) == 2496)

})

# Test convertdate for cinterval = "month"
test_that("convertdate works for months", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    cinterval = "month", type = "relative", spatial = NULL, upper = NA, lower = NA, stat = "mean")

# Test that no NA dates were produced
expect_equal(length(which(is.na(test))), 0)

# Test that the first climate date value is 1
expect_equal(test$cintno[1], 1)

# Test that the length of climate values is equal the original climate dataset
expect_equal(length(test$cintno), length(test$xvar))

# Test that biological dates are in climate dates
expect_true((max(test$bintno) %in% test$cintno))

#Compare to results from previous R version
expect_true(round(mean(test$xvar), 1) == 9.8)
expect_true(min(test$bintno) == 17)
expect_true(max(test$bintno) == 569)
expect_true(max(test$cintno) == 576)

})

######################################################################

# Test that convertdate works with two climate variables, cinterval = "day"
test_that("convertdate works for days and two climate variables (i.e. xvar2 != NULL)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    xvar2 = MassClimate$Rain, cinterval = "day", type = "relative", cross = TRUE,
                    spatial = NULL, upper = NA, lower = NA, stat = "mean")

# Test that no NA dates were produced
expect_equal(length(which(is.na(test))), 0)

# Test that the first climate date value is 1
expect_equal(test$cintno[1], 1)

# Test that the length of climate values is equal the original climate dataset
expect_equal(length(test$cintno), length(test$xvar))

# Test that the length of climate values is equal the original second climate dataset
expect_equal(length(test$cintno), length(test$xvar2))

# Test that biological dates are in climate dates
expect_true((max(test$bintno) %in% test$cintno))

#Compare to results from previous R version
expect_true(round(mean(test$xvar), 1) == 9.8)
expect_true(round(mean(test$xvar2), 1) == 2.6)
expect_true(min(test$bintno) == 493)
expect_true(max(test$bintno) == 17290)
expect_true(max(test$cintno) == 17532)

})

# Test that convertdate works with two climate variables, cinterval = "week"
test_that("convertdate works (week, xvar2)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    xvar2 = MassClimate$Rain, cinterval = "week", type = "relative", cross = TRUE,
                    spatial = NULL, upper = NA, lower = NA, stat = "mean")

# Test that no NA dates were produced
expect_equal(length(which(is.na(test))), 0)

# Test that the first climate date value is 1
expect_equal(test$cintno[1], 1)

# Test that the length of climate values is equal the original climate dataset
expect_equal(length(test$cintno), length(test$xvar))

# Test that the length of climate values is equal the original second climate dataset
expect_equal(length(test$cintno), length(test$xvar2))

# Test that biological dates are in climate dates
expect_true((max(test$bintno) %in% test$cintno))

#Compare to results from previous R version
expect_true(round(mean(test$xvar), 1) == 9.8)
expect_true(round(mean(test$xvar2), 1) == 2.6)
expect_true(min(test$bintno) == 58)
expect_true(max(test$bintno) == 2496)
expect_true(max(test$cintno) == 2544)

})

# Test that convertdate works with two climate variables, cinterval = "month"
test_that("convertdate works (month, variable)", {
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    xvar2 = MassClimate$Rain, cinterval = "month", type = "relative", cross = TRUE,
                    spatial = NULL, upper = NA, lower = NA, stat = "mean")

# Test that no NA dates were produced
expect_equal(length(which(is.na(test))), 0)

# Test that the first climate date value is 1
expect_equal(test$cintno[1], 1)

# Test that the length of climate values is equal the original climate dataset
expect_equal(length(test$cintno), length(test$xvar))

# Test that the length of climate values is equal the original second climate dataset
expect_equal(length(test$cintno), length(test$xvar2))

# Test that biological dates are in climate dates
expect_true((max(test$bintno) %in% test$cintno))

#Compare to results from previous R version
expect_true(round(mean(test$xvar), 1) == 9.8)
expect_true(round(mean(test$xvar2), 1) == 2.6)
expect_true(min(test$bintno) == 17)
expect_true(max(test$bintno) == 569)
expect_true(max(test$cintno) == 576)

})
