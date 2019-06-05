# Test outcomes of convertdate #

#############################################

# Test convertdate where cinterval = "day"
test_that("convertdate works for days and relative", {
  
current_option <- getOption("scipen")
options(scipen = 0)

data(Mass, envir = environment())
data(MassClimate, envir = environment())

MassClimatedup         <- MassClimate
MassClimatedup[17533,] <- MassClimatedup[17532,]

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    cinterval = "day", type = "relative", spatial = NULL, upper = NA, lower = NA)

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

options(scipen = current_option)

})

test_that("convertdate works for days and absolute", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  MassClimatedup         <- MassClimate
  MassClimatedup[17533,] <- MassClimatedup[17532,]
  
  test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                      cohort = lubridate::year(as.Date(Mass$Date, format = "%d/%m/%Y")), refday = c(1, 6),
                      cinterval = "day", type = "absolute", spatial = NULL, upper = NA, lower = NA)
  
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
                           cinterval = "R"))
  
  # Test for an error when there is duplicate climate data
  expect_error(convertdate(bdate = Mass$Date, cdate = MassClimatedup$Date,
                           cinterval = "day"))
  
  #Compare to results from previous R version
  expect_true(round(mean(test$xvar), 1) == 9.8)
  expect_true(min(test$bintno) == 517)
  expect_true(max(test$bintno) == 17319)
  expect_true(max(test$cintno) == 17532)
  
  options(scipen = current_option)
  
})

#######################################################################################

# Test convertdate with cinterval = week
test_that("convertdate works for week and relative", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    cinterval = "week", type = "relative", spatial = NULL, upper = NA, lower = NA)

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

options(scipen = current_option)

})

# Test convertdate with cinterval = week
test_that("convertdate works for week and absolute", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                      cohort = lubridate::year(as.Date(Mass$Date, format = "%d/%m/%Y")), refday = c(1, 6),
                      cinterval = "week", type = "absolute", spatial = NULL, upper = NA, lower = NA)
  
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
  expect_true(min(test$bintno) == 74)
  expect_true(max(test$bintno) == 2466)
  expect_true(max(test$cintno) == 2496)
  
  options(scipen = current_option)
  
})

##############################################################################

# Test convertdate for cinterval = "month"
test_that("convertdate works for month and relative", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    cinterval = "month", type = "relative", spatial = NULL, upper = NA, lower = NA)

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

options(scipen = current_option)

})

# Test convertdate for cinterval = "month"
test_that("convertdate works for month and absolute", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                      cohort = lubridate::year(as.Date(Mass$Date, format = "%d/%m/%Y")), refday = c(1, 6),
                      cinterval = "month", type = "absolute", spatial = NULL, upper = NA, lower = NA)
  
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
  expect_true(min(test$bintno) == 18)
  expect_true(max(test$bintno) == 570)
  expect_true(max(test$cintno) == 576)
  
  options(scipen = current_option)
  
})

######################################################################

# Test that convertdate works with two climate variables, cinterval = "day"
test_that("convertdate works (days, xvar2, relative).", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    xvar2 = MassClimate$Rain, cinterval = "day", type = "relative", cross = TRUE,
                    spatial = NULL, upper = NA, lower = NA)

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

options(scipen = current_option)

})

test_that("convertdate works (days, xvar2, absolute)", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                      cohort = lubridate::year(as.Date(Mass$Date, format = "%d/%m/%Y")), refday = c(1, 6),
                      xvar2 = MassClimate$Rain, cinterval = "day", type = "absolute", cross = TRUE,
                      spatial = NULL, upper = NA, lower = NA)
  
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
  expect_true(min(test$bintno) == 517)
  expect_true(max(test$bintno) == 17319)
  expect_true(max(test$cintno) == 17532)
  
  options(scipen = current_option)
  
})

#############################################################

# Test that convertdate works with two climate variables, cinterval = "week"
test_that("convertdate works (week, xvar2, relative)", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    xvar2 = MassClimate$Rain, cinterval = "week", type = "relative", cross = TRUE,
                    spatial = NULL, upper = NA, lower = NA)

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

options(scipen = current_option)

})

test_that("convertdate works (week, xvar2, relative)", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                      cohort = lubridate::year(as.Date(Mass$Date, format = "%d/%m/%Y")), refday = c(1, 6),
                      xvar2 = MassClimate$Rain, cinterval = "week", type = "absolute", cross = TRUE,
                      spatial = NULL, upper = NA, lower = NA)
  
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
  expect_true(min(test$bintno) == 59)
  expect_true(max(test$bintno) == 2497)
  expect_true(max(test$cintno) == 2544)
  
  options(scipen = current_option)
  
})

########################################################################

# Test that convertdate works with two climate variables, cinterval = "month"
test_that("convertdate works (month, xvar2, relative)", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
data(Mass, envir = environment())
data(MassClimate, envir = environment())

test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                    xvar2 = MassClimate$Rain, cinterval = "month", type = "relative", cross = TRUE,
                    spatial = NULL, upper = NA, lower = NA)

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

options(scipen = current_option)

})

test_that("convertdate works (month, xvar2, absolute)", {
  
  current_option <- getOption("scipen")
  options(scipen = 0)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  test <- convertdate(bdate = Mass$Date, cdate = MassClimate$Date, xvar = MassClimate$Temp,
                      cohort = lubridate::year(as.Date(Mass$Date, format = "%d/%m/%Y")), refday = c(1, 6),
                      xvar2 = MassClimate$Rain, cinterval = "month", type = "absolute", cross = TRUE,
                      spatial = NULL, upper = NA, lower = NA)
  
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
  expect_true(min(test$bintno) == 18)
  expect_true(max(test$bintno) == 570)
  expect_true(max(test$cintno) == 576)
  
  options(scipen = current_option)
  
})
