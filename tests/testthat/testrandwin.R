# Test randwin function #
test_that("Check randwin output with slidingwin and max", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 2
  closest = 1
  stat = "max"
  repeats = 2
  
  rand <- randwin(repeats = 2, xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
                  bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass), 
                  range = c(2, 1), 
                  type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                  window = "sliding")
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1)) / 2
  
  # Test that randwin produces an output
  expect_true(is.list(rand))
  
  # Test that there are no NA values in output
  expect_equal(length(which(is.na(rand[[1]][, 4]))), 0)
  
  # Test that the randomised output has the right number of columns
  expect_true(ncol(rand[[1]]) == 19)
  
  # Test that the right number of models has been fitted
  expect_equal(repeats, nrow(rand[[1]]))
  
  # Test that data has been stored as randomised
  expect_true((rand[[1]]["Randomised"])[1,] == "yes")
  
  #Test that values match previous R version
  expect_true(round(rand[[1]]$deltaAICc[1], 1) == 1.7)
  expect_true(rand[[1]]$WindowOpen[1] == 1 & rand[[1]]$WindowClose[1] == 1)
  expect_true(round(rand[[1]]$ModelBeta[1], 1) == -0.3)
  
})

############################################################

# Test randwin function with mean climate#
test_that("Check randwin output with slidingwin and mean", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 2
  closest = 1
  stat = "max"
  repeats = 2
  
  rand <- randwin(repeats = 2, xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
                  bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass), 
                  range = c(2, 1), 
                  type = "relative", stat = "mean", func = "lin", cmissing = FALSE,
                  window = "sliding")
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1)) / 2
  
  # Test that randwin produces an output
  expect_true(is.list(rand))
  
  # Test that there are no NA values in output
  expect_equal(length(which(is.na(rand[[1]][, 4]))), 0)
  
  # Test that the randomised output has the right number of columns
  expect_true(ncol(rand[[1]]) == 19)
  
  # Test that the right number of models has been fitted
  expect_equal(repeats, nrow(rand[[1]]))
  
  # Test that data has been stored as randomised
  expect_true((rand[[1]]["Randomised"])[1,] == "yes")
  
  #Test that values match previous R version
  expect_true(round(rand[[1]]$deltaAICc[1], 1) == 1.7)
  expect_true(rand[[1]]$WindowOpen[1] == 1 & rand[[1]]$WindowClose[1] == 1)
  expect_true(round(rand[[1]]$ModelBeta[1], 1) == -0.3)
  
})

############################################################

test_that("Check randwin output works with spatial replication", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  
  furthest = 2
  closest = 1
  stat = "max"
  repeats = 2
  
  rand <- randwin(repeats = 2, xvar = list(Temp = Clim$Temp), cdate = Clim$Date, 
                  bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass), 
                  range = c(2, 1), spatial = list(Mass$Plot, Clim$Plot),
                  type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                  window = "sliding")
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1)) / 2
  
  # Test that randwin produces an output
  expect_true(is.list(rand))
  
  # Test that there are no NA values in output
  expect_equal(length(which(is.na(rand[[1]][, 4]))), 0)
  
  # Test that the randomised output has the right number of columns
  expect_true(ncol(rand[[1]]) == 19)
  
  # Test that the right number of models has been fitted
  expect_equal(repeats, nrow(rand[[1]]))
  
  # Test that data has been stored as randomised
  expect_true((rand[[1]]["Randomised"])[1,] == "yes")
  
  #Test that values match previous R version
  expect_true(round(rand[[1]]$deltaAICc[1], 1) == 1.7)
  expect_true(rand[[1]]$WindowOpen[1] == 1 & rand[[1]]$WindowClose[1] == 1)
  expect_true(round(rand[[1]]$ModelBeta[1], 1) == -0.3)
  
})

#############################################################

test_that("Check randwin output with weightwin", {
  
  set.seed(666)
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 2
  closest = 1
  repeats = 2
  
  rand <- randwin(repeats = 2, xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
                  bdate = Mass$Date, baseline = lm(Mass ~ 1, data = Mass), 
                  range = c(2, 1), 
                  type = "relative", func = "lin", cmissing = FALSE,
                  window = "weighted", weightfunc = "W", cinterval = "day", 
                  par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)), 
                  method = "L-BFGS-B")
  
  duration  <- (furthest - closest) + 1
  maxmodno  <- (duration * (duration + 1)) / 2
  
  # Test that randwin produces an output
  expect_true(is.list(rand))
  
  # Test that there are no NA values in output
  expect_equal(length(which(is.na(rand[[1]][, 9]))), 0)
  
  # Test that the randomised output has the right number of columns
  expect_true(ncol(rand[[1]]) == 15)
  
  # Test that the right number of models has been fitted
  expect_equal(repeats, nrow(rand[[1]]))
  
  # Test that data has been stored as randomised
  expect_true((rand[[1]]["Randomised"])[1,] == "yes")
  
  #Test that values match previous R version
  expect_true(round(rand[[1]]$deltaAICc[1], 1) == 1.7)
  expect_true(round(rand[[1]]$ModelBeta[1], 1) == -0.3)
  expect_true(round(rand[[1]]$ModelInt[1], 0) == 132)
  
})
