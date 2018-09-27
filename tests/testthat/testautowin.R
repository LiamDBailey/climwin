context("autowin function")

# Test the outcomes of autowin #

test_that("AutoWinOutput has created an output", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  single <- singlewin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date,
                      baseline = lm(Mass ~ 1, data = Mass), range = c(1, 1),
                      stat = "mean", func = "lin",
                      type = "relative", cmissing = FALSE, cinterval = "day")
  
  test <- autowin(reference = single,
                  xvar  = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date,
                  baseline = lm(Mass ~ 1, data = Mass), range = c(2, 1), 
                  stat = "mean", func = "lin", type = "relative", cmissing = FALSE, cinterval = "day")
  
  
  furthest <- 2
  closest  <- 1
  duration <- (furthest - closest) + 1
  maxmodno <- (duration * (duration + 1))/2
  
  # Expect that an object AutoWinOutput exists
  expect_true(exists("test"))
  
  # Expect that there are no NA values
  expect_equal(length(which(is.na(test))), 0)
  
  # Expect that the number of columns is at least 7 (will vary with values of FIXED)
  expect_true(ncol(test) >= 7)
  
  # Expect that the number of rows is equal to the number of possible windows
  expect_equal(maxmodno, nrow(test))
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test$cor[2], 1) == 0.8)
  expect_true(test$BestWindowOpen[1] == 1 & test$BestWindowOpen[1] == 1)
  
})

###############################################################

# Test that spatial replication works with autowin #

test_that("Spatial replication works with autowin", {
  
  data(Mass, envir = environment())
  Mass$Plot <- c(rep(c("A", "B"), 23), "A")
  data(MassClimate, envir = environment())
  MassClimate$Plot <- "A"
  MassClimate2 <- MassClimate
  MassClimate2$Plot <- "B"
  Clim <- rbind(MassClimate, MassClimate2)
  
  single <- singlewin(xvar = list(Temp = Clim$Temp), cdate = Clim$Date, bdate = Mass$Date,
                      baseline = lm(Mass ~ 1, data = Mass), range = c(1, 1),
                      stat = "mean", func = "lin",
                      type = "relative", cmissing = FALSE, cinterval = "day",
                      spatial = list(Mass$Plot, Clim$Plot))
  
  test <- autowin(reference = single,
                  xvar  = list(Temp = Clim$Temp), cdate = Clim$Date, bdate = Mass$Date,
                  baseline = lm(Mass ~ 1, data = Mass), range = c(2, 1), 
                  stat = "mean", func = "lin", type = "relative", cmissing = FALSE, cinterval = "day",
                  spatial = list(Mass$Plot, Clim$Plot))
  
  furthest <- 2
  closest  <- 1
  duration <- (furthest - closest) + 1
  maxmodno <- (duration * (duration + 1))/2
  
  # Expect that an object AutoWinOutput exists
  expect_true(exists("test"))
  
  # Expect that there are no NA values
  expect_equal(length(which(is.na(test))), 0)
  
  # Expect that the number of columns is at least 7 (will vary with values of FIXED)
  expect_true(ncol(test) >= 7)
  
  # Expect that the number of rows is equal to the number of possible windows
  expect_equal(maxmodno, nrow(test))
  
  #Test the values we get out have stayed the same as our last R version
  expect_true(round(test$cor[2], 1) == 0.8)
  expect_true(test$BestWindowOpen[1] == 1 & test$BestWindowOpen[1] == 1)
  
})

