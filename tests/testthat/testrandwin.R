# Test randwin function #

# Test that randwin create only WindowOutputRand, not BestModel or BestModelData
# Test that WindowOutputRand is at least 13 columns (will vary with FUNC = Q or C)
# Test that the number of rows in each repeat is the same as number of windows
# Test that the randimised column is yes

test_that("Check randwin output", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  furthest = 2
  closest = 1
  STAT = "max"
  
  rand <- randwin(repeats = 1, Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date, 
                  baseline = lm(Mass ~ 1, data = Mass), furthest = 2, closest = 1, 
                  FIXED = FALSE, STAT = "max", FUNC = "L", CMISSING=FALSE)
  
  duration  <- (furthest - closest) + 1
  MaxMODNO  <- (duration * (duration + 1))/2
  
  expect_true(is.data.frame(rand))
  expect_equal(length(which(is.na(rand[,5]))), 0)
  expect_true(ncol(rand) >= 13)
  expect_equal(MaxMODNO, nrow(subset(rand, Repeat == 1)))
  expect_true((rand["Randomised"])[1,]=="yes")
  
})