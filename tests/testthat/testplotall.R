# Test plotall function #

##########################################################################################

# Test plotall runs when all data is provided#
test_that("plotall produces a graph when all variables provided", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  testdatarand <- randwin(repeats = 2, xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                          type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                          window = "sliding")
  
  single <- singlewin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date,
                      baseline = lm(Mass ~ 1, data = Mass), range = c(72, 15),
                      stat = "mean", func = "lin",
                      type = "relative", cmissing = FALSE, cinterval = "day")
  
  test <- plotall(dataset = testdata[[1]]$Dataset, datasetrand  = testdatarand[[1]], bestmodel = single[[1]],
                  bestmodeldata = single[[2]], cw1 = 0.95, cw2 = 0.5, cw3 = 0.25,
                  ## We know that deltaPAICc is unrelaible, but hide warning in test
                  verbose = FALSE)
  
  ## Creates a gtable (from gridExtra)
  expect_true(inherits(test, "gtable"))
  
})

# Test plotall runs when randomised data is not provided
test_that("plotall produces a graph when datasetrand removed", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  single <- singlewin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date,
                      baseline = lm(Mass ~ 1, data = Mass), range = c(72, 15),
                      stat = "mean", func = "lin",
                      type = "relative", cmissing = FALSE, cinterval = "day")
  
  test <- plotall(dataset = testdata[[1]]$Dataset, bestmodel = single[[1]],
                  bestmodeldata = single[[2]], cw1 = 0.95, cw2 = 0.5, cw3 = 0.25,
                  ## We know that deltaPAICc is unrelaible, but hide warning in test
                  verbose = FALSE)
  
  ## Creates a gtable (from gridExtra)
  expect_true(inherits(test, "gtable"))
  
})

# Test that plotall runs when best model is not provided
test_that("plotall produces a graph when bestmodel removed", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  testdatarand <- randwin(repeats = 2, xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                          type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                          window = "sliding")
  
  single <- singlewin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date,
                      baseline = lm(Mass ~ 1, data = Mass), range = c(72, 15),
                      stat = "mean", func = "lin",
                      type = "relative", cmissing = FALSE, cinterval = "day")
  
  test <- plotall(dataset = testdata[[1]]$Dataset, datasetrand  = testdatarand[[1]],
                  bestmodeldata = single[[2]], cw1 = 0.95, cw2 = 0.5, cw3 = 0.25,
                  ## We know that deltaPAICc is unrelaible, but hide warning in test
                  verbose = FALSE)
  
  ## Creates a gtable (from gridExtra)
  expect_true(inherits(test, "gtable"))
  
})
