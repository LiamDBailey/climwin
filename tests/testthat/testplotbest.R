# Test function plotbest #

# Test that plotbest produces a ggplot object #
test_that("plotbest produces a graph", {
  
  data(MassOutput, envir = environment())
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  single <- singlewin(xvar = list(Temp = MassClimate$Temp), 
                      cdate = MassClimate$Date, bdate = Mass$Date,
                      baseline = lm(Mass$Mass ~ 1), range = c(72, 15),
                      stat = "max", func = "lin",
                      type = "relative", cmissing = FALSE, cinterval = "day")
  
  test <- plotbest(dataset = testdata[[1]]$Dataset, bestmodel = single[[1]],
           bestmodeldata = single[[2]])
  
  # Test that a ggplot object is produced
  expect_true(inherits(test, "gg"))
  
})
