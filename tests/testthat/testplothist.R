# Test the plothist function #
test_that("plothist produces a graph", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  testdatarand <- randwin(repeats = 2, xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                          type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                          window = "sliding")
  
  test2 <- plothist(dataset = testdata[[1]]$Dataset, datasetrand = testdatarand[[1]],
                    ## We know we will get low sample size warning
                    verbose = FALSE)
    
  # Test that plothist creates a ggplot object with randomised data
  expect_true(inherits(test2, "gg"))

})