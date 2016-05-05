# Test plotdelta function #
test_that("plotdelta produces a graph", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  
  test <- plotdelta(dataset = testdata[[1]]$Dataset)
  
  # Test that plotdelta creates a ggplot item
  expect_true(attr(test, "class")[1] == "gg")
  
})