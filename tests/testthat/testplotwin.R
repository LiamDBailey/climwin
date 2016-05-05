# Test plotwin function #
test_that("plotwin produces a graph", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  
  test <- plotwin(dataset = testdata[[1]]$Dataset, cw = 0.95)
  
  # Test that plotwin produces a ggplot object
  expect_true(attr(test, "class")[1] == "gg")
  
})