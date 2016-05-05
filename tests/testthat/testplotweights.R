# Test the plotweights function #
test_that("plotweights produces a graph", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  
  test <- plotweights(dataset = testdata[[1]]$Dataset, cw1 = 0.95, cw2 = 0.75, cw3 = 0.25)
  
  # Test that plotweights produces a ggplot object
  expect_true(attr(test, "class")[1] == "gg")
  
})