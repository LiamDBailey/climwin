# Test the plothist function #
test_that("plothist produces a graph", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- climatewin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  testdatarand <- randwin(repeats = 2, xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                          baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                          type = "relative", stat = "max", func = "lin", cmissing = FALSE,
                          window = "Sliding")
  
  
  test  <- plothist(dataset = testdata[[1]]$Dataset)
  test2 <- plothist(dataset = testdata[[1]]$Dataset, datasetrand = testdatarand[[1]])
  
  # Test that plothist creates a ggplot object without randomised data
  expect_true(attr(test, "class")[1] == "gg")
  
  # Test that plothist creates a ggplot object with randomised data
  expect_true(attr(test2, "class")[1] == "gg")
  
  # Test that plothist has created a facetwrap when randomised data is provided
  expect_false(is.null(test2$facet$facets))

})