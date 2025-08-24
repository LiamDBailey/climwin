# Test the PlotCor function #
test_that("plotcor produces a graph", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  cross <- crosswin(xvar = list(Temp = MassClimate$Temp), 
                    xvar2 = list(Rain = MassClimate$Rain), cdate = MassClimate$Date,
                    bdate = Mass$Date, range = c(2, 1), 
                    stat = "max", stat2 = "max", type = "relative",
                    cmissing = FALSE, cinterval = "day")
  
  test <- plotcor(cor.output = cross, type = "A")
  
  # Test plotcor produces a ggplot object
  expect_true(inherits(test, "gg"))
  
})