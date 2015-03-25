# Test the PlotCor and PlotWeight functions #

# Test that PlotCor creates a ggplot object #
test_that("plotcor produces a graph", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  cross <- crosswin(Xvar = MassClimate$Temp, Xvar2 = MassClimate$Rain, CDate = MassClimate$Date,
                    BDate = Mass$Date, furthest = 2, closest = 1, 
                    STAT = "max", FIXED = FALSE,
                    CMISSING = FALSE, CINTERVAL = "D")
  
  test <- plotcor(CorWindowOutput = cross, TYPE = "A")
  
  expect_true(attr(test, "class")[1]=="gg") 
  
})