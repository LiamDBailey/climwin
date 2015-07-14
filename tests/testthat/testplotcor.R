# Test the PlotCor and PlotWeight functions #

# Test that PlotCor creates a ggplot object #
test_that("plotcor produces a graph", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  cross <- crosswin(Xvar = MassClimate$Temp, Xvar2 = MassClimate$Rain, Cdate = MassClimate$Date,
                    Bdate = Mass$Date, furthest = 2, closest = 1, 
                    stat = "max", stat2 = "max", type = "variable",
                    Cmissing = FALSE, Cinterval = "day")
  
  test <- plotcor(CorWindowOutput = cross, type = "A")
  
  expect_true(attr(test, "class")[1]=="gg") 
  
})