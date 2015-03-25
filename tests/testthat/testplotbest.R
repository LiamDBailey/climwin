# Test function plotbest #

# Test that plotbest produces a ggplot object #
test_that("plotbest produces a graph", {
  
  data(MassOutput, envir = environment())
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  single <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
                      baseline = lm(Mass$Mass ~ 1),furthest = 72, closest = 15,
                      STAT = "max", FUNC = "L",
                      FIXED = FALSE, CMISSING = FALSE, CINTERVAL = "D")
  
  test <- plotbest(Dataset = MassOutput, BestModel = single[[1]],
           BestModelData = single[[2]])
  
  expect_true(attr(test, "class")[1] == "gg")
  
})