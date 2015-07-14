# Test function plotbest #

# Test that plotbest produces a ggplot object #
test_that("plotbest produces a graph", {
  
  data(MassOutput, envir = environment())
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  single <- singlewin(Xvar = MassClimate$Temp, Cdate = MassClimate$Date, Bdate = Mass$Date,
                      baseline = lm(Mass$Mass ~ 1),furthest = 72, closest = 15,
                      stat = "max", func = "lin",
                      type = "variable", Cmissing = FALSE, Cinterval = "day")
  
  test <- plotbest(Dataset = MassOutput, BestModel = single[[1]],
           BestModelData = single[[2]])
  
  expect_true(attr(test, "class")[1] == "gg")
  
})