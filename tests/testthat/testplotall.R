# Test plotall function #

# Test plotwin produces a ggplot object #
test_that("plotall produces a graph when all variables provided", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  data(MassOutput, envir = environment())
  data(MassRand, envir = environment())
  
  single <- singlewin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date,
                      baseline = lm(Mass ~ 1, data = Mass), furthest = 72, closest = 15,
                      stat = "mean", func = "lin",
                      type = "variable", cmissing = FALSE, cinterval = "day")
  
  plotall(dataset = MassOutput, datasetrand  = MassRand, bestmodel = single[[1]],
          bestmodeldata = single[[2]], cw1 = 0.95, cw2 = 0.5, cw3 = 0.25, histq = 0.99)
  
})

test_that("plotall produces a graph when datasetrand removed", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  data(MassOutput, envir = environment())
  data(MassRand, envir = environment())
  
  single <- singlewin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date,
                      baseline = lm(Mass$Mass~1), furthest = 72, closest = 15,
                      stat = "mean", func = "lin",
                      type = "variable", cmissing = FALSE, cinterval = "day")
  
  plotall(dataset = MassOutput, bestmodel = single[[1]],
          bestmodeldata = single[[2]], cw1 = 0.95, cw2 = 0.5, cw3 = 0.25, histq = 0.99)
  
})

test_that("plotall produces a graph when bestmodel removed", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  data(MassOutput, envir = environment())
  data(MassRand, envir = environment())
  
  single <- singlewin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date,
                      baseline = lm(Mass$Mass~1), furthest = 72, closest = 15,
                      stat = "mean", func = "lin",
                      type = "variable", cmissing = FALSE, cinterval = "day")
  
  plotall(dataset = MassOutput, datasetrand  = MassRand,
          bestmodeldata = single[[2]], cw1 = 0.95, cw2 = 0.5, cw3 = 0.25, histq = 0.99)
  
})