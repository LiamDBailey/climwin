# Test plotall function #

# Test plotwin produces a ggplot object #
test_that("plotall produces a graph when all variables provided", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  data(MassOutput, envir = environment())
  data(MassRand, envir = environment())
  
  single <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
                    baseline = lm(Mass$Mass~1), furthest = 72, closest = 15,
                    STAT = "mean", FUNC = "L",
                    FIXED = FALSE, CMISSING = FALSE, CINTERVAL = "D")
  
  plotall(Dataset = MassOutput, DatasetRand  = MassRand, BestModel = single[[1]],
          BestModelData = single[[2]], CW1 = 0.95, CW2 = 0.5, CW3 = 0.25, HISTQ = 0.99)
  
})

test_that("plotall produces a graph when datasetrand removed", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  data(MassOutput, envir = environment())
  data(MassRand, envir = environment())
  
  single <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
                      baseline = lm(Mass$Mass~1), furthest = 72, closest = 15,
                      STAT = "mean", FUNC = "L",
                      FIXED = FALSE, CMISSING = FALSE, CINTERVAL = "D")
  
  plotall(Dataset = MassOutput, BestModel = single[[1]],
          BestModelData = single[[2]], CW1 = 0.95, CW2 = 0.5, CW3 = 0.25, HISTQ = 0.99)
  
})

test_that("plotall produces a graph when bestmodel removed", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  data(MassOutput, envir = environment())
  data(MassRand, envir = environment())
  
  single <- singlewin(Xvar = MassClimate$Temp, CDate = MassClimate$Date, BDate = Mass$Date,
                      baseline = lm(Mass$Mass~1), furthest = 72, closest = 15,
                      STAT = "mean", FUNC = "L",
                      FIXED = FALSE, CMISSING = FALSE, CINTERVAL = "D")
  
  plotall(Dataset = MassOutput, DatasetRand  = MassRand,
          BestModelData = single[[2]], CW1 = 0.95, CW2 = 0.5, CW3 = 0.25, HISTQ = 0.99)
  
})