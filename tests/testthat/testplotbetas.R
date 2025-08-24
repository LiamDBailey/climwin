# Test function plotbetas #

test_that("plotbetas produces a graph", {
  
  data(Mass, envir = environment())
  data(MassClimate, envir = environment())
  
  testdata <- slidingwin(xvar = list(MassClimate$Temp), cdate = MassClimate$Date, bdate = Mass$Date, 
                         baseline = lm(Mass ~ 1, data = Mass), range = c(3, 2), 
                         type = "relative", stat = "max", func = "lin", cmissing = FALSE)
  
  testenv <- environment()
  test    <- plotbetas(dataset = testdata[[1]]$Dataset)
  
  # Test that a ggplot object is produced
  expect_true(inherits(test, "gg"))
 
  testdata[[1]]$Dataset$ModelBetaQ <- testdata[[1]]$Dataset$ModelBeta
  testdata[[1]]$Dataset$Function   <- "quad"
  
  test <- plotbetas(dataset = testdata[[1]]$Dataset, plotall = TRUE, plotallenv = testenv)
  
  # Test that a second graph is produced when func = quad
  expect_true(exists("beta2", envir = testenv))
  
  testdata[[1]]$Dataset$ModelBetaC <- testdata[[1]]$Dataset$ModelBeta
  testdata[[1]]$Dataset$Function   <- "cub"
  
  test <- plotbetas(dataset = testdata[[1]]$Dataset, plotall = TRUE, plotallenv = testenv)
  
  # Test that a second graph is produced when func = cub
  expect_true(exists("beta2", envir = testenv))
  
  # Test that a third graph is produced when func = cub
  expect_true(exists("beta3", envir = testenv))
  
})
