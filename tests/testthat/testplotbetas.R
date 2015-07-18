# Test function plotbetas #

# Test that plotbetas produces a ggplot objects #
# Test that plotbetas produces new plots for quad and cub #
test_that("plotbetas produces a graph", {
  
  data(MassOutput, envir = environment())
  
  testenv <- environment()
  test    <- plotbetas(dataset = MassOutput)
  
  expect_true(attr(test, "class")[1] == "gg")
 
  MassOutput$ModelBetaQ <- MassOutput$ModelBeta
  MassOutput$Function   <- "quad"
  
  test <- plotbetas(dataset = MassOutput, plotall = TRUE, plotallenv = testenv)
  
  expect_true(exists("beta2", envir = testenv))
  
  MassOutput$ModelBetaC <- MassOutput$ModelBeta
  MassOutput$Function   <- "cub"
  
  test <- plotbetas(dataset = MassOutput, plotall = TRUE, plotallenv = testenv)
  
  expect_true(exists("beta2", envir = testenv))
  expect_true(exists("beta3", envir = testenv))
  
})