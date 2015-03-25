# Test function plotbetas #

# Test that plotbetas produces a ggplot objects #
# Test that plotbetas produces new plots for Q and C #
test_that("plotbetas produces a graph", {
  
  data(MassOutput, envir = environment())
  
  testenv <- environment()
  test    <- plotbetas(Dataset = MassOutput)
  
  expect_true(attr(test, "class")[1] == "gg")
 
  MassOutput$ModelBetaQ <- MassOutput$ModelBeta
  MassOutput$Function   <- "Q"
  
  test <- plotbetas(Dataset = MassOutput, plotall = TRUE, plotallenv = testenv)
  
  expect_true(exists("BETA2", envir = testenv))
  
  MassOutput$ModelBetaC <- MassOutput$ModelBeta
  MassOutput$Function   <- "C"
  
  test <- plotbetas(Dataset = MassOutput, plotall = TRUE, plotallenv = testenv)
  
  expect_true(exists("BETA2", envir = testenv))
  expect_true(exists("BETA3", envir = testenv))
  
})