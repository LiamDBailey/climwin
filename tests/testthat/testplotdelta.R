# Test plotdelta function #

# Test that plotdelta outputs a ggplot object #
test_that("plotdelta produces a graph", {
  
  data(MassOutput, envir = environment())
  
  test <- plotdelta(dataset = MassOutput)
  
  expect_true(attr(test, "class")[1] == "gg")
  
})