# Test plotwin function #

# Test plotwin produces a ggplot object #
test_that("plotwin produces a graph", {
  
  data(MassOutput, envir = environment())
  
  test <- plotwin(dataset = MassOutput, cw = 0.95)
  
  expect_true(attr(test, "class")[1] == "gg")
  
})