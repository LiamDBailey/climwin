# Test the plotweights function #

# Test that plotweights produces a ggplot object #
test_that("plotweights produces a graph", {
  
  data(MassOutput, envir = environment())
  
  test <- plotweights(Dataset = MassOutput, CW1 = 0.95, CW2 = 0.75, CW3 = 0.25)
  
  expect_true(attr(test, "class")[1] == "gg")
  
})