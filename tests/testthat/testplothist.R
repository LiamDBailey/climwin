# Test the plothist function #

# Test that plothist produces a ggplot object #
test_that("plothist produces a graph", {
  
  data(MassOutput, envir = environment())
  data(MassRand, envir = environment())
  
  test  <- plothist(dataset = MassOutput)
  test2 <- plothist(dataset = MassOutput, datasetrand = MassRand, histq = 0.95)
  
  expect_true(attr(test, "class")[1] == "gg")
  expect_true(attr(test2, "class")[1] == "gg")
  expect_false(is.null(test2$facet$facets))

})