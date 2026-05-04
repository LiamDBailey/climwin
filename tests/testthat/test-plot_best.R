test_that("plot_best returns a ggplot object", {
  # Use actual run_slidingwin output with small range for fast testing
  data("MassClimate")
  data("Mass")
  
  results <- run_slidingwin(range = 0:2, 
                           climate_data = MassClimate, 
                           bio_data = Mass,
                           baseline = lm(Mass ~ climate, data = bio_data))
  
  # Test that the function returns a ggplot object
  result <- plot_best(results)
  expect_s3_class(result, "ggplot")
})
