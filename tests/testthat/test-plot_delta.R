test_that("plot_slidingwin can take inputs from run_slidingwin", {
  # Use actual run_slidingwin output with small range for fast testing
  data("MassClimate")
  data("Mass")
  
  results <- run_slidingwin(range = c(0, 2), 
                           climate_data = MassClimate, 
                           bio_data = Mass,
                           baseline = lm(Mass ~ climate, data = bio_data))
  
  # Test that the function works with actual run_slidingwin output
  result <- plot_delta(results)
  expect_s3_class(result, "ggplot")
}) 
