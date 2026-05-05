test_that("plot_weights takes output from run_slidingwin", {
  # Use actual run_slidingwin output with small range for fast testing
  data("MassClimate")
  data("Mass")
  
  results <- run_slidingwin(range = c(0, 2), 
                           climate_data = MassClimate, 
                           bio_data = Mass,
                           baseline = lm(Mass ~ climate, data = bio_data))
  
  # Test that the function works with actual run_slidingwin output
  result <- plot_weights(results)
  expect_s3_class(result, "ggplot")
})

test_that("plot_weights can take different values of cw1", {
  
  # Use actual run_slidingwin output with small range for fast testing
  data("MassClimate")
  data("Mass")
  
  results <- run_slidingwin(range = c(0, 2), 
                            climate_data = MassClimate, 
                            bio_data = Mass,
                            baseline = lm(Mass ~ climate, data = bio_data))
  
  # Test with different cw1 values
  result1 <- plot_weights(results, cw1 = 0.5)
  result2 <- plot_weights(results, cw1 = 0.8)
  result3 <- plot_weights(results, cw1 = 0.95)
  
  # All should return ggplot objects
  expect_s3_class(result1, "ggplot")
  expect_s3_class(result2, "ggplot")
  expect_s3_class(result3, "ggplot")
}) 
