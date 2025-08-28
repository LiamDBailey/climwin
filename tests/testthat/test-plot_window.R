test_that("plot_window takes output from run_slidingwin", {
  # Use actual run_slidingwin output with small range for fast testing
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  results <- run_slidingwin(range = 0:2, 
                           climate_data = Climate, 
                           bio_data = Mass,
                           basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Test that the function works with actual run_slidingwin output
  result <- plot_window(results)
  expect_s3_class(result, "ggplot")
})

test_that("plot_window can take different values of cw1", {
  # Use actual run_slidingwin output with small range for fast testing
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  results <- run_slidingwin(range = 0:2, 
                            climate_data = Climate, 
                            bio_data = Mass,
                            basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Test with different cw1 values
  result1 <- plot_window(results, cw1 = 0.5)
  result2 <- plot_window(results, cw1 = 0.8)
  result3 <- plot_window(results, cw1 = 0.95)
  
  # All should return ggplot objects
  expect_s3_class(result1, "ggplot")
  expect_s3_class(result2, "ggplot")
  expect_s3_class(result3, "ggplot")
  
}) 
