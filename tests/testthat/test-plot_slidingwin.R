test_that("plot_slidingwin returns a patchwork object with multiple plots", {
  # Use actual run_slidingwin output with small range for fast testing
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  results <- run_slidingwin(range = 0:2, 
                           climate_data = Climate, 
                           bio_data = Mass,
                           basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Test that the function returns a patchwork object with multiple plots
  result <- plot_slidingwin(results, plots = c('delta', 'weights'))
  expect_s3_class(result, "patchwork")
  
  # Test that the function returns a patchwork object with default plots (all plots)
  result_default <- plot_slidingwin(results)
  expect_s3_class(result_default, "patchwork")
})

test_that("plot_slidingwin can handle different plot combinations", {
  # Use actual run_slidingwin output with small range for fast testing
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  results <- run_slidingwin(range = 0:2, 
                           climate_data = Climate, 
                           bio_data = Mass,
                           basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Test different plot combinations
  result1 <- plot_slidingwin(results, plots = c('weights', 'windows'))
  result2 <- plot_slidingwin(results, plots = c('delta', 'best'))
  result3 <- plot_slidingwin(results, plots = 'weights')  # Single plot
  
  # All should return valid objects
  expect_s3_class(result1, "patchwork")
  expect_s3_class(result2, "patchwork")
  expect_s3_class(result3, "ggplot")  # Single plot returns ggplot
})

test_that("plot_slidingwin validates plot types correctly", {
  # Use actual run_slidingwin output with small range for fast testing
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  results <- run_slidingwin(range = 0:2, 
                           climate_data = Climate, 
                           bio_data = Mass,
                           basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Test invalid plot type
  expect_error(
    plot_slidingwin(results, plots = c('invalid', 'delta')),
    "Invalid plot type"
  )
}) 
