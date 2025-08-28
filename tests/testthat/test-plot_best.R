test_that("plot_best returns a ggplot object", {
  # Use actual run_slidingwin output with small range for fast testing
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  results <- run_slidingwin(range = 0:2, 
                           climate_data = Climate, 
                           bio_data = Mass,
                           basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Test that the function returns a ggplot object
  result <- plot_best(results)
  expect_s3_class(result, "ggplot")
})
