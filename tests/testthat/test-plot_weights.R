test_that("plot_weights takes output from run_slidingwin", {
  # Use actual run_slidingwin output with small range for fast testing
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  results <- run_slidingwin(range = 0:2, 
                           climate_data = Climate, 
                           bio_data = Mass,
                           basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Test that the function works with actual run_slidingwin output
  result <- plot_weights(results)
  expect_s3_class(result, "ggplot")
})

test_that("plot_weights returns a ggplot object", {
  # Create sample data similar to run_slidingwin output
  sample_data <- data.frame(
    Start_Day = c(0, 0, 1, 1, 2, 2),
    End_Day = c(0, 1, 1, 2, 2, 3),
    AIC = c(150, 145, 140, 135, 130, 125),
    ModWeight = c(0.1, 0.15, 0.2, 0.25, 0.2, 0.1)
  )
  
  # Test that the function returns a ggplot object
  result <- plot_weights(sample_data)
  expect_s3_class(result, "ggplot")
})

test_that("plot_weights can take different values of cw1", {
  # Create sample data similar to run_slidingwin output
  sample_data <- data.frame(
    Start_Day = c(0, 0, 1, 1, 2, 2),
    End_Day = c(0, 1, 1, 2, 2, 3),
    AIC = c(150, 145, 140, 135, 130, 125),
    ModWeight = c(0.1, 0.15, 0.2, 0.25, 0.2, 0.1)
  )
  
  # Test with different cw1 values
  result1 <- plot_weights(sample_data, cw1 = 0.5)
  result2 <- plot_weights(sample_data, cw1 = 0.8)
  result3 <- plot_weights(sample_data, cw1 = 0.95)
  
  # All should return ggplot objects
  expect_s3_class(result1, "ggplot")
  expect_s3_class(result2, "ggplot")
  expect_s3_class(result3, "ggplot")
  
  # Test default value works
  result_default <- plot_weights(sample_data)
  expect_s3_class(result_default, "ggplot")
}) 