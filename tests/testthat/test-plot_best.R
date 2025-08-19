test_that("plot_best returns a ggplot object", {
  # Use actual run_slidingwin output with small range for fast testing
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  results <- run_slidingwin(range = 0:2, 
                           climate_data = Climate, 
                           bio_data = Mass,
                           basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Test that run_slidingwin output contains expected structure
  expect_true(is.list(results))
  expect_true("dataset" %in% names(results))
  expect_true("bestModel" %in% names(results))
  
  # Test that dataset contains expected columns including ModWeight
  expect_true("ModWeight" %in% names(results$dataset))
  expect_true("Start_Day" %in% names(results$dataset))
  expect_true("End_Day" %in% names(results$dataset))
  expect_true("AIC" %in% names(results$dataset))
  
  # Test that the function returns a ggplot object
  result <- plot_best(results)
  expect_s3_class(result, "ggplot")
})

test_that("plot_best can take inputs from run_slidingwin", {
  # Use actual run_slidingwin output with small range for fast testing
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  results <- run_slidingwin(range = 0:2, 
                           climate_data = Climate, 
                           bio_data = Mass,
                           basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Test that run_slidingwin output contains expected structure
  expect_true(is.list(results))
  expect_true("dataset" %in% names(results))
  expect_true("bestModel" %in% names(results))
  
  # Test that dataset contains expected columns including ModWeight
  expect_true("ModWeight" %in% names(results$dataset))
  expect_true("Start_Day" %in% names(results$dataset))
  expect_true("End_Day" %in% names(results$dataset))
  expect_true("AIC" %in% names(results$dataset))
  
  # Test that the function works with actual run_slidingwin output
  result <- plot_best(results)
  expect_s3_class(result, "ggplot")
}) 