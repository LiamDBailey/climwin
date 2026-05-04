test_that("plot_weightwin returns a patchwork object with multiple plots", {
  # Use actual run_weightwin output with small range for fast testing
  data("MassClimate")
  data("Mass")
  
  results <- run_weightwin(range = 0:2, 
                          bio_data = Mass,
                          climate_data = MassClimate,
                          cdate = "Date", bdate = "Date",
                          xvar = "Temp",
                          basemodel = lm(Mass ~ climate, data = bio_data),
                          par = c(1.25, 0.5))
  
  # Test that the function returns a patchwork object with multiple plots
  result <- plot_weightwin(results, plots = c('weibull', 'best'))
  expect_s3_class(result, "patchwork")
  
  # Test that the function returns a patchwork object with default plots (all plots)
  result_default <- plot_weightwin(results)
  expect_s3_class(result_default, "patchwork")
})

test_that("plot_weightwin can handle different plot combinations", {
  # Use actual run_weightwin output with small range for fast testing
  data("MassClimate")
  data("Mass")
  
  results <- run_weightwin(range = 0:2, 
                          bio_data = Mass,
                          climate_data = MassClimate,
                          cdate = "Date", bdate = "Date",
                          xvar = "Temp",
                          basemodel = lm(Mass ~ climate, data = bio_data),
                          par = c(1.25, 0.5))
  
  # Test different plot combinations
  result1 <- plot_weightwin(results, plots = c('weibull', 'best'))
  result2 <- plot_weightwin(results, plots = 'weibull')  # Single plot
  result3 <- plot_weightwin(results, plots = 'best')     # Single plot
  
  # All should return valid objects
  expect_s3_class(result1, "patchwork")
  expect_s3_class(result2, "ggplot")  # Single plot returns ggplot
  expect_s3_class(result3, "ggplot")  # Single plot returns ggplot
})

test_that("plot_weightwin validates plot types correctly", {
  # Use actual run_weightwin output with small range for fast testing
  data("MassClimate")
  data("Mass")
  
  results <- run_weightwin(range = 0:2, 
                          bio_data = Mass,
                          climate_data = MassClimate,
                          cdate = "Date", bdate = "Date",
                          xvar = "Temp",
                          basemodel = lm(Mass ~ climate, data = bio_data),
                          par = c(1.25, 0.5))
  
  # Test invalid plot type
  expect_error(
    plot_weightwin(results, plots = c('invalid', 'weibull')),
    "Invalid plot type"
  )
})

test_that("plot_weightwin can take inputs from run_weightwin", {
  # Use actual run_weightwin output with small range for fast testing
  data("MassClimate")
  data("Mass")
  
  results <- run_weightwin(range = 0:2, 
                          bio_data = Mass,
                          climate_data = MassClimate,
                          cdate = "Date", bdate = "Date",
                          xvar = "Temp",
                          basemodel = lm(Mass ~ climate, data = bio_data),
                          par = c(1.25, 0.5))
  
  # Test that the function works with actual run_weightwin output
  result <- plot_weightwin(results, y = "Mass")
  expect_s3_class(result, "patchwork")
})

