test_that("plot_weibull returns a ggplot object", {
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
  
  # Test that the function returns a ggplot object
  result <- plot_weibull(results)
  expect_s3_class(result, "ggplot")
})

test_that("plot_weibull can take inputs from run_weightwin", {
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
  result <- plot_weibull(results)
  expect_s3_class(result, "ggplot")
  
  # Test that the plot has expected components
  expect_true("ggplot" %in% class(result))
  expect_true("GeomLine" %in% class(result$layers[[1]]$geom))
})

test_that("plot_weibull works with different parameter values", {
  # Use actual run_weightwin output with small range for fast testing
  data("MassClimate")
  data("Mass")
  
  # Test with different initial parameters
  results1 <- run_weightwin(range = 0:2, 
                           bio_data = Mass,
                           climate_data = MassClimate,
                           cdate = "Date", bdate = "Date",
                           xvar = "Temp",
                           basemodel = lm(Mass ~ climate, data = bio_data),
                           par = c(2.0, 1.0))
  
  results2 <- run_weightwin(range = 0:2, 
                           bio_data = Mass,
                           climate_data = MassClimate,
                           cdate = "Date", bdate = "Date",
                           xvar = "Temp",
                           basemodel = lm(Mass ~ climate, data = bio_data),
                           par = c(0.5, 0.8))
  
  # Both should return ggplot objects
  result1 <- plot_weibull(results1)
  result2 <- plot_weibull(results2)
  
  expect_s3_class(result1, "ggplot")
  expect_s3_class(result2, "ggplot")
})

