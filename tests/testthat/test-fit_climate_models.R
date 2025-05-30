test_that("fit_climate_models works with valid input", {
  # Create sample data
  climate_means <- data.frame(
    Bio_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Summary_Value = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Mass = c(100, 110, 120)
  )
  
  # Test function
  result <- fit_climate_models(climate_means, bio_data)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 6)
  expect_equal(names(result), c("Start_Date", "End_Date", "AIC", "R_squared", "Slope", "P_value"))
  expect_equal(nrow(result), 3)
})

test_that("fit_climate_models handles invalid input", {
  # Test with non-data.frame input
  expect_error(fit_climate_models("not a df", Mass))
  expect_error(fit_climate_models(Mass, "not a df"))
  
  # Test with missing columns
  invalid_climate <- data.frame(wrong_col = 1)
  expect_error(fit_climate_models(invalid_climate, Mass))
  
  invalid_bio <- data.frame(wrong_col = 1)
  expect_error(fit_climate_models(Mass, invalid_bio))
})

test_that("fit_climate_models handles insufficient data", {
  # Create data with only 2 points (should be skipped)
  climate_means <- data.frame(
    Bio_Date = c("01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979"),
    Summary_Value = c(10, 15)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "01/01/1979"),
    Mass = c(100, 110)
  )
  
  # Test function
  result <- fit_climate_models(climate_means, bio_data)
  
  # Should return empty data frame
  expect_equal(nrow(result), 0)
})

test_that("fit_climate_models handles custom column names", {
  # Create sample data with custom column names
  climate_means <- data.frame(
    Bio_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Summary_Value = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    CustomDate = c("01/01/1979", "01/01/1979", "01/01/1979"),
    CustomMass = c(100, 110, 120)
  )
  
  # Test function with custom column names
  result <- fit_climate_models(climate_means, bio_data, 
                             mass_col = "CustomMass", 
                             date_col = "CustomDate")
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 6)
  expect_equal(nrow(result), 3)
})

test_that("fit_climate_models results are sorted by AIC", {
  # Create sample data
  climate_means <- data.frame(
    Bio_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Summary_Value = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Mass = c(100, 110, 120)
  )
  
  # Test function
  result <- fit_climate_models(climate_means, bio_data)
  
  # Check if AIC values are sorted
  expect_true(all(diff(result$AIC) >= 0))
}) 