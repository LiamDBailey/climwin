test_that("fit_climate_models works with valid input", {
  # Create sample data
  climate_means <- data.frame(
    Bio_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Summary_Value = c(10, 15, 20)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Mass = c(100, 110, 120),
    climate = 0
  )
  
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Test function
  result <- fit_climate_models(list(climate_means), basemodel = basemodel)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 6)
  expect_equal(names(result), c("Start_Date", "End_Date", "AIC", "R_squared", "Slope", "P_value"))
  expect_equal(nrow(result), 1)
})

test_that("fit_climate_models handles invalid input", {
  # Test with non-list input
  expect_error(fit_climate_models("not a list", basemodel = lm(Mass ~ climate, data = data.frame(Mass = 1:3, climate = 1:3))))
  
  # Test with missing columns in climate_means
  invalid_climate <- data.frame(wrong_col = 1)
  result <- fit_climate_models(list(invalid_climate), basemodel = lm(Mass ~ climate, data = data.frame(Mass = 1:3, climate = 1:3)))
  expect_equal(nrow(result), 1)
  expect_true(all(is.na(result[1, c("AIC", "R_squared", "Slope", "P_value")])) )
  
  # Test with invalid basemodel
  expect_error(fit_climate_models(list(climate_means), basemodel = "not a model"))
  
  # Test with NULL basemodel
  expect_error(fit_climate_models(list(climate_means), basemodel = NULL))
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
    Mass = c(100, 110),
    climate = 0
  )
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Test function
  result <- fit_climate_models(list(climate_means), basemodel = basemodel)
  
  # Should return a row of NAs
  expect_equal(nrow(result), 1)
  expect_true(all(is.na(result[1, c("AIC", "R_squared", "Slope", "P_value")])) )
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
    Mass = c(100, 110, 120),
    climate = 0
  )
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Test function
  result <- fit_climate_models(list(climate_means), basemodel = basemodel)
  
  # Check if AIC values are sorted
  expect_true(all(diff(result$AIC) >= 0))
})

test_that("fit_climate_models works with different basemodel structures", {
  # Create sample data
  climate_means <- data.frame(
    Bio_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Summary_Value = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Mass = c(100, 110, 120),
    Age = c(1, 2, 3),
    climate = 0
  )
  
  # Test with different model structures
  models <- list(
    lm(Mass ~ climate, data = bio_data),
    lm(Mass ~ climate + Age, data = bio_data),
    lm(Mass ~ climate * Age, data = bio_data)
  )
  
  for (basemodel in models) {
    result <- fit_climate_models(list(climate_means), basemodel = basemodel)
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 6)
    expect_equal(nrow(result), 1)
    # Allow NA p-values if model cannot estimate
    expect_true(is.numeric(result$Slope))
    expect_true(is.numeric(result$P_value) || is.na(result$P_value))
  }
}) 