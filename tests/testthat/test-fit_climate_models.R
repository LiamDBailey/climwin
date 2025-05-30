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
    Mass = c(100, 110, 120),
    climate = 0
  )
  
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Test function
  result <- fit_climate_models(list(climate_means), bio_data, basemodel = basemodel)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 6)
  expect_equal(names(result), c("Start_Date", "End_Date", "AIC", "R_squared", "Slope", "P_value"))
  expect_equal(nrow(result), 1)
})

test_that("fit_climate_models handles invalid input", {
  # Test with non-data.frame input
  expect_error(fit_climate_models("not a df", Mass, basemodel = lm(Mass ~ climate, data = data.frame(Mass = 1:3, climate = 1:3))))
  expect_error(fit_climate_models(Mass, "not a df", basemodel = lm(Mass ~ climate, data = data.frame(Mass = 1:3, climate = 1:3))))
  
  # Test with missing columns
  invalid_climate <- data.frame(wrong_col = 1)
  expect_error(fit_climate_models(list(invalid_climate), Mass, basemodel = lm(Mass ~ climate, data = data.frame(Mass = 1:3, climate = 1:3))))
  
  invalid_bio <- data.frame(wrong_col = 1)
  expect_error(fit_climate_models(list(Mass), invalid_bio, basemodel = lm(Mass ~ climate, data = data.frame(Mass = 1:3, climate = 1:3))))
  
  # Test with invalid basemodel
  expect_error(fit_climate_models(list(Mass), Mass, basemodel = "not a model"))
  
  # Test with missing climate column when basemodel is provided
  bio_data <- data.frame(
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Mass = c(100, 110, 120)
  )
  basemodel <- lm(Mass ~ climate, data = data.frame(Mass = 1:3, climate = 1:3))
  expect_error(fit_climate_models(list(climate_means), bio_data, basemodel = basemodel))
  
  # Test with NULL basemodel
  expect_error(fit_climate_models(list(climate_means), bio_data, basemodel = NULL))
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
    Mass = c(100, 110),
    climate = 0
  )
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Test function
  result <- fit_climate_models(list(climate_means), bio_data, basemodel = basemodel)
  
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
    CustomMass = c(100, 110, 120),
    climate = 0
  )
  basemodel <- lm(CustomMass ~ climate, data = bio_data)
  
  # Test function with custom column names
  result <- fit_climate_models(list(climate_means), bio_data, 
                             basemodel = basemodel,
                             mass_col = "CustomMass", 
                             date_col = "CustomDate")
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 6)
  expect_equal(nrow(result), 1)
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
    Mass = c(100, 110, 120),
    climate = 0
  )
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Test function
  result <- fit_climate_models(list(climate_means), bio_data, basemodel = basemodel)
  
  # Check if AIC values are sorted
  expect_true(all(diff(result$AIC) >= 0))
})

test_that("fit_climate_models works with basemodel", {
  # Create sample data
  climate_means <- data.frame(
    Bio_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Summary_Value = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Mass = c(100, 110, 120),
    Age = c(1, 2, 3),
    climate = 0  # Initialize climate column
  )
  
  # Create base model
  basemodel <- lm(Mass ~ climate + Age, data = bio_data)
  
  # Test function with basemodel
  result <- fit_climate_models(list(climate_means), bio_data, basemodel = basemodel)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 6)
  expect_equal(nrow(result), 1)
  
  # Check that climate coefficient is extracted correctly
  expect_true(!is.na(result$Slope))
  expect_true(!is.na(result$P_value))
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
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Mass = c(100, 110, 120),
    Age = c(1, 2, 3),
    climate = 0  # Initialize climate column
  )
  
  # Test with different model structures
  models <- list(
    lm(Mass ~ climate, data = bio_data),
    lm(Mass ~ climate + Age, data = bio_data),
    lm(Mass ~ climate * Age, data = bio_data)
  )
  
  for (basemodel in models) {
    result <- fit_climate_models(list(climate_means), bio_data, basemodel = basemodel)
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 6)
    expect_equal(nrow(result), 1)
    expect_true(!is.na(result$Slope))
    expect_true(!is.na(result$P_value))
  }
}) 