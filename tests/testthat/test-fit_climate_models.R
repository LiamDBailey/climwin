test_that("fit_climate_models works with valid input", {
  # Create sample data
  climate_means <- data.frame(
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Start_Day = c(0L, 0L, 0L),
    End_Day = c(0L, 1L, 2L),
    Summary_Value = c(10, 15, 20)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Mass = c(100, 110, 120),
    climate = 0
  )
  
  # Test function
  result <- fit_climate_models(list(climate_means), basemodel = lm(Mass ~ climate, data = bio_data), bio_data = bio_data)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5)
  expect_equal(names(result), c("Start_Date", "End_Date", "Start_Day", "End_Day", "AIC"))
  expect_equal(nrow(result), 1)
})

test_that("fit_climate_models handles insufficient data", {
  # Create data with only 2 points (should be skipped)
  climate_means <- data.frame(
    Date = c("01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979"),
    Start_Day = c(0L, 0L),
    End_Day = c(0L, 1L),
    Summary_Value = c(10, 15)
  )
  
  bio_data <- data.frame(
    Mass = c(100, 110),
    climate = 0
  )
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Test function
  result <- fit_climate_models(list(climate_means), basemodel = basemodel, bio_data = bio_data)
  
  # Should return a row of NAs
  expect_equal(nrow(result), 1)
  expect_true(all(is.na(result[1, c("AIC")])) )
})

test_that("fit_climate_models results are sorted by AIC", {
  # Create sample data
  climate_means <- data.frame(
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Start_Day = c(0L, 0L, 0L),
    End_Day = c(0L, 1L, 2L),
    Summary_Value = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Mass = c(100, 110, 120),
    climate = 0
  )
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Test function
  result <- fit_climate_models(list(climate_means), basemodel = basemodel, bio_data = bio_data)
  
  # Check if AIC values are sorted
  expect_true(all(diff(result$AIC) >= 0))
})

test_that("fit_climate_models works with different basemodel structures", {
  # Create sample data
  climate_means <- data.frame(
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Start_Day = c(0L, 0L, 0L),
    End_Day = c(0L, 1L, 2L),
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
    result <- fit_climate_models(list(climate_means), basemodel = basemodel, bio_data = bio_data)
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 5)
    expect_equal(names(result), c("Start_Date", "End_Date", "Start_Day", "End_Day", "AIC"))
    expect_equal(nrow(result), 1)
  }
})

test_that("fit_climate_models works with log(climate)", {
  # Create sample data
  climate_means <- data.frame(
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Start_Day = c(0L, 0L, 0L),
    End_Day = c(0L, 1L, 2L),
    Summary_Value = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Mass = c(100, 110, 120),
    climate = c(1, 2, 3)  # Use positive values for log(climate)
  )
  
  # Create a basemodel using log(climate)
  basemodel <- lm(Mass ~ log(climate), data = bio_data)
  
  # Test function
  result <- fit_climate_models(list(climate_means), basemodel = basemodel, bio_data = bio_data)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5)
  expect_equal(names(result), c("Start_Date", "End_Date", "Start_Day", "End_Day", "AIC"))
  expect_equal(nrow(result), 1)
})

test_that("fit_climate_models gives identical results with parallel = TRUE and FALSE", {
  # Create sample data
  climate_means <- data.frame(
    Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    Start_Date = c("01/01/1979", "01/01/1979", "01/01/1979"),
    End_Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Start_Day = c(0L, 0L, 0L),
    End_Day = c(0L, 1L, 2L),
    Summary_Value = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Mass = c(100, 110, 120),
    climate = 0
  )
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Run with parallel = TRUE
  result_parallel <- fit_climate_models(list(climate_means), basemodel = basemodel, bio_data = bio_data, parallel = TRUE)
  
  # Run with parallel = FALSE
  result_sequential <- fit_climate_models(list(climate_means), basemodel = basemodel, bio_data = bio_data, parallel = FALSE)
  
  # Compare results
  expect_identical(result_parallel, result_sequential)
}) 
