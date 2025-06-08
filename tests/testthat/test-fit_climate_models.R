test_that("fit_climate_models works with valid input", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    climate = 0
  )
  
  # Test function
  result <- fit_climate_models(range = 0:2,
                             climate_data = climate_data,
                             bio_data = bio_data,
                             basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5)
  expect_equal(names(result), c("Start_Date", "End_Date", "Start_Day", "End_Day", "AIC"))
  expect_equal(nrow(result), 6)  # 6 combinations: 0-0, 0-1, 0-2, 1-1, 1-2, 2-2
})

test_that("fit_climate_models works with different basemodel structures", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
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
    result <- fit_climate_models(range = 0:2,
                               climate_data = climate_data,
                               bio_data = bio_data,
                               basemodel = basemodel)
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 5)
    expect_equal(names(result), c("Start_Date", "End_Date", "Start_Day", "End_Day", "AIC"))
    expect_equal(nrow(result), 6)  # 6 combinations: 0-0, 0-1, 0-2, 1-1, 1-2, 2-2
  }
})

test_that("fit_climate_models works with log(climate)", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(1, 2, 3)  # Use positive values for log(climate)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    climate = c(1, 2, 3)  # Use positive values for log(climate)
  )
  
  # Create a basemodel using log(climate)
  basemodel <- lm(Mass ~ log(climate), data = bio_data)
  
  # Test function
  result <- fit_climate_models(range = 0:2,
                             climate_data = climate_data,
                             bio_data = bio_data,
                             basemodel = basemodel)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5)
  expect_equal(names(result), c("Start_Date", "End_Date", "Start_Day", "End_Day", "AIC"))
  expect_equal(nrow(result), 6)  # 6 combinations: 0-0, 0-1, 0-2, 1-1, 1-2, 2-2
})

test_that("fit_climate_models gives identical results with parallel = TRUE and FALSE", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    climate = 0
  )
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Run with parallel = TRUE
  result_parallel <- fit_climate_models(range = 0:2,
                                      climate_data = climate_data,
                                      bio_data = bio_data,
                                      basemodel = basemodel,
                                      parallel = TRUE)
  
  # Run with parallel = FALSE
  result_sequential <- fit_climate_models(range = 0:2,
                                        climate_data = climate_data,
                                        bio_data = bio_data,
                                        basemodel = basemodel,
                                        parallel = FALSE)
  
  # Compare results
  expect_identical(result_parallel, result_sequential)
}) 