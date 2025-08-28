test_that("repair_climate handles missing dates...", {
  # Test data with missing dates
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 12)
  )
  
  result <- repair_climate(climate_data, cdate = "Date", xvar = "Temp")
  
  # Check that missing date was added
  expect_equal(nrow(result), 5)
  expect_equal(result$Date, c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"))
  expect_equal(result$Temp, c(10, 15, 17.5, 20, 12))
})

test_that("repair_climate handles NA values...", {
  # Test data with multiple missing dates
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, NA, 20, 12)
  )
  
  result <- repair_climate(climate_data, cdate = "Date", xvar = "Temp")
  
  # Check that missing date was added
  expect_equal(nrow(result), 5)
  expect_equal(result$Date, c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"))
  expect_equal(result$Temp, c(10, 15, 17.5, 20, 12))
})

test_that("repair_climate handles Inf values...", {
  # Test data with Inf values
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, Inf, 20, 12)
  )
  
  result <- repair_climate(climate_data, cdate = "Date", xvar = "Temp")
  
  # Check that Inf was replaced with interpolated value
  expect_equal(nrow(result), 5)
  expect_equal(result$Date, c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"))
  expect_equal(result$Temp, c(10, 15, 17.5, 20, 12))
})

test_that("repair_climate handles data with no issues correctly", {
  # Test data with no missing dates or values
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 25, 30)
  )
  
  result <- repair_climate(climate_data, cdate = "Date", xvar = "Temp")
  
  # Check that data is unchanged
  expect_identical(climate_data, result)
})

test_that("repair_climate handles additional columns correctly if NOT specified...", {
  # Test data with additional columns
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 12),
    Humidity = c(60, 65, 70, 55),
    Wind = c(5, 8, 12, 3)
  )
  
  result <- repair_climate(climate_data, cdate = "Date", xvar = "Temp")
  
  expect_equal(nrow(result), 5)
  expect_equal(result$Date, c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"))
  expect_equal(result$Temp, c(10, 15, 17.5, 20, 12))
  expect_equal(result$Humidity, c(60, 65, NA, 70, 55))
  expect_equal(result$Wind, c(5, 8, NA, 12, 3))
  
})

test_that("repair_climate handles multiple columns...", {
  # Test data with additional columns
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 12),
    Humidity = c(60, 65, 70, 55),
    Wind = c(5, 8, 12, 3)
  )
  
  result <- repair_climate(climate_data, cdate = "Date", xvar = c("Temp", "Humidity", "Wind"))
  
  expect_equal(nrow(result), 5)
  expect_equal(result$Date, c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"))
  expect_equal(result$Temp, c(10, 15, 17.5, 20, 12))
  expect_equal(result$Humidity, c(60, 65, 67.5, 70, 55))
  expect_equal(result$Wind, c(5, 8, 10, 12, 3))
  
})

test_that("repair_climate can use other methods for interpolation...", {
  
  # Test data with additional columns
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 12),
    Humidity = c(60, 65, 70, 55),
    Wind = c(5, 8, 12, 3)
  )
  
  ## Use values 2 either side
  result <- repair_climate(climate_data, cdate = "Date",
                           xvar = c("Temp", "Humidity", "Wind"),
                           method = imputeTS::na_ma, k = 2, weighting = "simple")
  
  expect_equal(nrow(result), 5)
  expect_equal(result$Date, c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"))
  expect_equal(result$Temp, c(10, 15, 14.25, 20, 12))
  expect_equal(result$Humidity, c(60, 65, 62.5, 70, 55))
  expect_equal(result$Wind, c(5, 8, 7, 12, 3))
  
})

test_that("Interpolation with big gaps works but throws warning...", {
  
  # Test data with additional columns
  climate_data <- data.frame(
    Date = c("01/01/1979", "03/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20),
    Humidity = c(60, 65, 70),
    Wind = c(6, 8, 10)
  )
  
  ## Use values 2 either side
  expect_warning(result <- repair_climate(climate_data, cdate = "Date",
                           xvar = c("Temp", "Humidity", "Wind"),
                           method = imputeTS::na_ma, k = 2, weighting = "simple"),
                 "40% of dates are missing. Interpolation may be unreliable with such large gaps.")
  
  expect_equal(nrow(result), 5)
  expect_equal(result$Date, c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"))
  expect_equal(result$Temp, c(10, 12.5, 15, 17.5, 20))
  expect_equal(result$Humidity, c(60, 62.5, 65, 67.5, 70))
  expect_equal(result$Wind, c(6, 7, 8, 9, 10))
  
})
