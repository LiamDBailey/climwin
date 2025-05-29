test_that("calculate_temp_means returns correct format", {
  # Create test data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979", "03/01/1979")
  )
  
  # Run function
  result <- calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data)
  
  # Check output format
  expect_s3_class(result, "data.frame")
  expect_named(result, c("Bio_Date", "Start_Date", "End_Date", "Summary_Value"))
  expect_type(result$Bio_Date, "character")
  expect_type(result$Start_Date, "character")
  expect_type(result$End_Date, "character")
  expect_type(result$Summary_Value, "double")
  
  # Check date format
  expect_true(all(grepl("^\\d{2}/\\d{2}/\\d{4}$", result$Bio_Date)))
  expect_true(all(grepl("^\\d{2}/\\d{2}/\\d{4}$", result$Start_Date)))
  expect_true(all(grepl("^\\d{2}/\\d{2}/\\d{4}$", result$End_Date)))
})

test_that("calculate_temp_means calculates correct summary values with mean", {
  # Create test data with known values
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979")
  )
  
  # Run function
  result <- calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data)
  
  # Expected results for each combination:
  # 0-0: mean of 02/01/1979 = 12
  # 0-1: mean of 01/01/1979 to 02/01/1979 = (10 + 12)/2 = 11
  # 1-1: mean of 01/01/1979 = 10
  
  expected_values <- c(12, 11, 10)
  expect_equal(result$Summary_Value, expected_values)
})

test_that("calculate_temp_means works with different summary functions", {
  # Create test data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979")
  )
  
  # Test with median
  result_median <- calculate_temp_means(0:1, 
                                      climate_data = climate_data, 
                                      bio_data = bio_data,
                                      fn = median)
  
  # Expected results for each combination with median:
  # 0-0: median of 02/01/1979 = 12
  # 0-1: median of 01/01/1979 to 02/01/1979 = median(10, 12) = 11
  # 1-1: median of 01/01/1979 = 10
  expected_median <- c(12, 11, 10)
  expect_equal(result_median$Summary_Value, expected_median)
  
  # Test with max
  result_max <- calculate_temp_means(0:1, 
                                   climate_data = climate_data, 
                                   bio_data = bio_data,
                                   fn = max)
  
  # Expected results for each combination with max:
  # 0-0: max of 02/01/1979 = 12
  # 0-1: max of 01/01/1979 to 02/01/1979 = max(10, 12) = 12
  # 1-1: max of 01/01/1979 = 10
  expected_max <- c(12, 12, 10)
  expect_equal(result_max$Summary_Value, expected_max)
})

test_that("calculate_temp_means handles custom column names", {
  # Create test data with custom column names
  climate_data <- data.frame(
    my_date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    rainfall = c(10, 12, 14)
  )
  bio_data <- data.frame(
    bio_date = c("02/01/1979")
  )
  
  # Run function with custom column names
  result <- calculate_temp_means(0:1, 
                               climate_data = climate_data, 
                               bio_data = bio_data,
                               cdate = "my_date",
                               bdate = "bio_date",
                               xvar = "rainfall")
  
  # Check output format
  expect_s3_class(result, "data.frame")
  expect_named(result, c("Bio_Date", "Start_Date", "End_Date", "Summary_Value"))
  
  # Check values (same as previous test but with rainfall data)
  expected_values <- c(12, 11, 10)
  expect_equal(result$Summary_Value, expected_values)
})

test_that("calculate_temp_means handles multiple bio dates", {
  # Create test data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979", "03/01/1979")
  )
  
  # Run function
  result <- calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data)
  
  # Check number of rows (3 combinations for each of 2 bio dates = 6 rows)
  expect_equal(nrow(result), 6)
  
  # Check that all bio dates are present
  expect_equal(unique(result$Bio_Date), c("02/01/1979", "03/01/1979"))
  
  # Check values for first bio date (02/01/1979)
  first_date_values <- result$Summary_Value[result$Bio_Date == "02/01/1979"]
  expect_equal(first_date_values, c(12, 11, 10))
  
  # Check values for second bio date (03/01/1979)
  second_date_values <- result$Summary_Value[result$Bio_Date == "03/01/1979"]
  expect_equal(second_date_values, c(14, 13, 12))
})

test_that("calculate_temp_means handles missing columns", {
  # Create test data with missing columns
  climate_data <- data.frame(
    wrong_date = c("01/01/1979", "02/01/1979"),
    wrong_temp = c(10, 12)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979")
  )
  
  # Test missing date column
  expect_error(
    calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data),
    "climate_data must contain columns 'Date' and 'Temp'"
  )
  
  # Test missing temperature column
  climate_data$Date <- climate_data$wrong_date
  expect_error(
    calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data),
    "climate_data must contain columns 'Date' and 'Temp'"
  )
  
  # Test missing bio date column
  bio_data <- data.frame(
    wrong_date = c("02/01/1979")
  )
  climate_data$Temp <- climate_data$wrong_temp
  expect_error(
    calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data),
    "bio_data must contain column 'Date'"
  )
})

test_that("calculate_temp_means handles invalid function", {
  # Create test data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979"),
    Temp = c(10, 12)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979")
  )
  
  # Test with non-function
  expect_error(
    calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data, fn = "mean"),
    "fn must be a function"
  )
  
  # Test with invalid function
  expect_error(
    calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data, fn = function(x) stop("error")),
    "error"
  )
})

test_that("calculate_temp_means handles invalid date formats", {
  # Create test data with invalid date format
  climate_data <- data.frame(
    Date = c("1979-01-01", "1979-01-02"),
    Temp = c(10, 12)
  )
  bio_data <- data.frame(
    Date = c("1979-01-02")
  )
  
  expect_error(
    calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data),
    "All dates must be in format 'DD/MM/YYYY'"
  )
})

test_that("calculate_temp_means handles empty data", {
  # Create empty data frames
  climate_data <- data.frame(
    Date = character(),
    Temp = numeric()
  )
  bio_data <- data.frame(
    Date = character()
  )
  
  # Test with empty data
  result <- calculate_temp_means(0:1, climate_data = climate_data, bio_data = bio_data)
  expect_equal(nrow(result), 0)
  expect_named(result, c("Bio_Date", "Start_Date", "End_Date", "Summary_Value"))
}) 