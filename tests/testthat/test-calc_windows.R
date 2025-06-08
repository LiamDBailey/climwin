test_that("calc_windows returns correct format", {
  # Create test data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979", "03/01/1979")
  )
  
  # Run function
  result <- calc_windows(0:1, climate_data = climate_data, bio_data = bio_data)
  
  # Check list structure
  expect_type(result, "list")
  expect_length(result, 3)  # Should have 3 combinations: 0-0, 0-1, 1-1
  expect_named(result, c("0_0", "0_1", "1_1"))
  
  # Check structure of first dataframe
  first_df <- result[["0_0"]]
  expect_s3_class(first_df, "data.frame")
  expect_true(all(c("Start_Date", "End_Date", "Start_Day", "End_Day", "Summary_Value") %in% colnames(first_df)))
  expect_true(inherits(first_df$Start_Date, "character"))
  expect_true(inherits(first_df$End_Date, "character"))
  expect_true(inherits(first_df$Start_Day, "integer"))
  expect_true(inherits(first_df$End_Day, "integer"))
  expect_true(inherits(first_df$Summary_Value, "numeric"))
})

test_that("calc_windows calculates correct summary values with mean", {
  # Create test data with known values
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979", "03/01/1979")
  )
  
  # Run function
  result <- calc_windows(0:1, climate_data = climate_data, bio_data = bio_data)
  
  # Check values for each range combination
  expect_equal(result[["0_0"]]$Summary_Value, c(12, 14))  # Single day
  expect_equal(result[["0_1"]]$Summary_Value, c(11, 13))  # Two days
  expect_equal(result[["1_1"]]$Summary_Value, c(10, 12))  # Single day
})

test_that("calc_windows works with different summary functions", {
  # Create test data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979", "03/01/1979")
  )
  
  # Test with median
  result_median <- calc_windows(0:1, 
                              climate_data = climate_data,
                              bio_data = bio_data,
                              fn = median)
  
  expect_equal(result_median[["0_0"]]$Summary_Value, c(12, 14))
  expect_equal(result_median[["0_1"]]$Summary_Value, c(11, 13))
  expect_equal(result_median[["1_1"]]$Summary_Value, c(10, 12))
  
  # Test with max
  result_max <- calc_windows(0:1, 
                           climate_data = climate_data,
                           bio_data = bio_data,
                           fn = max)
  
  expect_equal(result_max[["0_0"]]$Summary_Value, c(12, 14))
  expect_equal(result_max[["0_1"]]$Summary_Value, c(12, 14))
  expect_equal(result_max[["1_1"]]$Summary_Value, c(10, 12))
})

test_that("calc_windows handles custom column names", {
  # Create test data with custom column names
  climate_data <- data.frame(
    my_date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    rainfall = c(10, 12, 14)
  )
  bio_data <- data.frame(
    bio_date = c("02/01/1979", "03/01/1979")
  )
  
  # Run function with custom column names
  result <- calc_windows(0:1, 
                       climate_data = climate_data,
                       bio_data = bio_data,
                       cdate = "my_date",
                       bdate = "bio_date",
                       xvar = "rainfall")
  
  # Check values for each range combination
  expect_equal(result[["0_0"]]$Summary_Value, c(12, 14))
  expect_equal(result[["0_1"]]$Summary_Value, c(11, 13))
  expect_equal(result[["1_1"]]$Summary_Value, c(10, 12))
})

test_that("calc_windows handles multiple bio dates", {
  # Create test data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979"),
    Temp = c(10, 12, 14, 16)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "04/01/1979")
  )
  
  # Run function
  result <- calc_windows(0:2, climate_data = climate_data, bio_data = bio_data)
  
  # Check number of combinations
  expect_length(result, 6)  # 0-0, 0-1, 0-2, 1-1, 1-2, 2-2
  
  # Check values for first bio date
  first_date_values <- sapply(result, function(df) df$Summary_Value[1])
  expect_equal(as.numeric(first_date_values), c(14, 13, 12, 12, 11, 10))
  
  # Check values for second bio date
  second_date_values <- sapply(result, function(df) df$Summary_Value[2])
  expect_equal(as.numeric(second_date_values), c(16, 15, 14, 14, 13, 12))
})

test_that("calc_windows handles missing columns", {
  # Create test data with missing columns
  climate_data <- data.frame(
    wrong_date = c("01/01/1979"),
    wrong_temp = c(10)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979")
  )
  
  # Test missing date column
  expect_error(
    calc_windows(0:1, climate_data = climate_data, bio_data = bio_data),
    "climate_data must contain columns 'Date' and 'Temp'"
  )
  
  # Test missing temperature column
  climate_data$Date <- climate_data$wrong_date
  expect_error(
    calc_windows(0:1, climate_data = climate_data, bio_data = bio_data),
    "climate_data must contain columns 'Date' and 'Temp'"
  )
  
  # Test missing bio date column
  bio_data <- data.frame(
    wrong_date = c("02/01/1979")
  )
  climate_data$Temp <- climate_data$wrong_temp
  expect_error(
    calc_windows(0:1, climate_data = climate_data, bio_data = bio_data),
    "bio_data must contain column 'Date'"
  )
})

test_that("calc_windows handles invalid function", {
  climate_data <- data.frame(Date = c("01/01/1979"), Temp = c(10))
  bio_data <- data.frame(Date = c("01/01/1979"))
  
  # Test with non-function
  expect_error(calc_windows(0:1, 
                          climate_data = climate_data,
                          bio_data = bio_data,
                          fn = "not_a_function"),
               "fn must be a function")
  
  # Test with invalid function, but only if range is valid
  expect_error(
    calc_windows(0, climate_data = climate_data, bio_data = bio_data, fn = function(x) stop("error")),
    "error"
  )
})

test_that("calc_windows handles missing climate data", {
  # Create empty data frames
  climate_data <- data.frame(
    Date = character(),
    Temp = numeric()
  )
  bio_data <- data.frame(
    Date = c("02/01/1979")
  )
  
  expect_error(
    calc_windows(0:1, bio_data = bio_data),
    "climate_data must contain at least 1 row"
  )
  
  expect_error(
    calc_windows(0:1, climate_data = climate_data, bio_data = bio_data),
    "climate_data must contain at least 1 row"
  )
})

test_that("calc_windows handles missing bio data", {
  # Create empty data frames
  climate_data <- data.frame(Date = c("01/01/1979"), Temp = c(10))
  
  bio_data <- data.frame(
    Date = character()
  )
  
  expect_error(
    calc_windows(0:1, climate_data = climate_data),
    "bio_data must contain at least 1 row"
  )
  
  expect_error(
    calc_windows(0:1, climate_data = climate_data, bio_data = bio_data),
    "bio_data must contain at least 1 row"
  )
})

test_that("calc_windows works with basic input", {
  # Create test data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979", "03/01/1979")
  )
  
  # Test with default parameters
  result <- calc_windows(0:1, climate_data = climate_data, bio_data = bio_data)
  
  # Check structure
  expect_type(result, "list")
  expect_length(result, 3)  # Should have 3 combinations: 0-0, 0-1, 1-1
  
  # Check one of the dataframes
  expect_s3_class(result[["0_0"]], "data.frame")
  expect_equal(nrow(result[["0_0"]]), 2)  # One row per bio date
  
  # Check values for 0-0 range
  expect_equal(result[["0_0"]]$Date, c("02/01/1979", "03/01/1979"))
  expect_equal(result[["0_0"]]$Start_Date, c("02/01/1979", "03/01/1979"))
  expect_equal(result[["0_0"]]$End_Date, c("02/01/1979", "03/01/1979"))
  expect_equal(result[["0_0"]]$Summary_Value, c(12, 14))
})

test_that("calc_windows works with custom column names", {
  climate_data <- data.frame(
    my_date = c("01/01/1979", "02/01/1979"),
    rainfall = c(10, 12)
  )
  bio_data <- data.frame(
    bio_date = c("01/01/1979", "02/01/1979")
  )
  
  result <- calc_windows(0:1, 
                       climate_data = climate_data,
                       bio_data = bio_data,
                       cdate = "my_date",
                       bdate = "bio_date",
                       xvar = "rainfall")
  
  # Check structure
  expect_type(result, "list")
  expect_length(result, 3)  # Should have 3 combinations: 0-0, 0-1, 1-1
  
  # Check values for 0-0 range
  expect_equal(result[["0_0"]]$Date, c("01/01/1979", "02/01/1979"))
  expect_equal(result[["0_0"]]$Summary_Value, c(10, 12))
})

test_that("calc_windows works with different summary functions", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979", "03/01/1979")
  )
  
  # Test with median
  result <- calc_windows(0:1, 
                       climate_data = climate_data,
                       bio_data = bio_data,
                       fn = median)
  
  # Check values for 0-0 range
  expect_equal(result[["0_0"]]$Summary_Value, c(12, 14))
})

test_that("calc_windows handles missing columns", {
  climate_data <- data.frame(wrong_date = c("01/01/1979"), Temp = c(10))
  bio_data <- data.frame(Date = c("01/01/1979"))
  
  expect_error(calc_windows(0:1, 
                          climate_data = climate_data,
                          bio_data = bio_data),
               "climate_data must contain columns 'Date' and 'Temp'")
  
  climate_data <- data.frame(Date = c("01/01/1979"), Temp = c(10))
  bio_data <- data.frame(wrong_date = c("01/01/1979"))
  
  expect_error(calc_windows(0:1, 
                          climate_data = climate_data,
                          bio_data = bio_data),
               "bio_data must contain column 'Date'")
})

test_that("calc_windows handles invalid function", {
  climate_data <- data.frame(Date = c("01/01/1979"), Temp = c(10))
  bio_data <- data.frame(Date = c("01/01/1979"))
  
  expect_error(calc_windows(0:1, 
                          climate_data = climate_data,
                          bio_data = bio_data,
                          fn = "not_a_function"),
               "fn must be a function")
})

test_that("calc_windows validates range against available data", {
  # Create test data with 3 days of climate data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 12, 14)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979")
  )
  
  # Test with range within available data
  expect_silent(calc_windows(0:2, climate_data = climate_data, bio_data = bio_data))
  
  # Test with range exceeding available data
  expect_error(
    calc_windows(0:3, climate_data = climate_data, bio_data = bio_data),
    "Requested range \\(3 days\\) exceeds available climate data range \\(2 days\\)"
  )
  
  # Test with range exactly at the limit
  expect_silent(calc_windows(0:2, climate_data = climate_data, bio_data = bio_data))
}) 

