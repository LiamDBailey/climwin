test_that("convert_dates_to_int works with valid input", {
  dates <- c("01/01/1979", "02/01/1979", "03/01/1979")
  result <- convert_dates_to_int(dates)
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("date_int", "min_date"))
  expect_type(result$date_int, "integer")
  expect_s3_class(result$min_date, "Date")
  
  # Check values
  expect_equal(result$date_int, c(0, 1, 2))
  expect_equal(result$min_date, as.Date("1979-01-01"))
})

test_that("convert_dates_to_int works with empty input and min_date", {
  result <- convert_dates_to_int(character(0), min_date = "01/01/1979")
  
  expect_type(result, "list")
  expect_named(result, c("date_int", "min_date"))
  expect_equal(length(result$date_int), 0)
  expect_equal(result$min_date, as.Date("1979-01-01"))
})

test_that("convert_dates_to_int handles invalid input", {
  # Invalid date format
  expect_error(convert_dates_to_int("1979-01-01"))
  
  # Invalid min_date format
  expect_error(convert_dates_to_int("01/01/1979", min_date = "1979-01-01"))
  
  # Empty input without min_date
  expect_error(convert_dates_to_int(character(0)))
}) 