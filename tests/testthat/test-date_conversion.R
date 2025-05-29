test_that("convert_dates_to_int handles basic conversion", {
  dates <- c("01/01/1979", "02/01/1979", "03/01/1979")
  result <- convert_dates_to_int(dates)
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("date_int", "min_date", "lookup_table"))
  
  # Check date_int
  expect_type(result$date_int, "integer")
  expect_equal(result$date_int, c(0, 1, 2))
  
  # Check min_date
  expect_s3_class(result$min_date, "Date")
  expect_equal(format(result$min_date, "%d/%m/%Y"), "01/01/1979")
  
  # Check lookup table
  expect_s3_class(result$lookup_table, "data.frame")
  expect_named(result$lookup_table, c("date_int", "date_char"))
  expect_equal(nrow(result$lookup_table), 3)
})

test_that("convert_dates_to_int handles custom min_date", {
  dates <- c("01/01/1979", "02/01/1979", "03/01/1979")
  result <- convert_dates_to_int(dates, min_date = "31/12/1978")
  
  # Check date_int (should be 1, 2, 3 since min_date is one day earlier)
  expect_equal(result$date_int, c(1, 2, 3))
  expect_equal(format(result$min_date, "%d/%m/%Y"), "31/12/1978")
})

test_that("convert_dates_to_int handles invalid dates", {
  # Test with invalid date format
  expect_error(
    convert_dates_to_int(c("1979-01-01", "02/01/1979")),
    "All dates must be in format 'DD/MM/YYYY'"
  )
  
  # Test with invalid min_date
  expect_error(
    convert_dates_to_int(c("01/01/1979"), min_date = "1979-01-01"),
    "min_date must be in format 'DD/MM/YYYY'"
  )
}) 