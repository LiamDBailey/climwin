test_that("convert_dates_to_int works with valid input", {
  dates <- c("01/01/1979", "02/01/1979", "03/01/1979")
  result <- convert_dates_to_int(dates)
  
  # Check structure
  expect_type(result, "integer")
  
  # Check values
  expect_equal(result, c(3287, 3288, 3289))
})

test_that("convert_dates_to_int handles invalid input", {
  # Invalid date format
  expect_error(convert_dates_to_int("1979-01-01"))
  
  # Empty input without min_date
  expect_error(convert_dates_to_int(character(0)))
}) 
