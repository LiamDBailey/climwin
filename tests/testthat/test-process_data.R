test_that("cdate column must be character in DD/MM/YYYY format", {
  
  ## Currently don't accept date format (could change in future)
  climate_data <- data.frame(
    Date = as.Date(c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"), format = "%d/%m/%Y"),
    Temp = c(10, 15, 20, 10, 12)
  )
  
  bio_data <- data.frame(
    Date = c("03/01/1979", "05/01/1979"),
    Mass = c(100, 120)
  )
  
  # Test with Date object - should error
  expect_error(
    process_data(
      climate_data = climate_data,
      bio_data = bio_data,
      range = 0:2
    ),
    "Column 'Date' in climate_data must be a character in format 'DD/MM/YYYY'"
  )
  
  # If character string, only accept dd/mm/yyyy (could change in future)
  climate_data <- data.frame(
    Date = c("1979-01-01", "1979-01-02", "1979-01-03", "1979-01-04", "1979-01-05"),
    Temp = c(10, 15, 20, 10, 12)
  )
  expect_error(
    process_data(
      climate_data = climate_data,
      bio_data = bio_data,
      range = 0:2
    ),
    "Column 'Date' in climate_data must be in format 'DD/MM/YYYY'"
  )
})

test_that("Date series must be continuous with no missing days", {
  
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10)
  )
  
  bio_data <- data.frame(
    Date = c("03/01/1979", "05/01/1979"),
    Mass = c(100, 120)
  )
  
  expect_error(
    process_data(
      climate_data = climate_data,
      bio_data = bio_data,
      range = 0:2
    ),
    "Climate data has missing dates: 1979-01-03. The date series must be continuous from 1979-01-01 to 1979-01-05."
  )
})

test_that("xvar column has no missing data (NA or Inf)", {
  
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, NA, 10, 12)
  )
  
  bio_data <- data.frame(
    Date = c("03/01/1979", "05/01/1979"),
    Mass = c(100, 120)
  )
  expect_error(
    process_data(
      climate_data = climate_data,
      bio_data = bio_data,
      range = 0:2
    ),
    "Column 'Temp' in climate_data contains 1 missing or infinite values out of 5 total values. All climate data must be complete."
  )
  
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, Inf, 10, 12)
  )
  
  expect_error(
    process_data(
      climate_data = climate_data,
      bio_data = bio_data,
      range = 0:2
    ),
    "Column 'Temp' in climate_data contains 1 missing or infinite values out of 5 total values. All climate data must be complete."
  )
})

test_that("All climate data completeness checks work together", {
  
  # Test with valid DD/MM/YYYY format - should work
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  
  bio_data <- data.frame(
    Date = c("03/01/1979", "05/01/1979"),
    Mass = c(100, 120)
  )
  
  expect_silent(
    result <- process_data(
      climate_data = climate_data,
      bio_data = bio_data,
      range = 0:2
    )
  )
  
  # Check that the function returns expected structure
  expect_true(is.list(result))
  expect_true("bio_data" %in% names(result))
  expect_true("bio_int_ranges" %in% names(result))
  expect_true("bio_data_row" %in% names(result))
  expect_true("bio_xvar_ranges" %in% names(result))
  expect_true("max_possible_range" %in% names(result))
})
