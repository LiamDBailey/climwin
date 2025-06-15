test_that("process_data handles basic date conversion correctly", {
  # Create test data
  climate_data <- data.frame(
    Date = as.Date("2020-01-01") + 0:4,
    Temp = 20:24
  )
  bio_data <- data.frame(
    Date = as.Date("2020-01-03") + 0:2,
    Mass = 1:3
  )
  
  result <- process_data(
    climate_data = climate_data,
    bio_data = bio_data,
    range = 0:2,
    cdate = "Date",
    bdate = "Date",
    xvar = "Temp",
    type = "relative"
  )
  
  # Check basic structure
  expect_type(result, "list")
  expect_named(result, c("bio_data", "bio_int_ranges", "bio_data_row", "bio_xvar_ranges", "max_possible_range"))
  
  # Check date conversion
  expect_equal(result$bio_data$date_int, 3:5)
  expect_equal(result$max_possible_range, 4)
})

test_that("process_data handles cohort dates correctly", {
  # Create test data with cohorts
  climate_data <- data.frame(
    Date = as.Date("2020-01-01") + 0:4,
    Temp = 20:24
  )
  bio_data <- data.frame(
    Date = as.Date(c("2020-01-03", "2021-01-03", "2022-01-03")),
    Mass = 1:3,
    Cohort = c("A", "A", "B")
  )
  
  result <- process_data(
    climate_data = climate_data,
    bio_data = bio_data,
    range = 0:2,
    cdate = "Date",
    bdate = "Date",
    xvar = "Temp",
    type = "relative",
    cohort = "Cohort"
  )
  
  # Check that all dates in cohort A use the earliest year (2020)
  cohort_a_dates <- result$bio_data$date_int[bio_data$Cohort == "A"]
  expect_true(all(cohort_a_dates == 3))
})

test_that("process_data handles absolute dates correctly", {
  # Create test data
  climate_data <- data.frame(
    Date = as.Date("2020-01-01") + 0:4,
    Temp = 20:24
  )
  bio_data <- data.frame(
    Date = as.Date(c("2020-05-03", "2021-03-06", "2022-05-05")),
    Mass = 1:3
  )
  
  result <- process_data(
    climate_data = climate_data,
    bio_data = bio_data,
    range = 0:2,
    cdate = "Date",
    bdate = "Date",
    xvar = "Temp",
    type = "absolute",
    refday = "03/01/2020"
  )
  
  # Check that all dates use the reference day's month and day
  expect_equal(result$bio_data$date_int, convert_dates_to_int(c("03/01/2020", "03/01/2021", "03/01/2022"), min_date = "01/01/2020") + 1)
})

test_that("process_data handles spatial grouping correctly", {
  # Create test data with spatial groups
  climate_data <- data.frame(
    Date = as.Date("2020-01-01") + 0:4,
    Temp = 20:24,
    Site = c("A", "A", "B", "B", "B")
  )
  bio_data <- data.frame(
    Date = as.Date("2020-01-03") + 0:2,
    Mass = 1:3,
    Site = c("A", "B", "B")
  )
  
  result <- process_data(
    climate_data = climate_data,
    bio_data = bio_data,
    range = 0:2,
    cdate = "Date",
    bdate = "Date",
    xvar = "Temp",
    spatial = "Site",
    type = "relative"
  )
  
  # Check spatial grouping
  expect_type(result$bio_int_ranges, "list")
  expect_named(result$bio_int_ranges, c("A", "B"))
  expect_equal(ncol(result$bio_int_ranges$A), 1)  # One date for site A
  expect_equal(ncol(result$bio_int_ranges$B), 2)  # Two dates for site B
})

test_that("process_data validates range correctly", {
  # Create test data
  climate_data <- data.frame(
    Date = as.Date("2020-01-01") + 0:2,
    Temp = 20:22
  )
  bio_data <- data.frame(
    Date = as.Date("2020-01-01") + 0:1,
    Mass = 1:2
  )
  
  # Test range that's too large
  expect_error(
    process_data(
      climate_data = climate_data,
      bio_data = bio_data,
      range = 0:5,  # Too large for the climate data
      cdate = "Date",
      bdate = "Date",
      xvar = "Temp",
      type = "relative"
    ),
    "'range' covers time periods not included in climate data. Consider adding more climate data or reducing range."
  )
})

test_that("process_data handles missing spatial column correctly", {
  # Create test data without spatial column
  climate_data <- data.frame(
    Date = as.Date("2020-01-01") + 0:4,
    Temp = 20:24
  )
  bio_data <- data.frame(
    Date = as.Date("2020-01-03") + 0:1,
    Mass = 1:2
  )
  
  result <- process_data(
    climate_data = climate_data,
    bio_data = bio_data,
    range = 0:2,
    cdate = "Date",
    bdate = "Date",
    xvar = "Temp",
    spatial = NULL,
    type = "relative"
  )
  
  # Check that spatial column was added
  expect_true("spatial" %in% names(result$bio_data))
  expect_true(all(result$bio_data$spatial == "A"))
})

test_that("process_data handles climate variable extraction correctly", {
  # Create test data
  climate_data <- data.frame(
    Date = as.Date("2020-01-01") + 0:4,
    Temp = 20:24,
    Rain = 1:5
  )
  bio_data <- data.frame(
    Date = as.Date("2020-01-03") + 0:1,
    Mass = 1:2
  )
  
  result <- process_data(
    climate_data = climate_data,
    bio_data = bio_data,
    range = 0:2,
    cdate = "Date",
    bdate = "Date",
    xvar = "Rain",  # Use Rain instead of Temp
    type = "relative"
  )
  
  # Check that the correct climate variable was extracted
  expect_equal(dim(result$bio_xvar_ranges), c(3, 2))  # 3 days, 2 bio records
  expect_equal(result$bio_xvar_ranges[1,], c(3, 4))  # First day's values
}) 
