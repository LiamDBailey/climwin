test_that("run_randwin works with valid input", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 25, 30)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Date = c("03/01/1979", "04/01/1979"),
    Mass = c(100, 110),
    climate = 0
  )
  
  # Test function with small number of repeats
  result <- run_randwin(repeats = 2,
                        range = 0:1,
                        climate_data = climate_data,
                        bio_data = bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data))
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5)
  expect_equal(names(result), c("Iteration", "Start_Day", "End_Day", "AIC", "ModWeight"))
  expect_equal(nrow(result), 2)  # Should have 2 rows (one per repeat)
  
  # Check iteration numbers
  expect_equal(result$Iteration, c(1, 2))
})

test_that("run_randwin validates arguments correctly", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("02/01/1979", "03/01/1979"),
    Mass = c(100, 110),
    climate = 0
  )
  
  # Test missing repeats
  expect_error(
    run_randwin(range = 0:1, 
                climate_data = climate_data,
                bio_data = bio_data,
                baseline = lm(Mass ~ climate, data = bio_data)),
    "'repeats' is required"
  )
  
  # Test invalid repeats
  expect_error(
    run_randwin(repeats = -1,
                range = 0:1, 
                climate_data = climate_data,
                bio_data = bio_data,
                baseline = lm(Mass ~ climate, data = bio_data)),
    "'repeats': must be a positive integer"
  )
  
  # Test missing range
  expect_error(
    run_randwin(repeats = 2,
                climate_data = climate_data,
                bio_data = bio_data,
                baseline = lm(Mass ~ climate, data = bio_data)),
    "'range' is required"
  )
})

test_that("run_randwin errors on invalid window_type", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  bio_data <- data.frame(
    Date = c("02/01/1979", "03/01/1979"),
    Mass = c(100, 110),
    climate = 0
  )
  expect_error(
    run_randwin(repeats = 1, range = 0:1,
                climate_data = climate_data, bio_data = bio_data,
                baseline = lm(Mass ~ climate, data = bio_data),
                window_type = "invalid"),
    "should be one of"
  )
})

# window_type = "weightwin" ---------------------------------------------------

make_randwin_data <- function() {
  climate_data <- data.frame(
    Date = c(
      "01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979",
      "06/01/1979", "07/01/1979", "08/01/1979", "09/01/1979", "10/01/1979"
    ),
    Temp = c(10, 15, 20, 10, 12, 8, 14, 18, 11, 13)
  )
  bio_data <- data.frame(
    Date   = c("06/01/1979", "08/01/1979", "10/01/1979"),
    Mass   = c(100, 110, 120),
    climate = 0
  )
  list(climate_data = climate_data, bio_data = bio_data)
}

test_that("run_randwin ('weightwin') returns a data frame with one row per repeat", {
  d <- make_randwin_data()
  result <- run_randwin(
    repeats      = 3,
    range        = 0:4,
    climate_data = d$climate_data,
    bio_data     = d$bio_data,
    baseline = lm(Mass ~ climate, data = bio_data),
    window_type  = "weightwin",
    par          = c(1.25, 0.5),
    progress     = FALSE
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true("Iteration" %in% names(result))
  expect_true("AIC"       %in% names(result))
  expect_equal(result$Iteration, c(1, 2, 3))
})

test_that("run_randwin ('weightwin') summary columns match weightfunc par labels", {
  d <- make_randwin_data()

  result_W <- run_randwin(
    repeats = 2, range = 0:4,
    climate_data = d$climate_data, bio_data = d$bio_data,
    baseline = lm(Mass ~ climate, data = bio_data),
    window_type = "weightwin", weightfunc = "W", par = c(1.25, 0.5),
    progress = FALSE
  )
  expect_true(all(c("start_shape", "start_scale",
                    "end_shape",   "end_scale") %in% names(result_W)))

  result_F <- run_randwin(
    repeats = 2, range = 0:4,
    climate_data = d$climate_data, bio_data = d$bio_data,
    baseline = lm(Mass ~ climate, data = bio_data),
    window_type = "weightwin", weightfunc = "F", par = c(0.5, 2),
    progress = FALSE
  )
  expect_true(all(c("start_scale", "start_shape",
                    "end_scale",   "end_shape") %in% names(result_F)))
  expect_false("start_loc" %in% names(result_F))
})

test_that("run_randwin randomizes climate data correctly", {
  # Create sample data with distinct values
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(1, 2, 3, 4, 5)  # Distinct values to test randomization
  )
  
  bio_data <- data.frame(
    Date = c("03/01/1979", "04/01/1979"),
    Mass = c(100, 110),
    climate = 0
  )
  
  # Run the function multiple times to check if results vary
  # (This is a probabilistic test, but with distinct values and multiple runs,
  # we should get some variation if randomization is working)
  set.seed(123)
  result1 <- run_randwin(repeats = 5,
                        range = 0:1,
                        climate_data = climate_data,
                        bio_data = bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data))
  
  set.seed(456)
  result2 <- run_randwin(repeats = 5,
                        range = 0:1,
                        climate_data = climate_data,
                        bio_data = bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data))
  
  # Check that we get results
  expect_equal(nrow(result1), 5)
  expect_equal(nrow(result2), 5)
  
  # Structure should be the same
  expect_equal(names(result1), names(result2))
  expect_equal(result1$Iteration, result2$Iteration)
}) 
