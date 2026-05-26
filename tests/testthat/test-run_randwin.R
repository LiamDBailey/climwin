test_that("run_randwin works with valid input", {
  climate_data <- data.frame(
    Date = c(
      "01/01/1979", "02/01/1979", "03/01/1979",
      "04/01/1979", "05/01/1979"
    ),
    Temp = c(10, 15, 20, 25, 30)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "04/01/1979"),
    Mass = c(100, 110),
    climate = 0
  )

  result <- run_randwin(
    repeats      = 2,
    range        = c(0, 1),
    climate_data = climate_data,
    bio_data     = bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data)
  )

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5)
  expect_equal(
    names(result),
    c("Iteration", "Start_Day", "End_Day", "AIC", "ModWeight")
  )
  expect_equal(nrow(result), 2)
  expect_equal(result$Iteration, c(1, 2))
})

test_that("run_randwin validates arguments correctly", {
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
    run_randwin(
      range        = c(0, 1),
      climate_data = climate_data,
      bio_data     = bio_data,
      baseline     = lm(Mass ~ climate, data = bio_data)
    ),
    "'repeats' is required"
  )

  expect_error(
    run_randwin(
      repeats      = -1,
      range        = c(0, 1),
      climate_data = climate_data,
      bio_data     = bio_data,
      baseline     = lm(Mass ~ climate, data = bio_data)
    ),
    "'repeats': must be a positive integer"
  )

  expect_error(
    run_randwin(
      repeats      = 2,
      climate_data = climate_data,
      bio_data     = bio_data,
      baseline     = lm(Mass ~ climate, data = bio_data)
    ),
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
    run_randwin(
      repeats      = 1,
      range        = c(0, 1),
      climate_data = climate_data,
      bio_data     = bio_data,
      baseline     = lm(Mass ~ climate, data = bio_data),
      window_type  = "invalid"
    ),
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
    Date    = c("06/01/1979", "08/01/1979", "10/01/1979"),
    Mass    = c(100, 110, 120),
    climate = 0
  )
  list(climate_data = climate_data, bio_data = bio_data)
}

test_that(
  "run_randwin ('weightwin') returns a data frame with one row per repeat", {
  d <- make_randwin_data()
  result <- run_randwin(
    repeats      = 3,
    range        = c(0, 4),
    climate_data = d$climate_data,
    bio_data     = d$bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data),
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

test_that(
  "run_randwin ('weightwin') summary columns match weightfunc par labels", {
  d <- make_randwin_data()

  result_w <- run_randwin(
    repeats      = 2,
    range        = c(0, 4),
    climate_data = d$climate_data,
    bio_data     = d$bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data),
    window_type  = "weightwin",
    weightfunc   = "W",
    par          = c(1.25, 0.5),
    progress     = FALSE
  )
  expect_true(all(c("start_shape", "start_scale",
                    "end_shape",   "end_scale") %in% names(result_w)))

  result_f <- run_randwin(
    repeats      = 2,
    range        = c(0, 4),
    climate_data = d$climate_data,
    bio_data     = d$bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data),
    window_type  = "weightwin",
    weightfunc   = "F",
    par          = c(0.5, 2),
    progress     = FALSE
  )
  expect_true(all(c("start_scale", "start_shape",
                    "end_scale",   "end_shape") %in% names(result_f)))
  expect_false("start_loc" %in% names(result_f))
})

# exclude parameter -----------------------------------------------------------

# Larger climate series: bio dates at Jan 15/17/19, so range <= 10 is safe.
make_randwin_exclude_data <- function() {
  start <- as.Date("01/01/1979", format = "%d/%m/%Y")
  dates <- format(start + seq(0, 19), "%d/%m/%Y")
  climate_data <- data.frame(
    Date = dates,
    Temp = c(10, 15, 20, 10, 12, 8, 14, 18, 11, 13,
             9, 16,  7, 12, 11, 14,  8, 19, 13, 10)
  )
  bio_data <- data.frame(
    Date    = c("15/01/1979", "17/01/1979", "19/01/1979"),
    Mass    = c(100, 110, 120),
    climate = 0
  )
  list(climate_data = climate_data, bio_data = bio_data)
}

test_that("run_randwin passes exclude to slidingwin path correctly", {
  d <- make_randwin_exclude_data()
  # exclude = c(2, 3): removes windows with duration <= 2 AND end_day >= 3.
  # Enough non-excluded windows remain so each iteration returns a best window.
  result <- run_randwin(
    repeats      = 3,
    range        = c(0, 6),
    climate_data = d$climate_data,
    bio_data     = d$bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data),
    exclude      = c(2, 3),
    progress     = FALSE
  )

  expect_equal(nrow(result), 3L)
  expect_equal(result$Iteration, 1:3)

  # No returned window should fall in the excluded zone
  dur     <- result$Start_Day - result$End_Day + 1L
  too_old <- result$End_Day >= 3L
  expect_true(!any(dur <= 2L & too_old))
})

test_that("run_randwin errors when exclude removes all candidate windows", {
  d <- make_randwin_exclude_data()
  # range = c(1, 4): all windows have end_day >= 1.
  # exclude = c(100, 1): removes every window (100 >> any window duration).
  expect_error(
    run_randwin(
      repeats      = 2,
      range        = c(1, 4),
      climate_data = d$climate_data,
      bio_data     = d$bio_data,
      baseline     = lm(Mass ~ climate, data = bio_data),
      exclude      = c(100, 1),
      progress     = FALSE
    ),
    "exclude.*removed all candidate windows"
  )
})

test_that("run_randwin with NULL exclude matches run without exclude", {
  d <- make_randwin_exclude_data()
  set.seed(42)
  result_null <- run_randwin(
    repeats      = 2,
    range        = c(0, 4),
    climate_data = d$climate_data,
    bio_data     = d$bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data),
    exclude      = NULL,
    progress     = FALSE
  )
  set.seed(42)
  result_default <- run_randwin(
    repeats      = 2,
    range        = c(0, 4),
    climate_data = d$climate_data,
    bio_data     = d$bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data),
    progress     = FALSE
  )
  expect_equal(result_null, result_default)
})

test_that("run_randwin randomizes climate data correctly", {
  climate_data <- data.frame(
    Date = c(
      "01/01/1979", "02/01/1979", "03/01/1979",
      "04/01/1979", "05/01/1979"
    ),
    Temp = c(1, 2, 3, 4, 5)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "04/01/1979"),
    Mass = c(100, 110),
    climate = 0
  )

  set.seed(123)
  result1 <- run_randwin(
    repeats      = 5,
    range        = c(0, 1),
    climate_data = climate_data,
    bio_data     = bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data)
  )

  set.seed(456)
  result2 <- run_randwin(
    repeats      = 5,
    range        = c(0, 1),
    climate_data = climate_data,
    bio_data     = bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data)
  )

  expect_equal(nrow(result1), 5)
  expect_equal(nrow(result2), 5)
  expect_equal(names(result1), names(result2))
  expect_equal(result1$Iteration, result2$Iteration)
})
