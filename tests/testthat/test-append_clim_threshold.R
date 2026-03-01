test_that("append_clim_threshold: upper-only, continuous", {
  clim <- data.frame(Date = "01/01/2000", Temp = c(-5, 0, 5, 10, 15))

  result <- append_clim_threshold(clim, xvar = "Temp",
                                  upper = 10, binary = FALSE)

  # Adds a threshold column without modifying existing columns
  expect_true("threshold" %in% names(result))
  expect_equal(result$Temp, clim$Temp)

  # Values >= upper retained; values < upper become 0
  expect_equal(result$threshold, c(0, 0, 0, 10, 15))
})

test_that("append_clim_threshold: upper-only, binary", {
  clim <- data.frame(Date = "01/01/2000", Temp = c(-5, 0, 5, 10, 15))

  result <- append_clim_threshold(clim, xvar = "Temp",
                                  upper = 10, binary = TRUE)

  expect_equal(result$threshold, c(0L, 0L, 0L, 1L, 1L))
})

test_that("append_clim_threshold: lower-only, continuous", {
  clim <- data.frame(Date = "01/01/2000", Temp = c(-5, 0, 5, 10, 15))

  result <- append_clim_threshold(clim, xvar = "Temp",
                                  lower = 5, binary = FALSE)

  # Values < lower retained; values >= lower become 0
  expect_equal(result$threshold, c(-5, 0, 0, 0, 0))
})

test_that("append_clim_threshold: lower-only, binary", {
  clim <- data.frame(Date = "01/01/2000", Temp = c(-5, 0, 5, 10, 15))

  result <- append_clim_threshold(clim, xvar = "Temp",
                                  lower = 5, binary = TRUE)

  expect_equal(result$threshold, c(1L, 1L, 0L, 0L, 0L))
})

test_that("append_clim_threshold: upper and lower, continuous", {
  clim <- data.frame(Date = "01/01/2000", Temp = c(-5, 0, 5, 10, 15))

  result <- append_clim_threshold(clim, xvar = "Temp",
                                  lower = 0, upper = 10,
                                  binary = FALSE)

  # Values strictly between lower (0) and upper (10) retained; others 0
  expect_equal(result$threshold, c(0, 0, 5, 0, 0))
})

test_that("append_clim_threshold: upper and lower, binary", {
  clim <- data.frame(Date = "01/01/2000", Temp = c(-5, 0, 5, 10, 15))

  result <- append_clim_threshold(clim, xvar = "Temp",
                                  lower = 0, upper = 10,
                                  binary = TRUE)

  expect_equal(result$threshold, c(0L, 0L, 1L, 0L, 0L))
})

test_that("append_clim_threshold: boundary values at upper and lower", {
  clim <- data.frame(Date = "01/01/2000", Temp = c(0, 5, 10))

  # upper = 5: value at exactly 5 is >= upper, so retained
  result_upper <- append_clim_threshold(clim, xvar = "Temp",
                                        upper = 5, binary = FALSE)
  expect_equal(result_upper$threshold, c(0, 5, 10))

  # lower = 5: value at exactly 5 is not < lower, so becomes 0
  result_lower <- append_clim_threshold(clim, xvar = "Temp",
                                        lower = 5, binary = FALSE)
  expect_equal(result_lower$threshold, c(0, 0, 0))

  # lower = 0, upper = 10: boundaries not strictly inside, become 0
  result_both <- append_clim_threshold(clim, xvar = "Temp",
                                       lower = 0, upper = 10,
                                       binary = FALSE)
  expect_equal(result_both$threshold, c(0, 5, 0))
})

test_that("append_clim_threshold: non-default xvar column", {
  clim <- data.frame(Date = "01/01/2000", Rain = c(0, 2, 5, 10))

  result <- append_clim_threshold(clim, xvar = "Rain",
                                  upper = 5, binary = FALSE)

  expect_equal(result$threshold, c(0, 0, 5, 10))
  expect_equal(result$Rain, clim$Rain)
})

test_that("append_clim_threshold: original columns preserved", {
  clim <- data.frame(Date = c("01/01/2000", "02/01/2000"),
                     Temp = c(3, 12),
                     Rain = c(1, 2))

  result <- append_clim_threshold(clim, xvar = "Temp",
                                  upper = 10, binary = FALSE)

  expect_equal(result$Date, clim$Date)
  expect_equal(result$Rain, clim$Rain)
  expect_equal(ncol(result), ncol(clim) + 1L)
})

test_that("append_clim_threshold: errors when neither upper nor lower given", {
  clim <- data.frame(Temp = c(1, 2, 3))

  expect_error(
    append_clim_threshold(clim, xvar = "Temp"),
    "At least one of 'upper' or 'lower' must be provided"
  )
})

test_that("append_clim_threshold: errors when xvar column missing", {
  clim <- data.frame(Temp = c(1, 2, 3))

  expect_error(
    append_clim_threshold(clim, xvar = "Rain", upper = 2),
    "Rain"
  )
})

test_that("append_clim_threshold: errors on empty data frame", {
  clim <- data.frame(Temp = numeric(0))

  expect_error(
    append_clim_threshold(clim, xvar = "Temp", upper = 5),
    "at least 1 row"
  )
})

test_that("append_clim_threshold: errors when upper is non-numeric", {
  clim <- data.frame(Temp = c(1, 2, 3))

  expect_error(
    append_clim_threshold(clim, xvar = "Temp", upper = "high")
  )
})

test_that("append_clim_threshold: errors when binary is non-logical", {
  clim <- data.frame(Temp = c(1, 2, 3))

  expect_error(
    append_clim_threshold(clim, xvar = "Temp", upper = 2, binary = "yes")
  )
})
