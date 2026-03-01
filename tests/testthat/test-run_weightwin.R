# Shared test data ----------------------------------------------------------

make_test_data <- function() {
  climate_data <- data.frame(
    Date = c(
      "01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979",
      "06/01/1979", "07/01/1979", "08/01/1979", "09/01/1979", "10/01/1979"
    ),
    Temp = c(10, 15, 20, 10, 12, 8, 14, 18, 11, 13)
  )
  bio_data <- data.frame(
    Date = c("06/01/1979", "08/01/1979", "10/01/1979"),
    Mass = c(100, 110, 120)
  )
  list(climate_data = climate_data, bio_data = bio_data)
}

# Weibull (weightfunc = "W") --------------------------------------------------

test_that("run_weightwin ('W') returns a valid climwin_weightwin object", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    par        = c(1.25, 0.5),
    plot_every = NULL
  )

  expect_true(inherits(result, "S7_object"))
  expect_true(inherits(result@weightwin_summary, "data.frame"))
  expect_true(inherits(result@weightwin_output, "list"))
  expect_length(result@weightwin_output, 1)
})

test_that("run_weightwin ('W') output has correct inner structure", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    par        = c(1.25, 0.5),
    plot_every = NULL
  )

  out <- result@weightwin_output[[1]]
  expect_true(inherits(out$bestModel$model, "lm"))
  expect_true(inherits(out$bestModel$data, "data.frame"))
  expect_true("climate" %in% names(out$bestModel$data))
})

test_that("run_weightwin ('W') weights sum to 1 and have correct length", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    par        = c(1.25, 0.5),
    plot_every = NULL
  )

  weights <- result@weightwin_output[[1]]$weights$weights
  expect_length(weights, 5)           # length(0:4)
  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights >= 0))
})

# Uniform (weightfunc = "U") --------------------------------------------------

test_that("run_weightwin ('U') returns a valid climwin_weightwin object", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    weightfunc   = "U",
    par        = c(1, 3),
    plot_every = NULL
  )

  expect_true(inherits(result, "S7_object"))
  expect_true(inherits(result@weightwin_summary, "data.frame"))
  expect_length(result@weightwin_output, 1)
})

test_that("run_weightwin ('U') weights sum to 1 and are non-negative", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    weightfunc   = "U",
    par        = c(1, 3),
    plot_every = NULL
  )

  weights <- result@weightwin_output[[1]]$weights$weights
  expect_length(weights, 5)
  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights >= 0))
})

test_that("fit_weights_uniform produces correct uniform weights", {
  d <- make_test_data()
  # par = c(1, 3) on range 0:4 -> weights 0, 1/3, 1/3, 1/3, 0
  out <- fit_weights_uniform(
    range        = 0:4,
    bio_data     = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    par          = c(1, 3)
  )

  w <- out$weights
  expect_length(w, 5)
  expect_equal(sum(w), 1, tolerance = 1e-10)
  # Days outside [1, 3] have zero weight
  expect_equal(w[c(1, 5)], c(0, 0))
  # Days inside [1, 3] have equal weight
  expect_equal(w[2], w[3])
  expect_equal(w[3], w[4])
  expect_equal(w[2], 1 / 3, tolerance = 1e-10)
})

test_that("run_weightwin ('U') bounds are set from range automatically", {
  d <- make_test_data()
  # Providing Weibull-style bounds should be ignored for "U" — no error
  expect_no_error(
    run_weightwin(
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      basemodel  = lm(Mass ~ climate, data = bio_data),
      weightfunc   = "U",
      par        = c(1, 3),
      lower      = c(0.1, 0.1),   # ignored for "U"
      upper      = c(10, 1000),   # ignored for "U"
      plot_every = NULL
    )
  )
})

# Validation ----------------------------------------------------------------

test_that("run_weightwin errors on invalid weightfunc", {
  d <- make_test_data()
  expect_error(
    run_weightwin(
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      basemodel  = lm(Mass ~ climate, data = bio_data),
      weightfunc   = "X",
      par        = c(1, 3),
      plot_every = NULL
    )
  )
})

test_that("run_weightwin ('U') errors when par[1] > par[2]", {
  d <- make_test_data()
  expect_error(
    run_weightwin(
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      basemodel  = lm(Mass ~ climate, data = bio_data),
      weightfunc   = "U",
      par        = c(3, 1),     # start > end
      plot_every = NULL
    ),
    "par\\[1\\].*<=.*par\\[2\\]"
  )
})

test_that("run_weightwin ('W') errors when lower >= upper", {
  d <- make_test_data()
  expect_error(
    run_weightwin(
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      basemodel  = lm(Mass ~ climate, data = bio_data),
      par        = c(1, 0.5),
      lower      = c(5, 0.1),
      upper      = c(2, 1000),   # upper[1] < lower[1]
      plot_every = NULL
    ),
    "lower bounds must be less than upper bounds"
  )
})

test_that("run_weightwin with n > 1 uses lower/upper as default par_min/par_max", {
  d <- make_test_data()
  # Should succeed without explicitly providing par_min / par_max
  expect_no_error(
    run_weightwin(
      n          = 2,
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      basemodel  = lm(Mass ~ climate, data = bio_data),
      par        = c(1.25, 0.5),
      plot_every = NULL
    )
  )
})

# n > 1 iterations ----------------------------------------------------------

test_that("run_weightwin with n > 1 ('W') returns one output per iteration", {
  d <- make_test_data()
  result <- run_weightwin(
    n          = 2,
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    par        = c(1.25, 0.5),
    par_min    = c(0.1, 0.1),
    par_max    = c(5, 10),
    plot_every = NULL
  )

  expect_length(result@weightwin_output, 2)
  expect_equal(nrow(result@weightwin_summary), 2)
  # summary should be sorted ascending by AIC
  expect_true(result@weightwin_summary$AIC[1] <=
                result@weightwin_summary$AIC[2])
})
