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

test_that("run_weightwin ('W') returns a valid output", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    baseline = lm(Mass ~ climate, data = bio_data),
    par        = c(1.25, 0.5),
    plot_every = NULL
  )

  ## returns climwin_weightwin object
  expect_true(inherits(result, "S7_object"))
  expect_true(inherits(result@weightwin_summary, "data.frame"))
  expect_true(inherits(result@weightwin_output, "list"))
  expect_length(result@weightwin_output, 1)

  ## internal structure as expected
  out <- result@weightwin_output[[1]]
  expect_true(inherits(out$bestModel$model, "lm"))
  expect_true(inherits(out$bestModel$data, "data.frame"))
  expect_true("climate" %in% names(out$bestModel$data))

  ## Weights as expected
  weights <- result@weightwin_output[[1]]$weights$weights
  expect_length(weights, 5)           # length(0:4)
  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights >= 0))
})

test_that("run_weightwin errors on invalid weightfunc", {
  d <- make_test_data()
  expect_error(
    run_weightwin(
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      baseline = lm(Mass ~ climate, data = bio_data),
      weightfunc   = "X",
      par        = c(1, 3),
      plot_every = NULL
    ),
    "should be one of "
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
      baseline = lm(Mass ~ climate, data = bio_data),
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
      baseline = lm(Mass ~ climate, data = bio_data),
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
    baseline = lm(Mass ~ climate, data = bio_data),
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

# weightfunc stored in output ---------------------------------------------------

test_that("run_weightwin stores weightfunc in each output element", {
  d <- make_test_data()
  for (wf in c("W", "G")) {
    result <- run_weightwin(
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      baseline = lm(Mass ~ climate, data = bio_data),
      weightfunc = wf,
      par        = c(0.5, 0.5),
      plot_every = NULL
    )
    expect_equal(result@weightwin_output[[1]]$weightfunc, wf)
  }
})

# Gumbel (weightfunc = "G") ---------------------------------------------------

test_that("run_weightwin ('G') returns a valid climwin_weightwin object", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    baseline = lm(Mass ~ climate, data = bio_data),
    weightfunc = "G",
    par        = c(0.5, 0.5),
    plot_every = NULL
  )

  expect_true(inherits(result, "S7_object"))
  expect_true(inherits(result@weightwin_summary, "data.frame"))
  expect_length(result@weightwin_output, 1)
  
  weights <- result@weightwin_output[[1]]$weights$weights
  expect_length(weights, 5)
  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights >= 0))
})

# Frechet (weightfunc = "F") --------------------------------------------------

test_that("run_weightwin ('F') returns a valid climwin_weightwin object", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    baseline = lm(Mass ~ climate, data = bio_data),
    weightfunc = "F",
    par        = c(0.5, 2),   # scale, shape (loc fixed at 0)
    plot_every = NULL
  )

  expect_true(inherits(result, "S7_object"))
  expect_true(inherits(result@weightwin_summary, "data.frame"))
  expect_length(result@weightwin_output, 1)
  
  weights <- result@weightwin_output[[1]]$weights$weights
  expect_length(weights, 5)
  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights >= 0))
})

# Custom weightfunc (function) -----------------------------------------------

test_that("run_weightwin accepts a custom density function for weightfunc", {
  d <- make_test_data()
  # Use exponential density as a custom weight function
  exp_dfun <- function(x, rate) dexp(x, rate = rate)
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    baseline = lm(Mass ~ climate, data = bio_data),
    weightfunc = exp_dfun,
    par        = c(2),
    lower      = c(0.1),
    upper      = c(10),
    plot_every = NULL
  )

  expect_true(inherits(result, "S7_object"))
  expect_equal(result@weightwin_output[[1]]$weightfunc, "custom")
  weights <- result@weightwin_output[[1]]$weights$weights
  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights >= 0))
})

test_that("run_weightwin errors when custom weightfunc given without lower/upper", {
  d <- make_test_data()
  exp_dfun <- function(x, rate) dexp(x, rate = rate)
  expect_error(
    run_weightwin(
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      baseline = lm(Mass ~ climate, data = bio_data),
      weightfunc = exp_dfun,
      par        = c(2),
      plot_every = NULL
    ),
    "lower.*upper.*must be supplied"
  )
})
