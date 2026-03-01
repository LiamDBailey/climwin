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

# weightfunc stored in output ---------------------------------------------------

test_that("run_weightwin stores weightfunc in each output element", {
  d <- make_test_data()
  for (wf in c("W", "G")) {
    par <- c(0.5, 0.5)
    result <- run_weightwin(
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      basemodel  = lm(Mass ~ climate, data = bio_data),
      weightfunc = wf,
      par        = par,
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
    basemodel  = lm(Mass ~ climate, data = bio_data),
    weightfunc = "G",
    par        = c(0.5, 0.5),
    plot_every = NULL
  )

  expect_true(inherits(result, "S7_object"))
  expect_true(inherits(result@weightwin_summary, "data.frame"))
  expect_length(result@weightwin_output, 1)
})

test_that("run_weightwin ('G') weights sum to 1 and are non-negative", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    weightfunc = "G",
    par        = c(0.5, 0.5),
    plot_every = NULL
  )

  weights <- result@weightwin_output[[1]]$weights$weights
  expect_length(weights, 5)
  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights >= 0))
})

test_that("fit_weights (Gumbel dfun) produces normalised non-negative weights", {
  d    <- make_test_data()
  dfun <- function(x, loc, scale) evd::dgumbel(x, loc = loc, scale = scale)
  out  <- fit_weights(
    range        = 0:4,
    bio_data     = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    dfun         = dfun,
    par          = c(0.5, 0.3)
  )
  w <- out$weights
  expect_length(w, 5)
  expect_equal(sum(w), 1, tolerance = 1e-10)
  expect_true(all(w >= 0))
})

# Frechet (weightfunc = "F") --------------------------------------------------

test_that("run_weightwin ('F') returns a valid climwin_weightwin object", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    weightfunc = "F",
    par        = c(0.5, 2),   # scale, shape (loc fixed at 0)
    plot_every = NULL
  )

  expect_true(inherits(result, "S7_object"))
  expect_true(inherits(result@weightwin_summary, "data.frame"))
  expect_length(result@weightwin_output, 1)
})

test_that("run_weightwin ('F') weights sum to 1 and are non-negative", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    weightfunc = "F",
    par        = c(0.5, 2),
    plot_every = NULL
  )

  weights <- result@weightwin_output[[1]]$weights$weights
  expect_length(weights, 5)
  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights >= 0))
})

test_that("fit_weights (Frechet dfun, loc=0) produces normalised non-negative weights", {
  d    <- make_test_data()
  dfun <- function(x, scale, shape) evd::dfrechet(x, loc = 0, scale = scale, shape = shape)
  out  <- fit_weights(
    range        = 0:4,
    bio_data     = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    dfun         = dfun,
    par          = c(0.5, 2)
  )
  w <- out$weights
  expect_length(w, 5)
  expect_equal(sum(w), 1, tolerance = 1e-10)
  expect_true(all(w >= 0))
})

test_that("run_weightwin ('F') errors when par does not have 2 elements", {
  d <- make_test_data()
  expect_error(
    run_weightwin(
      range      = 0:4,
      bio_data   = d$bio_data,
      climate_data = d$climate_data,
      cdate = "Date", bdate = "Date", xvar = "Temp",
      basemodel  = lm(Mass ~ climate, data = bio_data),
      weightfunc = "F",
      par        = c(0.1, 0.5, 2),   # 3 elements — loc no longer a free param
      plot_every = NULL
    ),
    "par must have 2 elements"
  )
})

test_that("run_weightwin ('F') summary has scale/shape start/end columns", {
  d <- make_test_data()
  result <- run_weightwin(
    range      = 0:4,
    bio_data   = d$bio_data,
    climate_data = d$climate_data,
    cdate = "Date", bdate = "Date", xvar = "Temp",
    basemodel  = lm(Mass ~ climate, data = bio_data),
    weightfunc = "F",
    par        = c(0.5, 2),
    plot_every = NULL
  )

  s <- result@weightwin_summary
  expect_true(all(c("start_scale", "start_shape") %in% names(s)))
  expect_true(all(c("end_scale",   "end_shape")   %in% names(s)))
  expect_false("start_loc" %in% names(s))
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
    basemodel  = lm(Mass ~ climate, data = bio_data),
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
      basemodel  = lm(Mass ~ climate, data = bio_data),
      weightfunc = exp_dfun,
      par        = c(2),
      plot_every = NULL
    ),
    "lower.*upper.*must be supplied"
  )
})
