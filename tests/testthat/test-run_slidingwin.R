test_that("run_slidingwin works with valid input", {
  
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )
  
  # Test function
  result <- run_slidingwin(range = c(0, 2),
                           climate_data = climate_data,
                           bio_data = bio_data,
                           baseline = lm(Mass ~ climate, data = bio_data))
  
  # Check structure
  expect_true(inherits(result, "S7_object"))
  expect_true(inherits(result@dataset, "data.frame"))
  expect_true(inherits(result@bestModel, "list"))
  expect_true(inherits(result@bestModel$data, "data.frame"))
  expect_true(inherits(result@bestModel$model, "lm"))
  expect_equal(nrow(result@dataset), 6)  # 6 combinations: 0-0, 0-1, 0-2, 1-1, 1-2, 2-2
})

test_that("run_slidingwin fails if we try to go back too far", {
  
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )
  
  # Test function
  expect_error(run_slidingwin(range = c(0, 20),
                           climate_data = climate_data,
                           bio_data = bio_data,
                           baseline = lm(Mass ~ climate, data = bio_data)),
               "'range' covers time periods not included in climate data. Consider adding more climate data or reducing range.")
})

test_that("run_slidingwin works with different baseline structures", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120),
    Age = c(1, 2, 3)
  )
  
  result1 <- run_slidingwin(range = c(0, 2),
                            climate_data = climate_data,
                            bio_data = bio_data,
                            baseline = lm(Mass ~ climate, data = bio_data))
  expect_true(inherits(result1, "S7_object"))
  expect_true(inherits(result1@dataset, "data.frame"))
  expect_true(inherits(result1@bestModel, "list"))
  expect_true(inherits(result1@bestModel$data, "data.frame"))
  expect_true(inherits(result1@bestModel$model, "lm"))
  expect_equal(nrow(result1@dataset), 6)
  
  result2 <- run_slidingwin(range = c(0, 2),
                            climate_data = climate_data,
                            bio_data = bio_data,
                            baseline = lm(Mass ~ climate + Age, data = bio_data))
  expect_true(inherits(result2, "S7_object"))
  expect_true(inherits(result2@dataset, "data.frame"))
  expect_true(inherits(result2@bestModel, "list"))
  expect_true(inherits(result2@bestModel$data, "data.frame"))
  expect_true(inherits(result2@bestModel$model, "lm"))
  expect_equal(nrow(result2@dataset), 6)
  
  result3 <- run_slidingwin(range = c(0, 2),
                            climate_data = climate_data,
                            bio_data = bio_data,
                            baseline = lm(Mass ~ climate * Age, data = bio_data))
  expect_true(inherits(result3, "S7_object"))
  expect_true(inherits(result3@dataset, "data.frame"))
  expect_true(inherits(result3@bestModel, "list"))
  expect_true(inherits(result3@bestModel$data, "data.frame"))
  expect_true(inherits(result3@bestModel$model, "lm"))
  expect_equal(nrow(result3@dataset), 6)
  
})

test_that("run_slidingwin works with log(climate)", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120),
    Age = c(1, 2, 3)
  )
  
  # Test function
  result <- run_slidingwin(range = c(0, 2),
                           climate_data = climate_data,
                           bio_data = bio_data,
                           baseline = lm(Mass ~ log(climate), data = bio_data))
  
  # Check structure
  expect_true(inherits(result, "S7_object"))
  expect_true(inherits(result@dataset, "data.frame"))
  expect_true(inherits(result@bestModel, "list"))
  expect_true(inherits(result@bestModel$data, "data.frame"))
  expect_true(inherits(result@bestModel$model, "lm"))
  expect_equal(nrow(result@dataset), 6)
})

test_that("run_slidingwin gives identical results with parallel = TRUE and FALSE", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120),
    Age = c(1, 2, 3)
  )
  # Run with parallel = TRUE
  result_parallel <- run_slidingwin(range = c(0, 2),
                                    climate_data = climate_data,
                                    bio_data = bio_data,
                                    baseline = lm(Mass ~ climate, data = bio_data),
                                    parallel = TRUE)
  
  # Run with parallel = FALSE
  result_sequential <- run_slidingwin(range = c(0, 2),
                                      climate_data = climate_data,
                                      bio_data = bio_data,
                                      baseline = lm(Mass ~ climate, data = bio_data),
                                      parallel = FALSE)
  
  # Compare results - compare the dataset components
  expect_identical(result_parallel@dataset, result_sequential@dataset)
  expect_identical(result_parallel@bestModel$data, result_sequential@bestModel$data)
  expect_identical(coef(result_parallel@bestModel$model),
                   coef(result_sequential@bestModel$model))
})

test_that("results and results_spatial are identical as shown in example", {
  
  # Create bio_data with climate column for the baseline
  bio_data <- Mass
  bio_data$climate <- 0  # Initialize climate column
  
  # Run standard analysis (without spatial)
  results <- run_slidingwin(range = c(0, 2), 
                            climate_data = MassClimate, 
                            bio_data = bio_data,
                            baseline = lm(Mass ~ climate, data = bio_data))
  
  # Create spatial version as shown in example
  Mass$site <- sample(c("A", "B"), size = nrow(Mass), replace = TRUE)
  Climate1 <- MassClimate
  Climate1$site <- "A"
  Climate2 <- MassClimate
  Climate2$site <- "B"
  Climate_site <- rbind(Climate1, Climate2)
  
  # Update bio_data with site column
  bio_data$site <- Mass$site
  
  # Run spatial analysis
  results_spatial <- run_slidingwin(range = c(0, 2), 
                                    climate_data = Climate_site, 
                                    bio_data = bio_data,
                                    baseline = lm(Mass ~ climate, data = bio_data),
                                    spatial = "site")
  
  # Results should be identical (same AIC values and structure)
  expect_equal(results@dataset$AIC, results_spatial@dataset$AIC, tolerance = 1e-10)
  expect_equal(results@dataset$Start_Day, results_spatial@dataset$Start_Day)
  expect_equal(results@dataset$End_Day, results_spatial@dataset$End_Day)
  expect_equal(nrow(results@dataset), nrow(results_spatial@dataset))
})

test_that("run_slidingwin fails when spatial column doesn't exist in climate_data", {
  # Create sample data without spatial column
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    climate = 0,
    site = c("A", "A", "A")  # bio_data has spatial column
  )
  
  # Should fail because climate_data doesn't have 'site' column
  expect_error(
    run_slidingwin(range = c(0, 1),
                   climate_data = climate_data,
                   bio_data = bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   spatial = "site"),
    "'climate_data': must contain column 'site'"
  )
})

test_that("run_slidingwin fails when spatial column doesn't exist in bio_data", {
  # Create sample data without spatial column in bio_data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20),
    site = c("A", "A", "A")  # climate_data has spatial column
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    climate = 0
    # bio_data missing spatial column
  )
  
  # Should fail because bio_data doesn't have 'site' column
  expect_error(
    run_slidingwin(range = c(0, 1),
                   climate_data = climate_data,
                   bio_data = bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   spatial = "site"),
    "'bio_data': must contain column 'site'"
  )
})

test_that("run_slidingwin fails when spatial column doesn't exist in both climate_data and bio_data", {
  # Create sample data without spatial column in either dataset
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    climate = 0
  )
  
  # Should fail because neither dataset has 'nonexistent_column' column
  expect_error(
    run_slidingwin(range = c(0, 1),
                   climate_data = climate_data,
                   bio_data = bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   spatial = "nonexistent_column"),
    "'climate_data': must contain column 'nonexistent_column'"
  )
}) 

# exclude parameter -----------------------------------------------------------

make_exclude_data <- function() {
  # 20 days of climate + 3 bio records — enough to test range = c(0, 15)
  list(
    climate_data = data.frame(
      Date = format(seq(as.Date("01/01/1979", "%d/%m/%Y"),
                        by = "day", length.out = 20), "%d/%m/%Y"),
      Temp = as.numeric(1:20)
    ),
    bio_data = data.frame(
      Date = c("16/01/1979", "17/01/1979", "18/01/1979"),
      Mass = c(100, 110, 120)
    )
  )
}

test_that("exclude removes short windows beyond the distance threshold", {
  d <- make_exclude_data()
  result <- run_slidingwin(
    range        = c(0, 10),
    climate_data = d$climate_data,
    bio_data     = d$bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data),
    exclude      = c(3, 5),
    progress     = FALSE
  )
  ds  <- getDataset(result)
  dur <- ds$Start_Day - ds$End_Day + 1L

  # No row should have duration <= 3 AND End_Day >= 5 simultaneously
  expect_false(any(dur <= 3 & ds$End_Day >= 5))
})

test_that("exclude reduces the total number of windows", {
  d <- make_exclude_data()
  res_full <- run_slidingwin(
    range = c(0, 10), climate_data = d$climate_data,
    bio_data = d$bio_data,
    baseline = lm(Mass ~ climate, data = bio_data),
    progress = FALSE
  )
  res_excl <- run_slidingwin(
    range = c(0, 10), climate_data = d$climate_data,
    bio_data = d$bio_data,
    baseline = lm(Mass ~ climate, data = bio_data),
    exclude  = c(3, 5),
    progress = FALSE
  )
  expect_lt(nrow(getDataset(res_excl)), nrow(getDataset(res_full)))
})

test_that("exclude = NULL produces identical results to omitting exclude", {
  d <- make_exclude_data()
  res_omit <- run_slidingwin(
    range = c(0, 5), climate_data = d$climate_data,
    bio_data = d$bio_data,
    baseline = lm(Mass ~ climate, data = bio_data),
    progress = FALSE
  )
  res_null <- run_slidingwin(
    range    = c(0, 5), climate_data = d$climate_data,
    bio_data = d$bio_data,
    baseline = lm(Mass ~ climate, data = bio_data),
    exclude  = NULL,
    progress = FALSE
  )
  expect_equal(getDataset(res_omit), getDataset(res_null))
})

test_that("ModWeight sums to 1 after exclude removes some windows", {
  d <- make_exclude_data()
  result <- run_slidingwin(
    range        = c(0, 10),
    climate_data = d$climate_data,
    bio_data     = d$bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data),
    exclude      = c(3, 5),
    progress     = FALSE
  )
  expect_equal(sum(getDataset(result)$ModWeight, na.rm = TRUE), 1,
               tolerance = 1e-6)
})

test_that("exclude errors when not length-2", {
  d <- make_exclude_data()
  expect_error(
    run_slidingwin(
      range = c(0, 10), climate_data = d$climate_data,
      bio_data = d$bio_data,
      baseline = lm(Mass ~ climate, data = bio_data),
      exclude  = c(3),
      progress = FALSE
    ),
    "two-element vector"
  )
})

test_that("exclude errors when values are non-positive", {
  d <- make_exclude_data()
  expect_error(
    run_slidingwin(
      range = c(0, 10), climate_data = d$climate_data,
      bio_data = d$bio_data,
      baseline = lm(Mass ~ climate, data = bio_data),
      exclude  = c(0, 5),
      progress = FALSE
    ),
    "must be positive"
  )
})

test_that("exclude errors when distance_limit exceeds range", {
  d <- make_exclude_data()
  expect_error(
    run_slidingwin(
      range = c(0, 10), climate_data = d$climate_data,
      bio_data = d$bio_data,
      baseline = lm(Mass ~ climate, data = bio_data),
      exclude  = c(3, 15),
      progress = FALSE
    ),
    "distance_limit exceeds the maximum range"
  )
})

test_that("exclude errors when all windows are removed", {
  d <- make_exclude_data()
  # range = c(1, 10): every window has end_day >= 1.
  # exclude = c(100, 1): duration_limit 100 >> any window size, so all removed.
  expect_error(
    run_slidingwin(
      range = c(1, 10), climate_data = d$climate_data,
      bio_data = d$bio_data,
      baseline = lm(Mass ~ climate, data = bio_data),
      exclude  = c(100, 1),
      progress = FALSE
    ),
    "has removed all candidate windows"
  )
})

# coef_fn parameter -----------------------------------------------------------
# Note: baseline must always be passed as an inline call (e.g. lm(...)), not a
# pre-computed variable, so that substitute() captures the full expression and
# eval() re-fits the model with updated climate data each window.

test_that("coef_fn = NULL gives identical output to omitting coef_fn", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )

  res_default <- run_slidingwin(range = c(0, 2), climate_data = climate_data,
                                bio_data = bio_data,
                                baseline = lm(Mass ~ climate, data = bio_data),
                                progress = FALSE)
  res_null    <- run_slidingwin(range = c(0, 2), climate_data = climate_data,
                                bio_data = bio_data,
                                baseline = lm(Mass ~ climate, data = bio_data),
                                coef_fn = NULL, progress = FALSE)

  expect_equal(getDataset(res_default), getDataset(res_null))
})

test_that("coef_fn returning unnamed numeric(1) adds a column named 'coef'", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )

  res <- run_slidingwin(range = c(0, 2), climate_data = climate_data,
                        bio_data = bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data),
                        coef_fn = function(m) coef(m)[["climate"]],
                        progress = FALSE)

  ds <- getDataset(res)
  expect_true("coef" %in% names(ds))
  expect_true(is.numeric(ds$coef))
  expect_equal(nrow(ds), 6L)
})

test_that("coef_fn returning named numeric(1) uses that name as column name", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )

  res <- run_slidingwin(range = c(0, 2), climate_data = climate_data,
                        bio_data = bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data),
                        coef_fn = function(m) c(beta = coef(m)[["climate"]]),
                        progress = FALSE)

  ds <- getDataset(res)
  expect_true("beta" %in% names(ds))
  expect_false("coef" %in% names(ds))
})

test_that("coef_fn returning a named numeric vector of length > 1 appends all columns", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )

  res <- run_slidingwin(
    range        = c(0, 2),
    climate_data = climate_data,
    bio_data     = bio_data,
    baseline     = lm(Mass ~ climate, data = bio_data),
    coef_fn      = function(m) {
      s <- summary(m)$coefficients
      c(beta = s["climate", "Estimate"], beta_se = s["climate", "Std. Error"])
    },
    progress = FALSE
  )

  ds <- getDataset(res)
  expect_true(all(c("beta", "beta_se") %in% names(ds)))
  expect_true(is.numeric(ds$beta))
  expect_true(is.numeric(ds$beta_se))
  expect_equal(nrow(ds), 6L)
})

test_that("coef_fn values match manual extraction for best window", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )

  res <- run_slidingwin(range = c(0, 2), climate_data = climate_data,
                        bio_data = bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data),
                        coef_fn = function(m) coef(m)[["climate"]],
                        progress = FALSE)

  ds <- getDataset(res)
  # Row 1 is the best window (sorted by AIC); its stored coef must match
  # the coefficient from the best model refitted at the end of run_slidingwin.
  expect_equal(ds$coef[1L], coef(getBestModel(res))[["climate"]], tolerance = 1e-8)
})

test_that("coef_fn errors on some windows produce NA rows, not a crash", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )

  # Succeeds on the first call, errors on all subsequent calls.
  # This lets first_coef be determined from window 1, then NA fills the rest.
  n_calls <- 0L
  mixed_coef_fn <- function(m) {
    n_calls <<- n_calls + 1L
    if (n_calls > 1L) stop("deliberate error")
    coef(m)[["climate"]]
  }

  res <- run_slidingwin(range = c(0, 2), climate_data = climate_data,
                        bio_data = bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data),
                        coef_fn = mixed_coef_fn, progress = FALSE)

  ds <- getDataset(res)
  expect_true("coef" %in% names(ds))
  expect_equal(nrow(ds), 6L)
  # First successful call produced a real value; the rest are NA
  expect_true(any(is.na(ds$coef)))
  expect_true(any(!is.na(ds$coef)))
})

test_that("coef_fn with non-numeric return type gives informative error", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )

  # list is not a numeric vector
  expect_error(
    run_slidingwin(range = c(0, 2), climate_data = climate_data,
                   bio_data = bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   coef_fn  = function(m) list(x = 1),
                   progress = FALSE),
    "must return a named numeric vector"
  )

  # data.frame is no longer accepted
  expect_error(
    run_slidingwin(range = c(0, 2), climate_data = climate_data,
                   bio_data = bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   coef_fn  = function(m) data.frame(x = 1),
                   progress = FALSE),
    "must return a named numeric vector"
  )
})

test_that("coef_fn with unnamed vector of length > 1 gives informative error", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )

  expect_error(
    run_slidingwin(range = c(0, 2), climate_data = climate_data,
                   bio_data = bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   coef_fn  = function(m) c(1.0, 2.0),  # unnamed, length 2
                   progress = FALSE),
    "unnamed numeric vector"
  )
})

test_that("coef_fn rejects non-function values", {
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979", "04/01/1979", "05/01/1979"),
    Temp = c(10, 15, 20, 10, 12)
  )
  bio_data <- data.frame(
    Date = c("03/01/1979", "03/01/1979", "05/01/1979"),
    Mass = c(100, 110, 120)
  )

  expect_error(
    run_slidingwin(range = c(0, 2), climate_data = climate_data,
                   bio_data = bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   coef_fn  = "not_a_function", progress = FALSE),
    "coef_fn"
  )
})

# k-fold cross-validation -------------------------------------------------------
# Helper data: 10 bio records give enough obs for k = 2/3 splits.
make_cv_data <- function() {
  list(
    climate_data = data.frame(
      Date = format(seq(as.Date("01/01/1979", "%d/%m/%Y"),
                        by = "day", length.out = 20), "%d/%m/%Y"),
      Temp = as.numeric(seq(5, 24))
    ),
    bio_data = data.frame(
      Date = format(seq(as.Date("11/01/1979", "%d/%m/%Y"),
                        by = "day", length.out = 10), "%d/%m/%Y"),
      Mass = as.numeric(seq(100, 109))
    )
  )
}

test_that("k = 0 produces no CV_score column", {
  d <- make_cv_data()
  res <- run_slidingwin(range = c(0, 3), climate_data = d$climate_data,
                        bio_data = d$bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data),
                        k = 0, progress = FALSE)
  expect_false("CV_score" %in% names(getDataset(res)))
})

test_that("k = 2 adds a numeric CV_score column", {
  d <- make_cv_data()
  set.seed(1)
  res <- run_slidingwin(range = c(0, 3), climate_data = d$climate_data,
                        bio_data = d$bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data),
                        k = 2, progress = FALSE)
  ds <- getDataset(res)
  expect_true("CV_score" %in% names(ds))
  expect_true(is.numeric(ds$CV_score))
  expect_equal(nrow(ds), 10L)  # (0+1+2+3) choose 2 with repeats = 10 windows
})

test_that("CV_score values differ across windows", {
  d <- make_cv_data()
  set.seed(42)
  res <- run_slidingwin(range = c(0, 3), climate_data = d$climate_data,
                        bio_data = d$bio_data,
                        baseline = lm(Mass ~ climate, data = bio_data),
                        k = 2, progress = FALSE)
  expect_gt(var(getDataset(res)$CV_score, na.rm = TRUE), 0)
})

test_that("k = 0 and k = 2 produce identical AIC values", {
  d <- make_cv_data()
  res0 <- run_slidingwin(range = c(0, 3), climate_data = d$climate_data,
                         bio_data = d$bio_data,
                         baseline = lm(Mass ~ climate, data = bio_data),
                         k = 0, progress = FALSE)
  set.seed(1)
  res2 <- run_slidingwin(range = c(0, 3), climate_data = d$climate_data,
                         bio_data = d$bio_data,
                         baseline = lm(Mass ~ climate, data = bio_data),
                         k = 2, progress = FALSE)
  expect_equal(getDataset(res0)$AIC, getDataset(res2)$AIC, tolerance = 1e-10)
})

test_that("k = 1 throws an error", {
  d <- make_cv_data()
  expect_error(
    run_slidingwin(range = c(0, 2), climate_data = d$climate_data,
                   bio_data = d$bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   k = 1, progress = FALSE),
    "must be 0.*or.*>= 2"
  )
})

test_that("k > nrow(bio_data) throws an error", {
  d <- make_cv_data()
  expect_error(
    run_slidingwin(range = c(0, 2), climate_data = d$climate_data,
                   bio_data = d$bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   k = 999, progress = FALSE),
    "cannot exceed number of observations"
  )
})

test_that("custom predict_fn is used during CV", {
  d <- make_cv_data()
  calls <- 0L
  spy_predict <- function(m, newdata) {
    calls <<- calls + 1L
    predict(m, newdata = newdata)
  }
  set.seed(1)
  run_slidingwin(range = c(0, 2), climate_data = d$climate_data,
                 bio_data = d$bio_data,
                 baseline = lm(Mass ~ climate, data = bio_data),
                 k = 2, predict_fn = spy_predict, progress = FALSE)
  # 6 windows × 2 folds = 12 predict calls
  expect_equal(calls, 12L)
})

test_that("predict_fn rejects non-function values", {
  d <- make_cv_data()
  expect_error(
    run_slidingwin(range = c(0, 2), climate_data = d$climate_data,
                   bio_data = d$bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   predict_fn = "not_a_function", progress = FALSE),
    "predict_fn"
  )
})

test_that("parallel = TRUE with k = 2 produces same CV_score as serial", {
  d <- make_cv_data()
  set.seed(7)
  res_serial <- run_slidingwin(range = c(0, 3), climate_data = d$climate_data,
                               bio_data = d$bio_data,
                               baseline = lm(Mass ~ climate, data = bio_data),
                               k = 2, parallel = FALSE, progress = FALSE)
  set.seed(7)
  res_parallel <- run_slidingwin(range = c(0, 3), climate_data = d$climate_data,
                                 bio_data = d$bio_data,
                                 baseline = lm(Mass ~ climate, data = bio_data),
                                 k = 2, parallel = TRUE, progress = FALSE)
  expect_equal(getDataset(res_serial)$CV_score,
               getDataset(res_parallel)$CV_score,
               tolerance = 1e-10)
})

test_that("custom CV_func changes CV_score values", {
  d <- make_cv_data()
  set.seed(42)
  res_mse <- run_slidingwin(range = c(0, 2), climate_data = d$climate_data,
                            bio_data = d$bio_data,
                            baseline = lm(Mass ~ climate, data = bio_data),
                            k = 2, progress = FALSE)
  set.seed(42)
  res_mae <- run_slidingwin(range = c(0, 2), climate_data = d$climate_data,
                            bio_data = d$bio_data,
                            baseline = lm(Mass ~ climate, data = bio_data),
                            k = 2,
                            CV_func = function(predicted, observed) mean(abs(predicted - observed)),
                            progress = FALSE)
  expect_false(identical(getDataset(res_mse)$CV_score,
                         getDataset(res_mae)$CV_score))
})

test_that("CV_func rejects non-function values", {
  d <- make_cv_data()
  expect_error(
    run_slidingwin(range = c(0, 2), climate_data = d$climate_data,
                   bio_data = d$bio_data,
                   baseline = lm(Mass ~ climate, data = bio_data),
                   k = 2, CV_func = "not_a_function", progress = FALSE),
    "CV_func"
  )
})
