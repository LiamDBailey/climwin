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
