test_that("run_slidingwin works with valid input", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  
  # Create a data frame with the response variable and climate
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    climate = 0
  )
  
  # Test function
  result <- run_slidingwin(range = 0:2,
                         climate_data = climate_data,
                         bio_data = bio_data,
                         basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5)
  expect_equal(names(result), c("Start_Date", "End_Date", "Start_Day", "End_Day", "AIC"))
  expect_equal(nrow(result), 6)  # 6 combinations: 0-0, 0-1, 0-2, 1-1, 1-2, 2-2
})

test_that("run_slidingwin works with different basemodel structures", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    Age = c(1, 2, 3),
    climate = 0
  )
  
  # Test with different model structures
  models <- list(
    lm(Mass ~ climate, data = bio_data),
    lm(Mass ~ climate + Age, data = bio_data),
    lm(Mass ~ climate * Age, data = bio_data)
  )
  
  for (basemodel in models) {
    result <- run_slidingwin(range = 0:2,
                           climate_data = climate_data,
                           bio_data = bio_data,
                           basemodel = basemodel)
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 5)
    expect_equal(names(result), c("Start_Date", "End_Date", "Start_Day", "End_Day", "AIC"))
    expect_equal(nrow(result), 6)  # 6 combinations: 0-0, 0-1, 0-2, 1-1, 1-2, 2-2
  }
})

test_that("run_slidingwin works with log(climate)", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(1, 2, 3)  # Use positive values for log(climate)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    climate = c(1, 2, 3)  # Use positive values for log(climate)
  )
  
  # Create a basemodel using log(climate)
  basemodel <- lm(Mass ~ log(climate), data = bio_data)
  
  # Test function
  result <- run_slidingwin(range = 0:2,
                         climate_data = climate_data,
                         bio_data = bio_data,
                         basemodel = basemodel)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5)
  expect_equal(names(result), c("Start_Date", "End_Date", "Start_Day", "End_Day", "AIC"))
  expect_equal(nrow(result), 6)  # 6 combinations: 0-0, 0-1, 0-2, 1-1, 1-2, 2-2
})

test_that("run_slidingwin gives identical results with parallel = TRUE and FALSE", {
  # Create sample data
  climate_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Temp = c(10, 15, 20)
  )
  
  bio_data <- data.frame(
    Date = c("01/01/1979", "02/01/1979", "03/01/1979"),
    Mass = c(100, 110, 120),
    climate = 0
  )
  basemodel <- lm(Mass ~ climate, data = bio_data)
  
  # Run with parallel = TRUE
  result_parallel <- run_slidingwin(range = 0:2,
                                  climate_data = climate_data,
                                  bio_data = bio_data,
                                  basemodel = basemodel,
                                  parallel = TRUE)
  
  # Run with parallel = FALSE
  result_sequential <- run_slidingwin(range = 0:2,
                                    climate_data = climate_data,
                                    bio_data = bio_data,
                                    basemodel = basemodel,
                                    parallel = FALSE)
  
  # Compare results
  expect_identical(result_parallel, result_sequential)
})

test_that("results and results_spatial are identical as shown in example", {
  # Load the actual example data files
  Climate <- read.csv("../../MassClimate.csv")
  Mass <- read.csv("../../Mass.csv")
  
  # Create bio_data with climate column for the basemodel
  bio_data <- Mass
  bio_data$climate <- 0  # Initialize climate column
  
  # Run standard analysis (without spatial)
  results <- run_slidingwin(range = 0:2, 
                          climate_data = Climate, 
                          bio_data = bio_data,
                          basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Create spatial version as shown in example
  Mass$site <- sample(c("A", "B"), size = nrow(Mass), replace = TRUE)
  Climate1 <- Climate
  Climate1$site <- "A"
  Climate2 <- Climate
  Climate2$site <- "B"
  Climate_site <- rbind(Climate1, Climate2)
  
  # Update bio_data with site column
  bio_data$site <- Mass$site
  
  # Run spatial analysis
  results_spatial <- run_slidingwin(range = 0:2, 
                          climate_data = Climate_site, 
                          bio_data = bio_data,
                          basemodel = lm(Mass ~ climate, data = bio_data),
                          spatial = "site")
  
  # Results should be identical (same AIC values and structure)
  expect_equal(results$AIC, results_spatial$AIC, tolerance = 1e-10)
  expect_equal(results$Start_Day, results_spatial$Start_Day)
  expect_equal(results$End_Day, results_spatial$End_Day)
  expect_equal(nrow(results), nrow(results_spatial))
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
    run_slidingwin(range = 0:1,
                   climate_data = climate_data,
                   bio_data = bio_data,
                   basemodel = lm(Mass ~ climate, data = bio_data),
                   spatial = "site"),
    "climate_data must contain column 'site'"
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
    run_slidingwin(range = 0:1,
                   climate_data = climate_data,
                   bio_data = bio_data,
                   basemodel = lm(Mass ~ climate, data = bio_data),
                   spatial = "site"),
    "bio_data must contain column 'site'"
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
    run_slidingwin(range = 0:1,
                   climate_data = climate_data,
                   bio_data = bio_data,
                   basemodel = lm(Mass ~ climate, data = bio_data),
                   spatial = "nonexistent_column"),
    "climate_data must contain column 'nonexistent_column'"
  )
}) 