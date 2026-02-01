test_that("MassWin results can be recreated", {
  
  # Example usage:
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  results <- run_slidingwin(range = 0:150, type = "absolute", fn = mean,
                            refday = "20/05/2025",
                            climate_data = Climate, 
                            bio_data = Mass,
                            basemodel = lm(Mass ~ climate, data = bio_data))
  
  # Access the dataset and best model
  ## Compare top models
  ## TODO: Compare full dataset
  new_results <- getDataset(results) |> 
    select(-AIC) |> 
    slice(1:6)
  old_results <- data.frame(Start_Day = c(15, 14, 15, 14, 13, 13),
                            End_Day = c(72, 72, 73, 73, 72, 73),
                            ModWeight = c(0.0268355,
                                          0.0235472,
                                          0.0232902,
                                          0.0218251,
                                          0.0207839,
                                          0.0198168))
  expect_equal(new_results, old_results, tolerance = 0.00001)
  
  ## Compare model coefs
  ## TODO: Compare exact model coefs
  new_best_model <- getBestModel(results)
  expect_equal(as.numeric(coef(new_best_model)),
               c(163.544, -4.481), tolerance = c(0.01))
  
  ## Compare model data
  ## TODO: Compare all data not just top slice
  new_best_model_data <- getBestModelData(results) |> 
    slice(1:6) |> 
    select(Mass, climate)
  old_best_model_data <- data.frame(Mass = c(140, 138, 136, 135, 134, 134),
                                    climate = c(6.068966, 6.160345, 6.781034, 6.877586, 6.713793, 6.120690))
  expect_equal(new_best_model_data, old_best_model_data, tolerance = c(0.0001))
  
})

test_that("Masswin with interaction", {
  
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  results <- run_slidingwin(range = 0:150, type = "absolute", fn = mean,
                            refday = "20/05/2025",
                            climate_data = Climate, 
                            bio_data = Mass,
                            basemodel = lm(Mass ~ climate*Age, data = bio_data))
  
  ## Compare model coefficients
  ## TODO: Compare full outputs
  new_best_model <- getBestModel(results)
  expect_equal(coef(new_best_model),
               c(`(Intercept)` = 170.2628, climate = -5.5466, Age = -2.6046, `climate:Age` = 0.4024), tolerance = 0.001)
  
})

test_that("Test cohort works", {
  
  MassClimate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  CohortMass <- structure(list(cohort = c("A", "B", "B", "A", "A", "C", "A", 
                                          "B", "C", "A", "B", "A", "C", "B", "A", "C", "A", "B", "B", "C", 
                                          "C", "B", "A", "C", "B", "B", "B", "A", "A", "A", "B", "C", "A", 
                                          "A", "A", "A", "C", "A", "A", "B", "A", "C", "B", "A", "B", "A", 
                                          "A", "C", "B", "A", "B", "C", "A", "C", "C", "A", "B", "A", "A", 
                                          "A", "C", "C", "C", "B", "B", "A", "C", "C", "A", "A", "A", "B", 
                                          "A", "B", "B", "B", "A", "B", "A", "C", "C", "A", "C", "B", "B", 
                                          "A", "C", "C", "C", "B", "B", "B", "A", "C", "C", "A", "C", "C", 
                                          "A", "A"), Mass = c(188.74, 204.75, 185.73, 181.94, 191.54, 163.92, 
                                                              179.38, 193, 162.06, 185.76, 208.11, 195.81, 167.08, 198.37, 
                                                              187.96, 154.94, 187, 188.6, 198.15, 162.71, 155.67, 197.74, 182.07, 
                                                              160.78, 199.35, 201.27, 191.07, 186.31, 195.18, 178.46, 196.12, 
                                                              162.95, 195.84, 188.96, 186.94, 187.32, 169.5, 187.05, 187.04, 
                                                              205.75, 190.67, 165.59, 194.12, 180.25, 190.28, 187.67, 179.8, 
                                                              163.29, 196.43, 190.29, 190.84, 167.61, 186.43, 159.2, 154.23, 
                                                              192.31, 196.46, 183.44, 177.79, 188.79, 160.75, 153.86, 166.05, 
                                                              189.69, 200.21, 182.8, 161.32, 161.51, 180.39, 189.69, 182.31, 
                                                              203.7, 181.84, 192.07, 199.08, 199.94, 183.99, 194.94, 184.24, 
                                                              158.87, 164.68, 188.77, 167.44, 200.34, 186.38, 191.22, 166.87, 
                                                              159.57, 158.92, 192.55, 193.26, 177.74, 185.28, 157.93, 166.69, 
                                                              189.95, 167.92, 163.34, 182.88, 187.72),
                               Date = c("01/11/1984", 
                                        "01/2/1986", "01/2/1986", "01/12/1984", "01/2/1985", "01/11/1986", 
                                        "01/11/1984", "01/11/1985", "01/2/1987", "01/11/1984", "01/2/1986", 
                                        "01/12/1984", "01/12/1986", "01/12/1985", "01/12/1984", "01/1/1987", 
                                        "01/1/1985", "01/1/1986", "01/11/1985", "01/1/1987", "01/11/1986", 
                                        "01/11/1985", "01/2/1985", "01/2/1987", "01/1/1986", "01/12/1985", 
                                        "01/1/1986", "01/11/1984", "01/11/1984", "01/11/1984", "01/1/1986", 
                                        "01/1/1987", "01/1/1985", "01/11/1984", "01/1/1985", "01/1/1985", 
                                        "01/11/1986", "01/11/1984", "01/1/1985", "01/2/1986", "01/1/1985", 
                                        "01/12/1986", "01/12/1985", "01/1/1985", "01/11/1985", "01/2/1985", 
                                        "01/2/1985", "01/1/1987", "01/12/1985", "01/12/1984", "01/12/1985", 
                                        "01/1/1987", "01/2/1985", "01/1/1987", "01/12/1986", "01/11/1984", 
                                        "01/12/1985", "01/2/1985", "01/11/1984", "01/11/1984", "01/2/1987", 
                                        "01/1/1987", "01/11/1986", "01/1/1986", "01/2/1986", "01/11/1984", 
                                        "01/2/1987", "01/12/1986", "01/12/1984", "01/1/1985", "01/1/1985", 
                                        "01/12/1985", "01/12/1984", "01/12/1985", "01/1/1986", "01/1/1986", 
                                        "01/2/1985", "01/11/1985", "01/1/1985", "01/12/1986", "01/11/1986", 
                                        "01/1/1985", "01/1/1987", "01/11/1985", "01/2/1986", "01/2/1985", 
                                        "01/2/1987", "01/1/1987", "01/2/1987", "01/12/1985", "01/1/1986", 
                                        "01/11/1985", "01/12/1984", "01/1/1987", "01/12/1986", "01/1/1985", 
                                        "01/1/1987", "01/1/1987", "01/1/1985", "01/2/1985")),
                          class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -100L))
  
  ## Without cohort
  results <- run_slidingwin(range = 0:100, type = "absolute", fn = mean,
                            refday = "01/11/2025",
                            climate_data = MassClimate, 
                            bio_data = CohortMass,
                            cohort = "cohort",
                            basemodel = lm(Mass ~ climate, data = bio_data))
  
  results_old <- slidingwin(xvar = list(Temp = MassClimate$Temp),
                            cdate = MassClimate$Date,
                            bdate = CohortMass$Date,
                            baseline = lm(Mass ~ 1, data = CohortMass),
                            cinterval = "day",
                            range = c(100, 0),
                            type = "absolute", refday = c(01, 11),
                            stat = "mean",
                            func = "lin",
                            cohort = CohortMass$cohort)
  
  ## Compare windows
  ## TODO: Things start to misalign at very poor windows (maybe because of using AIC rather than deltaAIC)
  ## Remove anything where weight is very small
  new_results <- getDataset(results) |> select(Start_Day, End_Day, ModWeight) |> 
    mutate(Start_Day = as.integer(Start_Day), End_Day = as.integer(End_Day), ModWeight = round(ModWeight, digits = 6)) |> 
    filter(ModWeight > 0)
  old_results <- results_old[[1]]$Dataset |> 
    select(Start_Day = WindowClose, End_Day = WindowOpen, ModWeight) |> 
    mutate(Start_Day = as.integer(Start_Day), End_Day = as.integer(End_Day), ModWeight = round(ModWeight, digits = 6)) |> 
    filter(ModWeight > 0)
  expect_equal(new_results, old_results, tolerance = 0.001)
  
})

test_that("Weightwin has backwards compatibility", {
  
  # Example usage:
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass <- read.csv(system.file("Mass.csv", package = "climwin"))
  
  new_result <- run_weightwin(range = 0:150,
                              bio_data = Mass, climate_data = Climate,
                              basemodel = lm(Mass ~ climate, data = bio_data),
                              par = c(3, 0.2),
                              xvar = "Temp", cdate = "Date", bdate = "Date")
  
  old_result <- weightwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date, 
                      bdate = Mass$Date, 
                      baseline = lm(Mass ~ 1, data = Mass), 
                      range = c(150, 0),
                      func = "lin", type = "relative", 
                      weightfunc = "W", cinterval = "day",
                      par = c(3, 0.2, 0))
  
  ## Calculate difference in weights.
  ## Sum of difference should be small
  diff <- new_result@weights$weights - old_result$Weights
  expect_true(sum(diff) < 0.00001)
  
})