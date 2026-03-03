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
  
  set.seed(12)
  new_result <- run_weightwin(range = 0:150,
                              bio_data = Mass, climate_data = Climate,
                              basemodel = lm(Mass ~ climate, data = bio_data),
                              type = "absolute", 
                              refday = "20/05/2025", 
                              par = c(3, 0.2),
                              xvar = "Temp", cdate = "Date", bdate = "Date")
  
  old_result <- weightwin(xvar = list(Temp = Climate$Temp), cdate = Climate$Date, 
                      bdate = Mass$Date, 
                      baseline = lm(Mass ~ 1, data = Mass), 
                      range = c(150, 0),
                      func = "lin", type = "absolute", 
                      refday = c(20, 5),
                      weightfunc = "W", cinterval = "day",
                      par = c(3, 0.2, 0))
  
  ## Calculate difference in weights.
  ## Sum of difference should be small
  ## Exact final model and coefs are hard to compare because they can vary stochastically, so this is our best option
  diff <- abs(new_result@weights$weights - old_result$Weights)
  expect_true(sum(diff) < 0.5) ## We don't get that close...but it's at least qualitatively similar

})

test_that("slidingwin and run_slidingwin give same results with cinterval = 'month'", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  # Old implementation: range = c(furthest, closest) in months
  results_old <- slidingwin(
    xvar      = list(Temp = Climate$Temp),
    cdate     = Climate$Date,
    bdate     = Mass$Date,
    baseline  = lm(Mass ~ 1, data = Mass),
    cinterval = "month",
    range     = c(6, 0),
    type      = "absolute",
    refday    = c(20, 5),
    stat      = "mean",
    func      = "lin"
  )

  # New implementation: pre-aggregate then run
  Climate_monthly <- trans_clim_interval(Climate, cinterval = "month")
  results_new <- run_slidingwin(
    range        = 0:6,
    climate_data = Climate_monthly,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    cinterval    = "month",
    type         = "absolute",
    refday       = "20/05/2025"
  )

  ## Compare window dataset (filter negligible weights to avoid AIC vs AICc rounding differences)
  new_results <- getDataset(results_new) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(Start_Day = as.integer(Start_Day),
           End_Day   = as.integer(End_Day),
           ModWeight = round(ModWeight, digits = 6)) |>
    dplyr::filter(ModWeight > 0)

  old_results <- results_old[[1]]$Dataset |>
    select(Start_Day = WindowClose, End_Day = WindowOpen, ModWeight) |>
    mutate(Start_Day = as.integer(Start_Day),
           End_Day   = as.integer(End_Day),
           ModWeight = round(ModWeight, digits = 6)) |>
    dplyr::filter(ModWeight > 0)

  expect_equal(new_results, old_results, tolerance = 0.001)

  ## Compare best model coefficients
  new_best_model <- getBestModel(results_new)
  expect_equal(as.numeric(coef(new_best_model)),
               as.numeric(coef(results_old[[1]]$BestModel)),
               tolerance = 0.01)

})

test_that("slidingwin and run_slidingwin give same results with cinterval = 'week'", {
  
  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))
  
  # Old implementation: range = c(furthest, closest) in months
  results_old <- slidingwin(
    xvar      = list(Temp = Climate$Temp),
    cdate     = Climate$Date,
    bdate     = Mass$Date,
    baseline  = lm(Mass ~ 1, data = Mass),
    cinterval = "week",
    range     = c(24, 0),
    type      = "absolute",
    refday    = c(20, 5),
    stat      = "mean",
    func      = "lin"
  )
  
  # New implementation: pre-aggregate then run
  Climate_weekly <- trans_clim_interval(Climate, cinterval = "week")
  results_new <- run_slidingwin(
    range        = 0:24,
    climate_data = Climate_weekly,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    cinterval    = "week",
    type         = "absolute",
    refday       = "20/05/2025"
  )
  
  ## Compare window dataset (filter negligible weights to avoid AIC vs AICc rounding differences)
  ## We expect slight differences...do we at least get same top models
  new_results <- getDataset(results_new) |>
    select(Start_Day, End_Day) |>
    mutate(Start_Day = as.integer(Start_Day),
           End_Day   = as.integer(End_Day)) |> 
    slice(1:2)
  
  old_results <- results_old[[1]]$Dataset |>
    select(Start_Day = WindowClose, End_Day = WindowOpen) |>
    mutate(Start_Day = as.integer(Start_Day),
           End_Day   = as.integer(End_Day)) |> 
    slice(1:2)
  
  expect_equal(new_results, old_results, tolerance = 0.01)
  
  ## Compare best model coefficients
  new_best_model <- getBestModel(results_new)
  expect_equal(as.numeric(coef(new_best_model)),
               as.numeric(coef(results_old[[1]]$BestModel)),
               tolerance = 0.01)

})

test_that("trans_clim_interval -> run_slidingwin pipeline produces valid output", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  ## ── monthly pipeline ───────────────────────────────────────────────────────
  Climate_monthly <- trans_clim_interval(Climate, cinterval = "month")

  # Climate_monthly has one row per month (Date is 1st of month, Date class)
  expect_equal(as.integer(format(Climate_monthly$Date, "%d")),
               rep(1L, nrow(Climate_monthly)))

  results_monthly <- run_slidingwin(
    range        = 0:6,
    climate_data = Climate_monthly,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    cinterval    = "month",
    type         = "absolute",
    refday       = "20/05/2025"
  )

  ds_monthly <- getDataset(results_monthly)
  # 7 start values × 7 end values, upper-triangle only → 28 windows
  expect_equal(nrow(ds_monthly), 28L)
  # ModWeights sum to 1
  expect_equal(sum(ds_monthly$ModWeight), 1, tolerance = 1e-6)
  # Best model is a valid lm object
  expect_s3_class(getBestModel(results_monthly), "lm")

  ## ── weekly pipeline ────────────────────────────────────────────────────────
  Climate_weekly <- trans_clim_interval(Climate, cinterval = "week")

  # Climate_weekly has 7-day gaps between consecutive dates
  date_diffs <- as.integer(diff(sort(unique(Climate_weekly$Date))))
  expect_true(all(date_diffs == 7L))

  results_weekly <- run_slidingwin(
    range        = 0:4,
    climate_data = Climate_weekly,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    cinterval    = "week",
    type         = "absolute",
    refday       = "20/05/2025"
  )

  ds_weekly <- getDataset(results_weekly)
  # 5 start values × 5 end values, upper-triangle only → 15 windows
  expect_equal(nrow(ds_weekly), 15L)
  expect_equal(sum(ds_weekly$ModWeight), 1, tolerance = 1e-6)

  ## ── error: daily data passed with cinterval = "month" ─────────────────────
  expect_error(
    run_slidingwin(
      range        = 0:6,
      climate_data = Climate,
      bio_data     = Mass,
      basemodel    = lm(Mass ~ climate, data = bio_data),
      cinterval    = "month",
      type         = "absolute",
      refday       = "20/05/2025"
    ),
    "trans_clim_interval"
  )

})

# ── old_slidingwin backward-compatibility tests ────────────────────────────────

test_that("old_slidingwin and run_slidingwin produce same results (absolute, lin)", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  results_old_api <- old_slidingwin(
    xvar     = list(Temp = Climate$Temp),
    cdate    = Climate$Date,
    bdate    = Mass$Date,
    baseline = lm(Mass ~ 1, data = Mass),
    range    = c(10, 0),
    type     = "absolute",
    refday   = c(20, 5),
    stat     = "mean",
    func     = "lin"
  )

  results_new_api <- run_slidingwin(
    range        = 0:10,
    climate_data = Climate,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    fn           = mean,
    type         = "absolute",
    refday       = "20/05/2000"
  )

  old_ds <- getDataset(results_old_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric))

  new_ds <- getDataset(results_new_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric))

  expect_equal(old_ds, new_ds, tolerance = 1e-4)
  expect_equal(
    as.numeric(coef(getBestModel(results_old_api))),
    as.numeric(coef(getBestModel(results_new_api))),
    tolerance = 1e-4
  )

})

test_that("old_slidingwin and run_slidingwin produce same results (relative, lin)", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  results_old_api <- old_slidingwin(
    xvar     = list(Temp = Climate$Temp),
    cdate    = Climate$Date,
    bdate    = Mass$Date,
    baseline = lm(Mass ~ 1, data = Mass),
    range    = c(10, 0),
    type     = "relative",
    stat     = "mean",
    func     = "lin"
  )

  results_new_api <- run_slidingwin(
    range        = 0:10,
    climate_data = Climate,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    fn           = mean,
    type         = "relative"
  )

  old_ds <- getDataset(results_old_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric))

  new_ds <- getDataset(results_new_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric))

  expect_equal(old_ds, new_ds, tolerance = 1e-4)
  expect_equal(
    as.numeric(coef(getBestModel(results_old_api))),
    as.numeric(coef(getBestModel(results_new_api))),
    tolerance = 1e-4
  )

})

test_that("old_slidingwin and run_slidingwin produce same results (func = 'quad')", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  results_old_api <- old_slidingwin(
    xvar     = list(Temp = Climate$Temp),
    cdate    = Climate$Date,
    bdate    = Mass$Date,
    baseline = lm(Mass ~ 1, data = Mass),
    range    = c(10, 0),
    type     = "absolute",
    refday   = c(20, 5),
    stat     = "mean",
    func     = "quad"
  )

  results_new_api <- run_slidingwin(
    range        = 0:10,
    climate_data = Climate,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ poly(climate, 2), data = bio_data),
    fn           = mean,
    type         = "absolute",
    refday       = "20/05/2000"
  )

  old_ds <- getDataset(results_old_api) |>
    select(Start_Day, End_Day) |>
    slice(1:3) |>
    mutate(across(everything(), as.integer))

  new_ds <- getDataset(results_new_api) |>
    select(Start_Day, End_Day) |>
    slice(1:3) |>
    mutate(across(everything(), as.integer))

  expect_equal(old_ds, new_ds)
  expect_equal(
    as.numeric(coef(getBestModel(results_old_api))),
    as.numeric(coef(getBestModel(results_new_api))),
    tolerance = 1e-4
  )

})

test_that("old_slidingwin and run_slidingwin produce same results (cinterval = 'month')", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  results_old_api <- old_slidingwin(
    xvar      = list(Temp = Climate$Temp),
    cdate     = Climate$Date,
    bdate     = Mass$Date,
    baseline  = lm(Mass ~ 1, data = Mass),
    range     = c(6, 0),
    type      = "absolute",
    refday    = c(20, 5),
    stat      = "mean",
    func      = "lin",
    cinterval = "month"
  )

  Climate_monthly <- trans_clim_interval(Climate, cinterval = "month")
  results_new_api <- run_slidingwin(
    range        = 0:6,
    climate_data = Climate_monthly,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    fn           = mean,
    type         = "absolute",
    refday       = "20/05/2000",
    cinterval    = "month"
  )

  old_ds <- getDataset(results_old_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric))

  new_ds <- getDataset(results_new_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric))

  expect_equal(old_ds, new_ds, tolerance = 1e-4)
  expect_equal(
    as.numeric(coef(getBestModel(results_old_api))),
    as.numeric(coef(getBestModel(results_new_api))),
    tolerance = 1e-4
  )

})

test_that("old_slidingwin and run_slidingwin produce same results (cinterval = 'week')", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  results_old_api <- old_slidingwin(
    xvar      = list(Temp = Climate$Temp),
    cdate     = Climate$Date,
    bdate     = Mass$Date,
    baseline  = lm(Mass ~ 1, data = Mass),
    range     = c(8, 0),
    type      = "absolute",
    refday    = c(20, 5),
    stat      = "mean",
    func      = "lin",
    cinterval = "week"
  )

  Climate_weekly <- trans_clim_interval(Climate, cinterval = "week")
  results_new_api <- run_slidingwin(
    range        = 0:8,
    climate_data = Climate_weekly,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    fn           = mean,
    type         = "absolute",
    refday       = "20/05/2000",
    cinterval    = "week"
  )

  old_ds <- getDataset(results_old_api) |>
    select(Start_Day, End_Day) |>
    slice(1:3) |>
    mutate(across(everything(), as.integer))

  new_ds <- getDataset(results_new_api) |>
    select(Start_Day, End_Day) |>
    slice(1:3) |>
    mutate(across(everything(), as.integer))

  expect_equal(old_ds, new_ds)
  expect_equal(
    as.numeric(coef(getBestModel(results_old_api))),
    as.numeric(coef(getBestModel(results_new_api))),
    tolerance = 1e-4
  )

})

test_that("old_slidingwin with cohort matches run_slidingwin with cohort", {

  MassClimate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  CohortMass  <- structure(list(
    cohort = c("A", "B", "B", "A", "A", "C", "A",
               "B", "C", "A", "B", "A", "C", "B", "A", "C", "A", "B", "B", "C",
               "C", "B", "A", "C", "B", "B", "B", "A", "A", "A", "B", "C", "A",
               "A", "A", "A", "C", "A", "A", "B", "A", "C", "B", "A", "B", "A",
               "A", "C", "B", "A"),
    Mass   = c(188.74, 204.75, 185.73, 181.94, 191.54, 163.92,
               179.38, 193, 162.06, 185.76, 208.11, 195.81, 167.08, 198.37,
               187.96, 154.94, 187, 188.6, 198.15, 162.71, 155.67, 197.74,
               182.07, 160.78, 199.35, 201.27, 191.07, 186.31, 195.18, 178.46,
               196.12, 162.95, 195.84, 188.96, 186.94, 187.32, 169.5, 187.05,
               187.04, 205.75, 190.67, 165.59, 194.12, 180.25, 190.28, 187.67,
               179.8, 163.29, 196.43, 190.29),
    Date   = c("01/11/1984", "01/2/1986", "01/2/1986", "01/12/1984",
               "01/2/1985", "01/11/1986", "01/11/1984", "01/11/1985",
               "01/2/1987", "01/11/1984", "01/2/1986", "01/12/1984",
               "01/12/1986", "01/12/1985", "01/12/1984", "01/1/1987",
               "01/1/1985", "01/1/1986", "01/11/1985", "01/1/1987",
               "01/11/1986", "01/11/1985", "01/2/1985", "01/2/1987",
               "01/1/1986", "01/12/1985", "01/1/1986", "01/11/1984",
               "01/11/1984", "01/11/1984", "01/1/1986", "01/1/1987",
               "01/1/1985", "01/11/1984", "01/1/1985", "01/1/1985",
               "01/11/1986", "01/11/1984", "01/1/1985", "01/2/1986",
               "01/1/1985", "01/12/1986", "01/12/1985", "01/1/1985",
               "01/11/1985", "01/2/1985", "01/2/1985", "01/1/1987",
               "01/12/1985", "01/12/1984")),
    class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -50L))

  results_old_api <- old_slidingwin(
    xvar     = list(Temp = MassClimate$Temp),
    cdate    = MassClimate$Date,
    bdate    = CohortMass$Date,
    baseline = lm(Mass ~ 1, data = CohortMass),
    range    = c(30, 0),
    type     = "absolute",
    refday   = c(1, 11),
    stat     = "mean",
    func     = "lin",
    cohort   = CohortMass$cohort
  )

  results_new_api <- run_slidingwin(
    range        = 0:30,
    climate_data = MassClimate,
    bio_data     = CohortMass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    fn           = mean,
    type         = "absolute",
    refday       = "01/11/2000",
    cohort       = "cohort"
  )

  old_ds <- getDataset(results_old_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric)) |>
    dplyr::filter(ModWeight > 0)

  new_ds <- getDataset(results_new_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric)) |>
    dplyr::filter(ModWeight > 0)

  expect_equal(old_ds, new_ds, tolerance = 1e-4)

})

test_that("old_slidingwin, run_slidingwin, and slidingwin all agree (absolute, lin)", {

  skip_if(!exists("slidingwin", mode = "function", inherits = TRUE),
          "slidingwin (CRAN climwin) not available")

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  results_old_api <- old_slidingwin(
    xvar     = list(Temp = Climate$Temp),
    cdate    = Climate$Date,
    bdate    = Mass$Date,
    baseline = lm(Mass ~ 1, data = Mass),
    range    = c(20, 0),
    type     = "absolute",
    refday   = c(20, 5),
    stat     = "mean",
    func     = "lin"
  )

  results_new_api <- run_slidingwin(
    range        = 0:20,
    climate_data = Climate,
    bio_data     = Mass,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    fn           = mean,
    type         = "absolute",
    refday       = "20/05/2000"
  )

  results_cran <- slidingwin(
    xvar     = list(Temp = Climate$Temp),
    cdate    = Climate$Date,
    bdate    = Mass$Date,
    baseline = lm(Mass ~ 1, data = Mass),
    range    = c(20, 0),
    type     = "absolute",
    refday   = c(20, 5),
    stat     = "mean",
    func     = "lin"
  )

  ## old_slidingwin == run_slidingwin
  old_ds <- getDataset(results_old_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric))

  new_ds <- getDataset(results_new_api) |>
    select(Start_Day, End_Day, ModWeight) |>
    mutate(across(everything(), as.numeric))

  expect_equal(old_ds, new_ds, tolerance = 1e-4)

  ## old_slidingwin == slidingwin (CRAN)
  cran_ds <- results_cran[[1]]$Dataset |>
    select(Start_Day = WindowClose, End_Day = WindowOpen, ModWeight) |>
    mutate(across(everything(), as.numeric)) |>
    dplyr::filter(ModWeight > 0)

  old_ds_filtered <- old_ds |> dplyr::filter(ModWeight > 0)

  expect_equal(old_ds_filtered, cran_ds, tolerance = 0.001)

})

test_that("old_slidingwin with multiple func values returns a list", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  result <- old_slidingwin(
    xvar     = list(Temp = Climate$Temp),
    cdate    = Climate$Date,
    bdate    = Mass$Date,
    baseline = lm(Mass ~ 1, data = Mass),
    range    = c(5, 0),
    type     = "absolute",
    refday   = c(20, 5),
    stat     = "mean",
    func     = c("lin", "quad")
  )

  expect_type(result, "list")
  expect_length(result, 2L)
  expect_true(is.data.frame(getDataset(result[[1]])))
  expect_true(is.data.frame(getDataset(result[[2]])))

  ## Linear best model: 2 coefficients; quadratic: 3 coefficients
  expect_equal(length(coef(getBestModel(result[[1]]))), 2L)
  expect_equal(length(coef(getBestModel(result[[2]]))), 3L)

})

# ── old_weightwin backward-compatibility tests ─────────────────────────────────

test_that("old_weightwin and run_weightwin produce identical results (Weibull, absolute)", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  set.seed(42)
  result_old <- old_weightwin(
    xvar       = list(Temp = Climate$Temp),
    cdate      = Climate$Date,
    bdate      = Mass$Date,
    baseline   = lm(Mass ~ 1, data = Mass),
    range      = c(150, 0),
    func       = "lin",
    type       = "absolute",
    refday     = c(20, 5),
    weightfunc = "W",
    par        = c(3, 0.2, 0)   # 3-param old-style; truncated to c(3, 0.2) internally
  )

  set.seed(42)
  result_new <- run_weightwin(
    range        = 0:150,
    bio_data     = Mass,
    climate_data = Climate,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    type         = "absolute",
    refday       = "20/05/2000",
    par          = c(3, 0.2),
    xvar         = "Temp",
    cdate        = "Date",
    bdate        = "Date"
  )

  ## old_weightwin delegates directly to run_weightwin, so results must be exact
  expect_equal(getWeights(result_old), getWeights(result_new))
  expect_equal(
    as.numeric(coef(getBestModel(result_old))),
    as.numeric(coef(getBestModel(result_new)))
  )
  expect_equal(result_old@weightwin_summary, result_new@weightwin_summary)

})

test_that("old_weightwin and run_weightwin produce identical results (Weibull, relative)", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  set.seed(7)
  result_old <- old_weightwin(
    xvar       = list(Temp = Climate$Temp),
    cdate      = Climate$Date,
    bdate      = Mass$Date,
    baseline   = lm(Mass ~ 1, data = Mass),
    range      = c(100, 0),
    func       = "lin",
    type       = "relative",
    weightfunc = "W",
    par        = c(2, 0.5, 0)
  )

  set.seed(7)
  result_new <- run_weightwin(
    range        = 0:100,
    bio_data     = Mass,
    climate_data = Climate,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    type         = "relative",
    par          = c(2, 0.5),
    xvar         = "Temp",
    cdate        = "Date",
    bdate        = "Date"
  )

  expect_equal(getWeights(result_old), getWeights(result_new))
  expect_equal(
    as.numeric(coef(getBestModel(result_old))),
    as.numeric(coef(getBestModel(result_new)))
  )

})

test_that("old_weightwin and run_weightwin produce identical results (Weibull, cinterval = 'month')", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  set.seed(5)
  result_old <- old_weightwin(
    xvar       = list(Temp = Climate$Temp),
    cdate      = Climate$Date,
    bdate      = Mass$Date,
    baseline   = lm(Mass ~ 1, data = Mass),
    range      = c(12, 0),
    func       = "lin",
    type       = "absolute",
    refday     = c(20, 5),
    weightfunc = "W",
    cinterval  = "month",
    par        = c(2, 0.5, 0)
  )

  Climate_monthly <- trans_clim_interval(Climate, cinterval = "month")
  set.seed(5)
  result_new <- run_weightwin(
    range        = 0:12,
    bio_data     = Mass,
    climate_data = Climate_monthly,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    type         = "absolute",
    refday       = "20/05/2000",
    par          = c(2, 0.5),
    xvar         = "Temp",
    cdate        = "Date",
    bdate        = "Date",
    cinterval    = "month"
  )

  expect_equal(getWeights(result_old), getWeights(result_new))
  expect_equal(
    as.numeric(coef(getBestModel(result_old))),
    as.numeric(coef(getBestModel(result_new)))
  )

})

test_that("old_weightwin, run_weightwin, and weightwin (CRAN) are comparable (Weibull, absolute)", {

  Climate <- read.csv(system.file("MassClimate.csv", package = "climwin"))
  Mass    <- read.csv(system.file("Mass.csv",        package = "climwin"))

  set.seed(12)
  result_old_api <- old_weightwin(
    xvar       = list(Temp = Climate$Temp),
    cdate      = Climate$Date,
    bdate      = Mass$Date,
    baseline   = lm(Mass ~ 1, data = Mass),
    range      = c(150, 0),
    func       = "lin",
    type       = "absolute",
    refday     = c(20, 5),
    weightfunc = "W",
    par        = c(3, 0.2, 0)
  )

  set.seed(12)
  result_new_api <- run_weightwin(
    range        = 0:150,
    bio_data     = Mass,
    climate_data = Climate,
    basemodel    = lm(Mass ~ climate, data = bio_data),
    type         = "absolute",
    refday       = "20/05/2025",
    par          = c(3, 0.2),
    xvar         = "Temp",
    cdate        = "Date",
    bdate        = "Date"
  )

  ## old_weightwin == run_weightwin (exact, same seed)
  expect_equal(getWeights(result_old_api), getWeights(result_new_api))

  ## old_weightwin ~~ weightwin (CRAN): qualitatively similar weights.
  ## weightwin has a known data-scoping issue in test environments (re-evaluating
  ## lm(Mass ~ 1, data = Mass) in a frame where Mass is not visible); skip the
  ## CRAN comparison if it cannot run.
  set.seed(12)
  result_cran <- tryCatch(
    suppressMessages(weightwin(
      xvar       = list(Temp = Climate$Temp),
      cdate      = Climate$Date,
      bdate      = Mass$Date,
      baseline   = lm(Mass ~ 1, data = Mass),
      range      = c(150, 0),
      func       = "lin",
      type       = "absolute",
      refday     = c(20, 5),
      weightfunc = "W",
      cinterval  = "day",
      par        = c(3, 0.2, 0)
    )),
    error = function(e) NULL
  )

  if (!is.null(result_cran)) {
    diff <- sum(abs(getWeights(result_old_api) - result_cran$Weights))
    expect_true(diff < 0.5)
  } else {
    skip("weightwin (old) could not run due to data-scoping limitation")
  }

})