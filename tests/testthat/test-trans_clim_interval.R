# Helper function to create climate data
## Each week (7 days) has a different temp
make_weekly_clim <- function() {
  dates <- format(
    seq.Date(as.Date("2020-01-01"), by = "day", length.out = 28),
    "%d/%m/%Y"
  )
  data.frame(Date = dates, Temp = c(0, 0, 0, 1, 2, 2, 2,
                                    1, 1, 1, 2, 3, 3, 3,
                                    2, 2, 2, 3, 4, 4, 4,
                                    3, 3, 3, 4, 5, 5, 5))
}

# Each month has different temp
## Not full data in each month
make_monthly_clim <- function() {
  jan <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10)
  feb <- seq.Date(as.Date("2020-02-01"), by = "day", length.out = 10)
  data.frame(
    Date = format(c(jan, feb), "%d/%m/%Y"),
    Temp = c(0, 0, 0, 0, 0, 2, 2, 2, 2, 2,
             1, 1, 1, 1, 1, 3, 3, 3, 3, 3),
    ## We add two climate vars. Rain is ignored when just calling Temp
    Rain = c(2, 2, 2, 2, 2, 4, 4, 4, 4, 4,
             5, 5, 5, 5, 5, 7, 7, 7, 7, 7)
  )
}

# ── cinterval = "day" ──────────────────────────────────────────────────────────

test_that("trans_clim_interval: day returns data unchanged", {
  clim <- make_weekly_clim()

  result <- trans_clim_interval(clim, cinterval = "day")
  
  ## Results should be the same but Date col is Date object
  expect_equal(clim |> 
                 mutate(Date = lubridate::dmy(Date)),
               result)
})

# ── cinterval = "month" ────────────────────────────────────────────────────────

test_that("trans_clim_interval: month aggregates to monthly mean (default)", {
  clim <- make_monthly_clim()

  result <- trans_clim_interval(clim, cinterval = "month")
  
  ## Expected result is one day per month with mean Temp
  result_expected <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")),
                                Temp = c(1, 2))
  expect_equal(result, result_expected)
})

test_that("trans_clim_interval: month aggregates to monthly sum with aggfunc", {
  clim <- make_monthly_clim()

  result <- trans_clim_interval(clim, cinterval = "month", aggfunc = sum)
  ## Expected result is one day per month with mean Temp
  result_expected <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")),
                                Temp = c(10, 20))
  expect_equal(result, result_expected)
})

# ── cinterval = "week" ─────────────────────────────────────────────────────────

test_that("trans_clim_interval: week aggregates to weekly means", {
  clim <- make_weekly_clim()

  result <- trans_clim_interval(clim, cinterval = "week")
  result_expected <- data.frame(Date = lubridate::dmy(c("01/01/2020", "08/01/2020",
                                                        "15/01/2020", "22/01/2020")),
                                Temp = c(1, 2, 3, 4))
  expect_equal(result, result_expected)
})

# ── Multiple xvar columns ──────────────────────────────────────────────────────

test_that("trans_clim_interval: multiple xvar, single aggfunc", {
  clim <- make_monthly_clim()
  result <- trans_clim_interval(clim, xvar = c("Temp", "Rain"),
                                  cinterval = "month", aggfunc = mean)
  result_expected <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")),
                                Temp = c(1, 2),
                                Rain = c(3, 6))
  expect_equal(result, result_expected)
})

test_that("trans_clim_interval: multiple xvar, different aggfuncs per column", {
  clim <- make_monthly_clim()
  result <- trans_clim_interval(clim, xvar = c("Temp", "Rain"),
                                  cinterval = "month",
                                  aggfunc = list(mean, sum))
  result_expected <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")),
                                Temp = c(1, 2),
                                Rain = c(30, 60))
  expect_equal(result, result_expected)
})

# ── Misc tests ───────────────────────────────────────────────────────────

test_that("trans_clim_interval: accepts Date class input", {
  clim <- make_monthly_clim()
  clim$Date <- as.Date(clim$Date, format = "%d/%m/%Y")  # pre-converted
  result <- trans_clim_interval(clim, cinterval = "month")
  result_expected <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01")),
                                Temp = c(1, 2))
  expect_equal(result, result_expected)
})

test_that("trans_clim_interval: non-default cdate column", {
  clim <- make_monthly_clim()
  names(clim)[names(clim) == "Date"] <- "Datum"
  result <- trans_clim_interval(clim, cdate = "Datum", cinterval = "month")
  result_expected <- data.frame(Datum = as.Date(c("2020-01-01", "2020-02-01")),
                                Temp = c(1, 2))
  expect_equal(result, result_expected)
})

# ── Error cases ────────────────────────────────────────────────────────────────

test_that("trans_clim_interval: errors on empty data frame", {
  clim <- data.frame(Date = character(0), Temp = numeric(0))
  expect_error(
    trans_clim_interval(clim, cinterval = "month"),
    "must contain atleast 1 row"
  )
})

test_that("trans_clim_interval: errors when xvar column missing", {
  clim <- make_monthly_clim()
  expect_error(
    trans_clim_interval(clim, xvar = "Snow", cinterval = "month"),
    "Snow"
  )
})

test_that("trans_clim_interval: errors when cdate column missing", {
  clim <- make_monthly_clim()
  expect_error(
    trans_clim_interval(clim, cdate = "Datum", xvar = "Temp", cinterval = "month"),
    "Datum"
  )
})

test_that("trans_clim_interval: errors on invalid cinterval", {
  clim <- make_monthly_clim()
  expect_error(
    trans_clim_interval(clim, cinterval = "year")
  )
})

test_that("trans_clim_interval: errors when aggfunc length mismatches xvar", {
  clim <- make_monthly_clim()
  expect_error(
    trans_clim_interval(clim, xvar = c("Temp", "Rain"),
                         cinterval = "month",
                         aggfunc = list(mean, sum, max)),
    "length of xvar"
  )
})

test_that("trans_clim_interval: errors when aggfunc contains non-function", {
  clim <- make_monthly_clim()
  expect_error(
    trans_clim_interval(clim, cinterval = "month", aggfunc = list("mean")),
    "all elements must be functions"
  )
})

test_that("trans_clim_interval: errors on unparseable date format", {
  clim <- data.frame(Date = "2020-01-01", Temp = 5)
  expect_error(
    trans_clim_interval(clim, cinterval = "month"),
    "DD/MM/YYYY"
  )
})

test_that("trans_clim_interval: errors when date column is wrong type", {
  clim <- data.frame(Date = 1, Temp = 5)
  expect_error(
    trans_clim_interval(clim, cinterval = "month"),
    "must be character"
  )
})
