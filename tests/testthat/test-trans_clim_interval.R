# Helper: 28 days of daily data, 4 weeks of constant temperatures
make_weekly_clim <- function() {
  dates <- format(
    seq.Date(as.Date("2020-01-01"), by = "day", length.out = 28),
    "%d/%m/%Y"
  )
  data.frame(Date = dates, Temp = rep(1:4, each = 7))
}

# Helper: two calendar months of daily data
make_monthly_clim <- function() {
  jan <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10)
  feb <- seq.Date(as.Date("2020-02-01"), by = "day", length.out = 10)
  data.frame(
    Date = format(c(jan, feb), "%d/%m/%Y"),
    Temp = c(rep(1, 10), rep(2, 10))
  )
}

# ── cinterval = "day" ──────────────────────────────────────────────────────────

test_that("trans_clim_interval: day returns data unchanged", {
  clim <- make_weekly_clim()

  result <- trans_clim_interval(clim, cinterval = "day")

  expect_equal(result$Temp, clim$Temp)
  expect_equal(nrow(result), nrow(clim))
  # All original columns preserved
  expect_true(all(names(clim) %in% names(result)))
})

# ── cinterval = "month" ────────────────────────────────────────────────────────

test_that("trans_clim_interval: month aggregates to monthly means", {
  clim <- make_monthly_clim()

  result <- trans_clim_interval(clim, cinterval = "month")

  expect_equal(nrow(result), 2L)
  expect_equal(result$Temp, c(1, 2))
  expect_equal(result$Date,
               as.Date(c("2020-01-01", "2020-02-01")))
})

test_that("trans_clim_interval: month sum aggregation", {
  clim <- make_monthly_clim()

  result <- trans_clim_interval(clim, cinterval = "month", aggfunc = sum)

  # 10 days × 1 = 10 for Jan, 10 × 2 = 20 for Feb
  expect_equal(result$Temp, c(10, 20))
})

# ── cinterval = "week" ─────────────────────────────────────────────────────────

test_that("trans_clim_interval: week aggregates to weekly means", {
  clim <- make_weekly_clim()

  result <- trans_clim_interval(clim, cinterval = "week")

  expect_equal(nrow(result), 4L)
  expect_equal(result$Temp, c(1, 2, 3, 4))
  # First week label is the first date
  expect_equal(result$Date[1], as.Date("2020-01-01"))
  expect_equal(result$Date[2], as.Date("2020-01-08"))
})

test_that("trans_clim_interval: week with partial final week", {
  # 10 days: full week 1 (7 days, Temp=1) + partial week 2 (3 days, Temp=2)
  dates <- format(
    seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10),
    "%d/%m/%Y"
  )
  clim <- data.frame(Date = dates, Temp = c(rep(1, 7), rep(2, 3)))

  result <- trans_clim_interval(clim, cinterval = "week")

  expect_equal(nrow(result), 2L)
  expect_equal(result$Temp, c(1, 2))
})

# ── Multiple xvar columns ──────────────────────────────────────────────────────

test_that("trans_clim_interval: multiple xvar, single aggfunc", {
  clim <- make_monthly_clim()
  clim$Rain <- c(rep(3, 10), rep(6, 10))

  result <- trans_clim_interval(clim, xvar = c("Temp", "Rain"),
                                  cinterval = "month", aggfunc = mean)

  expect_equal(result$Temp, c(1, 2))
  expect_equal(result$Rain, c(3, 6))
})

test_that("trans_clim_interval: multiple xvar, different aggfuncs per column", {
  clim <- make_monthly_clim()
  clim$Rain <- c(rep(3, 10), rep(6, 10))

  result <- trans_clim_interval(clim, xvar = c("Temp", "Rain"),
                                  cinterval = "month",
                                  aggfunc = list(mean, sum))

  # Temp: mean(1,1,...) = 1, mean(2,2,...) = 2
  expect_equal(result$Temp, c(1, 2))
  # Rain: sum(3*10) = 30, sum(6*10) = 60
  expect_equal(result$Rain, c(30, 60))
})

# ── Output structure ───────────────────────────────────────────────────────────

test_that("trans_clim_interval: non-xvar columns dropped after aggregation", {
  clim <- make_monthly_clim()
  clim$Extra <- 99  # not in xvar

  result <- trans_clim_interval(clim, xvar = "Temp", cinterval = "month")

  expect_false("Extra" %in% names(result))
  expect_true("Date"  %in% names(result))
  expect_true("Temp"  %in% names(result))
  expect_equal(ncol(result), 2L)
})

test_that("trans_clim_interval: date column is Date class in output", {
  clim <- make_monthly_clim()  # character dates

  result <- trans_clim_interval(clim, cinterval = "month")

  expect_s3_class(result$Date, "Date")
})

test_that("trans_clim_interval: accepts Date class input", {
  clim <- make_monthly_clim()
  clim$Date <- as.Date(clim$Date, format = "%d/%m/%Y")  # pre-converted

  result <- trans_clim_interval(clim, cinterval = "month")

  expect_equal(nrow(result), 2L)
  expect_equal(result$Temp, c(1, 2))
})

test_that("trans_clim_interval: non-default cdate column", {
  clim <- make_monthly_clim()
  names(clim)[names(clim) == "Date"] <- "Datum"

  result <- trans_clim_interval(clim, cdate = "Datum", cinterval = "month")

  expect_equal(nrow(result), 2L)
  expect_true("Datum" %in% names(result))
})

# ── append_clim_threshold compatibility ───────────────────────────────────────

test_that("append_clim_threshold works on trans_clim_interval month output", {
  clim <- make_monthly_clim()  # Jan mean=1, Feb mean=2

  monthly <- trans_clim_interval(clim, cinterval = "month")
  result  <- append_clim_threshold(monthly, xvar = "Temp",
                                   upper = 1.5, binary = FALSE)

  # Jan Temp = 1 < 1.5 → 0; Feb Temp = 2 >= 1.5 → retained
  expect_equal(result$threshold, c(0, 2))
  expect_equal(nrow(result), 2L)
  expect_true("Date" %in% names(result))
})

test_that("append_clim_threshold binary on trans_clim_interval week output", {
  clim <- make_weekly_clim()  # weekly means: 1, 2, 3, 4

  weekly <- trans_clim_interval(clim, cinterval = "week")
  result  <- append_clim_threshold(weekly, xvar = "Temp",
                                   upper = 2, binary = TRUE)

  # Temp >= 2: weeks 2,3,4 → 1; week 1 → 0
  expect_equal(result$threshold, c(0L, 1L, 1L, 1L))
})

# ── Error cases ────────────────────────────────────────────────────────────────

test_that("trans_clim_interval: errors on empty data frame", {
  clim <- data.frame(Date = character(0), Temp = numeric(0))

  expect_error(
    trans_clim_interval(clim, cinterval = "month"),
    "at least 1 row"
  )
})

test_that("trans_clim_interval: errors when xvar column missing", {
  clim <- data.frame(Date = "01/01/2020", Temp = 5)

  expect_error(
    trans_clim_interval(clim, xvar = "Rain", cinterval = "month"),
    "Rain"
  )
})

test_that("trans_clim_interval: errors when cdate column missing", {
  clim <- data.frame(Date = "01/01/2020", Temp = 5)

  expect_error(
    trans_clim_interval(clim, cdate = "Datum", cinterval = "month"),
    "Datum"
  )
})

test_that("trans_clim_interval: errors on invalid cinterval", {
  clim <- data.frame(Date = "01/01/2020", Temp = 5)

  expect_error(
    trans_clim_interval(clim, cinterval = "year")
  )
})

test_that("trans_clim_interval: errors when aggfunc length mismatches xvar", {
  clim <- make_monthly_clim()
  clim$Rain <- 1

  expect_error(
    trans_clim_interval(clim, xvar = c("Temp", "Rain"),
                         cinterval = "month",
                         aggfunc = list(mean, sum, max)),
    "length"
  )
})

test_that("trans_clim_interval: errors when aggfunc contains non-function", {
  clim <- make_monthly_clim()

  expect_error(
    trans_clim_interval(clim, cinterval = "month", aggfunc = list("mean"))
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
  clim <- data.frame(Date = 20200101L, Temp = 5)

  expect_error(
    trans_clim_interval(clim, cinterval = "month"),
    "character"
  )
})
