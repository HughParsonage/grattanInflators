library(data.table)
library(grattanInflators)

# Use the bundled snapshot even if the machine has downloaded revised data.
local({
  old_dir <- Sys.getenv("R_USER_DATA_DIR", unset = NA_character_)
  old_options <- options(grattanInflators.env = new.env(parent = emptyenv()))
  on.exit({
    options(old_options)
    if (is.na(old_dir)) Sys.unsetenv("R_USER_DATA_DIR") else Sys.setenv(R_USER_DATA_DIR = old_dir)
  })
  Sys.setenv(R_USER_DATA_DIR = tempfile("earnings-empty-cache-"))
  getters <- list(awe_original, awe_seasonal, awe_trend,
                  awote_original, awote_seasonal, awote_trend)
  ids <- c("A85002157R", "A84998735A", "A84990050R",
           "A85002148L", "A84998729F", "A84990044V")
  expect_equal(content2series_id(c("awe", "awote"), c("original", "seasonal", "trend")), ids)
  # Published May 2026 levels, independently read from ABS tables 1-3.
  latest <- c(1579.2, 1579.2, 1579.4, 2083.7, 2083.7, 2084.2)
  for (i in seq_along(getters)) {
    index <- getters[[i]]()
    expect_equal(names(index), c("date", "value"))
    expect_true(inherits(index$date, "IDate"))
    expect_equal(unique(diff(12L * year(index$date) + month(index$date))), 6L)
    expect_equal(last(index$date), as.IDate("2026-05-15"))
    expect_equal(last(index$value), latest[i])
    expect_equal(nrow(index), if (i %in% c(1L, 4L)) 64L else 29L)
    f <- if (i <= 3L) awe_inflator else awote_inflator
    # Every observation must be reached at its published date.
    expect_equal(f(index$date[1L], index$date, series = index, check = 2L),
                 index$value / index$value[1L])
    expect_equal(f(index$date, index$date[1L], series = index, check = 2L),
                 index$value[1L] / index$value)
    extended <- getters[[i]]("4%")
    n <- nrow(index)
    expect_equal(extended$value[n + 2L] / extended$value[n], 1.04)
    expect_equal(unique(diff(12L * year(extended$date) + month(extended$date))), 6L)
    expect_equal(getters[[i]](), index) # custom extension must not mutate cache
  }
  expect_equal(awe_inflator("2025-05-15", "2026-05-15", check = 2L), 1579.2 / 1542.3)
  expect_equal(awote_inflator("2025-05-15", "2026-05-15", check = 2L), 2083.7 / 2010)
})

# Deliberately unequal changes expose accidental calendar-half or quarterly
# bucketing. Exercise all native paths and scalar broadcasting directions.
index <- data.table(date = as.IDate(c("2023-05-15", "2023-11-15", "2024-05-15",
                                     "2024-11-15", "2025-05-15")),
                    value = c(100, 110, 132, 165, 198))
dates <- as.IDate(c("2023-05-15", "2023-10-31", "2023-11-01", "2024-01-01",
                    "2024-04-30", "2024-05-01", "2024-10-31", "2024-11-01",
                    "2025-04-30", "2025-05-15"))
values <- c(100, 100, 110, 110, 110, 132, 132, 165, 165, 198)
for (represent in list(identity, as.Date, as.character)) {
  for (threads in c(1L, 4L)) {
    from <- represent(dates)
    first <- represent(index$date[1L])
    expect_equal(Inflate(first, from, index, nThread = threads), values / 100)
    expect_equal(Inflate(from, first, index, nThread = threads), 100 / values)
    expect_equal(Inflate(from, rev(from), index, nThread = threads), rev(values) / values)
    x <- rep(10, length(from))
    expect_equal(awe_inflator(first, from, series = index, x = x, nThread = threads), values / 10)
    expect_equal(x, values / 10)
    expect_true(is.nan(Inflate(represent(as.IDate("2023-04-30")), first, index, check = 0L)))
    expect_true(is.nan(Inflate(represent(as.IDate(NA_character_)), first, index)))
    expect_equal(Inflate(represent(as.IDate(character())), represent(as.IDate(character())), index), numeric())
  }
}
expect_equal(Inflate(2024L, 2025L, index), 165 / 110)
for (mo in 1:12) {
  # FY 2022-23 -> 2023-24 would precede this fixture for some months; use
  # a longer index and independent month matching for all twelve FY choices.
  full <- data.table(date = seq(as.IDate("2021-11-15"), as.IDate("2025-11-15"), by = "6 months"),
                     value = c(90, 100, 105, 110, 120, 140, 150, 180, 200))
  fy_date <- function(end) as.IDate(sprintf("%d-%02d-01", end - (mo >= 7L), mo))
  ym <- function(d) 12L * year(d) + month(d)
  expected <- full$value[findInterval(ym(fy_date(2024)), ym(full$date))] /
    full$value[findInterval(ym(fy_date(2023)), ym(full$date))]
  expect_equal(Inflate("2022-23", "2023-24", full, fy_month = mo), expected)
  expect_equal(Inflate(fy::yr2fy(2023L), fy::yr2fy(2024L), full, fy_month = mo), expected)
}
expect_equal(Inflate("2023-05-15", "2024-05-15", index, x = 1:3), (1:3) * 1.32)
expect_error(Inflate(dates, dates, index, x = c(1, 2)), "length")
expect_error(Inflate("2023-05-14", "2024-05-15", index), "earlier")
expect_error(Inflate("2023-05-15", "2025-05-16", index), "later")
expect_warning(Inflate("2023-05-15", "2025-10-31", index, check = 1L), "carried forward")
expect_equal(suppressWarnings(Inflate("2023-05-15", "2025-10-31", index, check = 1L)), 1.98)
expect_error(grattanInflators:::validate_index(index[-3L]), "regular sequence")

# Forecast and custom extension must advance in six-month steps and compound
# annual rates over two periods, including when the anchor is November.
for (anchor in c("2020-05-15", "2020-11-15")) {
  half <- data.table(date = seq(as.IDate(anchor), by = "6 months", length.out = 7L),
                     value = 100 * 1.1^(0:6))
  end <- last(half$date)
  expect_equal(grattanInflators:::.forecast_horizon(half$date, end + 1L), 0L)
  expect_equal(grattanInflators:::.forecast_horizon(half$date, grattanInflators:::.add_months(end, 6L)), 1L)
  prolonged <- grattanInflators:::.prolong_Index(half, grattanInflators:::.add_months(end, 12L))
  expect_equal(prolonged$value[9L] / half$value[7L], 1.21)
  custom <- dr2index(half, 2030, 0.21)
  expect_equal(custom$value[9L] / half$value[7L], 1.21)
  expect_equal(unique(diff(12L * year(custom$date) + month(custom$date))), 6L)
  forecast <- suppressMessages(grattanInflators:::.prolong_ets(half, until = grattanInflators:::.add_months(end, 6L)))
  expect_true(nrow(forecast) >= 8L)
  expect_equal(forecast$date[8L], grattanInflators:::.add_months(end, 6L))
  expect_true(all(is.finite(forecast$value)))
}
