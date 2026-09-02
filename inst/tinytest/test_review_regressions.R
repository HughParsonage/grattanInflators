library(data.table)
library(grattanInflators)

ii <- grattanInflators:::Inflate

# OpenMP min/max reductions must retain maxima regardless of vector order,
# leading missing values, or the number of worker threads.
min_date <- as.IDate("2000-01-01")
max_date <- as.IDate("2020-01-01")
valid <- as.IDate("2010-01-01")
middle <- as.IDate("2015-01-01")
future <- as.IDate("2021-01-01")
future_cases <- list(
  c(valid, future),
  c(valid, future, middle),
  c(as.IDate(NA), future),
  c(future, valid)
)
for (nThread in c(1L, 4L)) {
  for (z in future_cases) {
    expect_true(grattanInflators:::.check_input(
      z, min_date, max_date, check = 1L, nThread = nThread,
      fy_month = 3L, var = "z", xclass = grattanInflators:::CLASS_IDate
    ))
    expect_error(grattanInflators:::.check_input(
      z, min_date, max_date, check = 2L, nThread = nThread,
      fy_month = 3L, var = "z", xclass = grattanInflators:::CLASS_IDate
    ), "later")
  }
  expect_false(grattanInflators:::.check_input(
    rep(as.IDate(NA), 3L), min_date, max_date, check = 1L,
    nThread = nThread, fy_month = 3L, var = "z",
    xclass = grattanInflators:::CLASS_IDate
  ))
  expect_false(grattanInflators:::.check_input(
    as.IDate(character()), min_date, max_date, check = 1L,
    nThread = nThread, fy_month = 3L, var = "z",
    xclass = grattanInflators:::CLASS_IDate
  ))
}

# C_Inflate2 must reject raw invalid dates before its clamping month lookup.
full_index <- data.table(
  date = seq(as.IDate("1948-01-01"), as.IDate("2075-12-01"), by = "1 month"),
  value = as.double(seq_len(1536L))
)
for (constructor in list(as.IDate, as.Date)) {
  lower <- constructor("1947-12-31")
  upper <- constructor("2076-01-01")
  first <- constructor("1948-01-31")
  last <- constructor("2075-12-01")
  missing <- constructor(NA_character_)
  for (check in 0:2) {
    expect_true(is.nan(ii(missing, first, index = full_index, check = check)))
    expect_equal(is.nan(ii(c(first, missing), first,
                           index = full_index, check = check)), c(FALSE, TRUE))
    expect_true(is.finite(ii(first, last, index = full_index, check = check)))
    if (check == 0L) {
      expect_true(is.nan(ii(lower, first, index = full_index, check = check)))
      expect_true(is.nan(ii(upper, last, index = full_index, check = check)))
    } else {
      expect_error(ii(lower, first, index = full_index, check = check),
                   "earlier|supported")
      expect_error(ii(upper, last, index = full_index, check = check),
                   "later|supported")
    }
  }
}

# Fast Date/IDate and generic character paths use the same annual anchor.
june_index <- data.table(
  date = as.IDate(c("2014-06-01", "2015-06-01", "2016-06-01")),
  value = c(100, 110, 121)
)
from_idate <- as.IDate(c("2015-05-01", "2015-06-01", "2015-07-01"))
to_idate <- as.IDate("2015-06-01")
expected <- c(1.1, 1, 1)
expect_equal(ii(from_idate, to_idate, index = june_index), expected)
expect_equal(ii(as.Date(from_idate), as.Date(to_idate), index = june_index), expected)
expect_equal(ii(as.character(from_idate), as.character(to_idate), index = june_index),
             expected)
expect_equal(ii(2015L, 2016L, index = june_index),
             ii("2015-01-01", "2016-01-01", index = june_index))

# Quarterly Date/IDate inputs retain the generic kernel's calendar-quarter
# interpretation, including dates before the observation month in a quarter.
quarter_index <- data.table(
  date = seq(as.IDate("2014-03-01"), by = "3 months", length.out = 12L),
  value = as.double(seq_len(12L))
)
quarter_dates <- as.IDate(c("2015-01-01", "2015-02-01", "2015-03-01",
                            "2015-04-01"))
expect_equal(ii(quarter_dates, as.IDate("2015-06-01"), index = quarter_index),
             ii(as.character(quarter_dates), "2015-06-01", index = quarter_index))

# Integer custom values are normalized locally rather than rejected in C or
# changed by reference in the caller's data.table.
integer_index <- data.table(
  date = as.IDate(c("2020-01-01", "2021-01-01")),
  value = c(100L, 110L)
)
expect_equal(ii(as.IDate("2020-01-01"), as.IDate("2021-01-01"),
                index = integer_index), 1.1)
expect_identical(typeof(integer_index$value), "integer")

# fy_month has one contract, independent of how a financial year is supplied.
fy_value <- fy::yr2fy(2016L)
for (bad_month in list(0L, 13L, NA_integer_, c(3L, 4L), 3.5)) {
  expect_error(ii(fy_value, fy_value, index = june_index, fy_month = bad_month),
               "one integer from 1 to 12")
  expect_error(ii("2015-16", "2015-16", index = june_index,
                  fy_month = bad_month), "one integer from 1 to 12")
}

# Download parsing ignores irrelevant columns but never deletes a malformed
# required observation before structural validation.
read_tsv <- grattanInflators:::read_series_tsv
fixture <- function(lines) {
  path <- tempfile(fileext = ".tsv")
  writeLines(lines, path)
  path
}
valid_extra <- fixture(c(
  "date\tvalue\textra",
  "2020-01-01\t100\t",
  "2021-01-01\t110\tok"
))
expect_equal(nrow(read_tsv(valid_extra)), 2L)

for (lines in list(
  c("date\tvalue", "2020-01-01\t", "2021-01-01\t110"),
  c("date\tvalue", "2020-01-01\t100", "2021-01-01\t"),
  c("date\tvalue", "2020-01-01\t100", "2020-02-01\t", "2020-03-01\t102"),
  c("date\tvalue", "not-a-date\t100", "2021-01-01\t110")
)) {
  expect_error(read_tsv(fixture(lines)), "missing or unparseable")
}
# Legacy caches may contain unavailable boundary periods, but a missing
# interior observation is corruption even in compatibility mode.
legacy_boundary <- fixture(c(
  "date\tvalue", "2019-01-01\t", "2020-01-01\t100", "2021-01-01\t"
))
expect_equal(nrow(read_tsv(legacy_boundary, strict = FALSE)), 1L)
expect_error(read_tsv(fixture(c(
  "date\tvalue", "2019-01-01\t100", "2020-01-01\t", "2021-01-01\t110"
)), strict = FALSE), "missing or unparseable")
# Syntactically plausible dates must not be normalized into another month.
expect_error(read_tsv(fixture(c(
  "date\tvalue", "2021-02-29\t100", "2022-02-29\t110"
))), "missing or unparseable")

# The same boundary-trimming mode used for newly downloaded ABS files yields
# a regular index while retaining every available observation.
download_scaffold <- fixture(c(
  "date\tvalue",
  "2019-12-01\t",
  "2020-03-01\t100",
  "2020-06-01\t101",
  "2020-09-01\t102",
  "2020-12-01\t"
))
trimmed_download <- read_tsv(download_scaffold, strict = FALSE)
expect_equal(trimmed_download$date,
             as.IDate(c("2020-03-01", "2020-06-01", "2020-09-01")))
expect_equal(length(grattanInflators:::validate_index(trimmed_download)), 3L)

# The horizon is endpoint- and frequency-dependent; an old monthly custom
# index can require more than the former fixed 700 observations.
old_monthly_dates <- seq(as.IDate("1999-01-01"), as.IDate("2000-12-01"),
                         by = "1 month")
expect_equal(grattanInflators:::.forecast_horizon(
  old_monthly_dates, as.IDate("2070-12-01")
), 840L)
expect_equal(grattanInflators:::.requested_until(
  c("2070-Q4", "2070-71"), grattanInflators:::CLASS_character, 3L
), as.IDate("2071-03-01"))
