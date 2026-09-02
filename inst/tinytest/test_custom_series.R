# Direct coverage of the custom-series constructors and both extrapolation
# paths, against synthetic indices so that this runs without any ABS data.

library(data.table)
library(grattanInflators)

nd <- grattanInflators:::.next_date
r2r <- grattanInflators:::.rate2rate
prolong <- grattanInflators:::.prolong_Index
prolong_r <- grattanInflators:::.prolong_annual_r
r2index <- grattanInflators:::r2index

IndexA <- data.table(date = as.IDate(paste0(2000:2020, "-01-01")), value = 1.01^(0:20))
IndexQ <- data.table(date = seq(as.IDate("2000-03-01"), by = "3 months", length.out = 84),
                     value = 1.005^(0:83))
IndexM <- data.table(date = seq(as.IDate("2000-01-01"), by = "1 month", length.out = 252),
                     value = 1.002^(0:251))

# --- .next_date -------------------------------------------------------------
expect_equal(nd(as.IDate(c("2019-01-01", "2020-01-01"))), as.IDate("2021-01-01"))
expect_equal(nd(as.IDate(c("2024-06-01", "2024-09-01"))), as.IDate("2024-12-01"))
# a quarter anchored in October rolls into January, not month 13
expect_equal(nd(as.IDate(c("2024-07-01", "2024-10-01"))), as.IDate("2025-01-01"))
expect_equal(nd(as.IDate(c("2024-08-01", "2024-11-01"))), as.IDate("2025-02-01"))
expect_equal(nd(as.IDate(c("2024-11-01", "2024-12-01"))), as.IDate("2025-01-01"))
# the day of the month is clamped rather than made invalid
expect_equal(nd(as.IDate(c("2024-11-30", "2024-12-31"))), as.IDate("2025-01-31"))
expect_equal(nd(as.IDate(c("2022-12-31", "2023-01-31"))), as.IDate("2023-02-28"))
expect_equal(nd(as.IDate(c("2023-12-31", "2024-01-31"))), as.IDate("2024-02-29"))

# --- .rate2rate -------------------------------------------------------------
expect_equal(r2r(0.05), 0.05)
expect_equal(r2r("5%"), 0.05)
expect_equal(r2r(" 5% "), 0.05)
expect_equal(r2r("0.05"), 0.05)
# a minus sign must survive: gsub("[^0-9.]", "", .) used to delete it
expect_equal(r2r("-5%"), -0.05)
expect_equal(r2r("-0.05"), -0.05)
expect_equal(r2r("+5%"), 0.05)
expect_equal(r2r("2.5%"), 0.025)
# exponents must survive too
expect_equal(r2r("1e-2"), 0.01)
expect_equal(r2r("-1.5e-2"), -0.015)
expect_error(r2r("abc"), "no digits|not a valid rate")
expect_error(r2r("5%%"), "not a valid rate")
expect_error(r2r("5 per cent"), "not a valid rate")
expect_error(r2r(NA_character_), "NA|finite")
expect_error(r2r(NA_real_), "finite")
expect_error(r2r(Inf), "finite")
expect_error(r2r(c(1, 2)), "length")
expect_error(r2r(TRUE), "must be character or numeric")
# a rate of -100% or worse cannot be compounded
expect_error(r2r(-1), "cannot be compounded")
expect_error(r2r("-150%"), "cannot be compounded")
expect_message(r2r(0.9), "unlikely")

# --- dr2index ---------------------------------------------------------------
# No period may be skipped, and none may be added beyond the requested end.
for (Index in list(IndexA, IndexQ, IndexM)) {
  freq <- grattanInflators:::date2freq(.subset2(Index, "date"))
  ext <- dr2index(Index, 2025, 0)
  d <- .subset2(ext, "date")
  # a regular series throughout
  expect_equal(unique(diff(12L * year(d) + month(d))), 12L %/% freq)
  # every period of 2025 is present exactly once
  expect_equal(sum(year(d) == 2025L), freq)
  # r = 0 means the value is flat from the end of the published series
  v <- .subset2(ext, "value")
  expect_equal(v[year(d) >= 2025L], rep(last(.subset2(Index, "value")), sum(year(d) >= 2025L)))
  # extended to the end of the supported range, and no further
  expect_true(max(d) <= as.IDate("2075-12-31"))
  expect_true(max(d) >= as.IDate("2075-01-01"))
}

# The rate is annual: after one whole year the index has grown by exactly r.
ext <- dr2index(IndexM, 2030, 0.1)
v <- .subset2(ext, "value")
d <- .subset2(ext, "date")
expect_equal(v[d == as.IDate("2030-12-01")] / v[d == as.IDate("2029-12-01")], 1.1,
             tolerance = 1e-10)
# a percentage string gives the same answer as the number
expect_equal(.subset2(dr2index(IndexM, 2030, "10%"), "value"), v)

# Several date-rate pairs
ext2 <- dr2index(IndexM, 2030, 0.1, 2035, 0)
v2 <- .subset2(ext2, "value")
d2 <- .subset2(ext2, "date")
expect_equal(v2[d2 == as.IDate("2030-12-01")] / v2[d2 == as.IDate("2029-12-01")], 1.1,
             tolerance = 1e-10)
expect_equal(v2[d2 == as.IDate("2034-12-01")], v2[d2 == as.IDate("2031-12-01")],
             tolerance = 1e-10)

expect_error(dr2index(IndexM, 2030), "r1 is missing")
expect_equal(dr2index(IndexM), IndexM)

# --- r2index ----------------------------------------------------------------
# `next_date` used to be referenced before it was assigned here.
ri <- r2index(IndexM, 2030, 0.1, r = 0.03)
dr <- .subset2(ri, "date")
vr <- .subset2(ri, "value")
expect_equal(unique(diff(12L * year(dr) + month(dr))), 1L)
expect_equal(vr[dr == as.IDate("2030-12-01")] / vr[dr == as.IDate("2029-12-01")], 1.1,
             tolerance = 1e-10)
expect_equal(vr[dr == as.IDate("2040-12-01")] / vr[dr == as.IDate("2039-12-01")], 1.03,
             tolerance = 1e-10)
expect_equal(r2index(IndexM), IndexM)

# a trailing rate alone, dispatched through the same path the inflators use
cs <- grattanInflators:::.custom_series(IndexM, 0.04)
dc <- .subset2(cs, "date")
vc <- .subset2(cs, "value")
expect_equal(vc[dc == as.IDate("2040-12-01")] / vc[dc == as.IDate("2039-12-01")], 1.04,
             tolerance = 1e-10)
expect_equal(grattanInflators:::.custom_series(IndexM), IndexM)
# date-rate pairs plus a trailing rate
cs2 <- grattanInflators:::.custom_series(IndexM, 2030, 0.1, 0.03)
expect_equal(.subset2(cs2, "value"), vr)

# --- .prolong_Index (the fable-free fallback) -------------------------------
# The annual branch built one more date than value; the "monthly" branch was
# quarterly arithmetic on quarterly dates.
for (Index in list(IndexA, IndexQ, IndexM)) {
  freq <- grattanInflators:::date2freq(.subset2(Index, "date"))
  p <- prolong(Index, as.IDate("2030-06-01"))
  d <- .subset2(p, "date")
  v <- .subset2(p, "value")
  expect_equal(length(d), length(v))
  expect_false(anyDuplicated(d) > 0L)
  expect_equal(unique(diff(12L * year(d) + month(d))), 12L %/% freq)
  expect_true(max(d) >= as.IDate("2030-06-01"))
  expect_true(all(is.finite(v)))
  # the appended values continue the observed year-on-year rate
  n0 <- nrow(Index)
  r_obs <- last(.subset2(Index, "value")) / .subset2(Index, "value")[n0 - freq]
  expect_equal(v[n0 + freq] / v[n0], r_obs, tolerance = 1e-10)
  # and the result is a valid index
  expect_equal(length(grattanInflators:::validate_index(p)), length(d))
}
# a quarterly series needs five observations to imply a year-on-year rate
expect_error(prolong(IndexQ[1:4], as.IDate("2030-06-01")), "too few")
expect_error(prolong(IndexM[1:12], as.IDate("2030-06-01")), "too few")

# --- .prolong_annual_r ------------------------------------------------------
for (Index in list(IndexA, IndexQ, IndexM)) {
  p <- prolong_r(Index, 0.02)
  expect_equal(length(grattanInflators:::validate_index(p)), nrow(p))
  expect_true(max(.subset2(p, "date")) <= as.IDate("2075-12-31"))
}
expect_error(prolong_r(IndexM, "not a rate"), "not a valid rate")
