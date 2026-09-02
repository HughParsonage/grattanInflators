# Financial years and quarters must mean the same thing to the checker and to
# the converter, and an <fy> object must mean the same thing as the equivalent
# string. A monthly index is used throughout, because a quarterly index hides
# a one- or two-month disagreement.

library(data.table)
library(grattanInflators)
ii <- grattanInflators:::Inflate
YMS <- grattanInflators:::YearMonthSplit

IndexM <- data.table(date = seq(as.IDate("1990-01-01"), by = "1 month", length.out = 400),
                     value = 1.002^(0:399))
IndexQ <- data.table(date = seq(as.IDate("1990-03-01"), by = "3 months", length.out = 134),
                     value = 1.005^(0:133))

# --- an <fy> object means what the equivalent string means ------------------
for (fym in 1:12) {
  # Jul-Dec falls in the first calendar year of the label, Jan-Jun the second
  expected <- as.IDate(sprintf("%04d-%02d-01", if (fym >= 7L) 2015L else 2016L, fym))
  expect_equal(ii(fy::yr2fy(2016L), "2010-01-01", index = IndexM, fy_month = fym),
               ii(expected, as.IDate("2010-01-01"), index = IndexM),
               info = paste("fy object, fy_month =", fym))
  expect_equal(ii("2015-16", "2010-01-01", index = IndexM, fy_month = fym),
               ii(expected, as.IDate("2010-01-01"), index = IndexM),
               info = paste("fy string, fy_month =", fym))
  # and the two representations agree with each other
  expect_equal(ii(fy::yr2fy(2016L), "2010-01-01", index = IndexM, fy_month = fym),
               ii("2015-16", "2010-01-01", index = IndexM, fy_month = fym),
               info = paste("fy object == fy string, fy_month =", fym))
  # in either argument position
  expect_equal(ii("2010-01-01", fy::yr2fy(2016L), index = IndexM, fy_month = fym),
               ii("2010-01-01", "2015-16", index = IndexM, fy_month = fym),
               info = paste("fy in `to`, fy_month =", fym))
}

# vectors of financial years
# (fy::fy2yr() rejects NA, so a missing financial year is out of scope here)
fyv <- fy::yr2fy(c(2000L, 2010L, 2016L))
expect_equal(ii(fyv, "2010-01-01", index = IndexM, fy_month = 9L),
             ii(c("1999-00", "2009-10", "2015-16"), "2010-01-01",
                index = IndexM, fy_month = 9L))

# the same holds on a quarterly index
for (fym in c(3L, 6L, 9L, 12L)) {
  expect_equal(ii(fy::yr2fy(2016L), "2010-01-01", index = IndexQ, fy_month = fym),
               ii("2015-16", "2010-01-01", index = IndexQ, fy_month = fym),
               info = paste("quarterly, fy_month =", fym))
}

# --- checker and converter must agree ---------------------------------------
# With check = 2 an in-range financial year must not error, and with check = 0
# it must give the same answer as with check = 2.
for (fym in 1:12) {
  for (chk in 0:2) {
    expect_equal(ii("2015-16", "2010-01-01", index = IndexM, fy_month = fym, check = chk),
                 ii("2015-16", "2010-01-01", index = IndexM, fy_month = fym, check = 2L),
                 info = paste("fy_month =", fym, "check =", chk))
  }
}

# The last financial year of the series must be usable, and the one after it
# must be out of range, for a fy_month on either side of July.
last_date <- max(.subset2(IndexM, "date"))   # 2023-04-01
expect_true(is.double(ii("2022-23", "2010-01-01", index = IndexM, fy_month = 3L)))
expect_error(ii("2030-31", "2010-01-01", index = IndexM, fy_month = 3L), "later")
expect_error(ii("2030-31", "2010-01-01", index = IndexM, fy_month = 9L), "later")

# On an annual series not anchored in January, financial years must stay on
# the anchor-aware generic path even when both arguments are scalar <fy>s.
IndexA_Jun <- data.table(date = as.IDate(sprintf("%d-06-01", 2010:2020)),
                         value = (1:11)^2)
expect_equal(ii(fy::yr2fy(2016L), fy::yr2fy(2017L),
                index = IndexA_Jun, fy_month = 3L),
             ii("2015-16", "2016-17", index = IndexA_Jun, fy_month = 3L))
fy_x <- c(2, 3)
expect_equal(ii(fy::yr2fy(2016L), fy::yr2fy(2017L),
                index = IndexA_Jun, fy_month = 3L, x = fy_x),
             c(2, 3) * ii("2015-16", "2016-17",
                          index = IndexA_Jun, fy_month = 3L))

# Unsupported <fy> values are invalid inputs, not missing dates.
expect_error(ii(fy::yr2fy(2090L), fy::yr2fy(2017L),
                index = IndexA_Jun, fy_month = 3L, check = 1L),
             "supported years")
expect_error(ii(fy::yr2fy(2090L), fy::yr2fy(2017L),
                index = IndexA_Jun, fy_month = 3L, check = 2L),
             "supported years")
expect_true(is.nan(ii(fy::yr2fy(2090L), fy::yr2fy(2017L),
                      index = IndexA_Jun, fy_month = 3L, check = 0L)))

# --- quarters ---------------------------------------------------------------
# A quarter is the last month of that quarter, and the checker agrees.
for (q in 1:4) {
  expect_equal(ii(sprintf("2015-Q%d", q), "2010-01-01", index = IndexM),
               ii(sprintf("2015-%02d-01", 3L * q), "2010-01-01", index = IndexM),
               info = paste("quarter", q))
  expect_equal(ii(sprintf("2015-Q%d", q), "2010-01-01", index = IndexM, check = 0L),
               ii(sprintf("2015-Q%d", q), "2010-01-01", index = IndexM, check = 2L),
               info = paste("quarter", q, "check 0 == 2"))
}
expect_error(ii("2015-Q5", "2010-01-01", index = IndexM), "Q")
expect_error(ii("2015-Q0", "2010-01-01", index = IndexM), "Q")

# --- YearMonthSplit ---------------------------------------------------------
expect_equal(YMS("2001-02", fy_month = 3L), list(2002L, 3L))
expect_equal(YMS("2001-02", fy_month = 9L), list(2001L, 9L))
expect_equal(YMS("2001-Q1"), list(2001L, 3L))
expect_equal(YMS("2001-Q4"), list(2001L, 12L))
expect_equal(YMS(fy::yr2fy(2002L), fy_month = 3L), list(2002L, 3L))
expect_equal(YMS(fy::yr2fy(2002L), fy_month = 9L), list(2001L, 9L))

# --- the checker rejects the strings the converter cannot read ---------------
# "2020" is not a date the converter understands, so it must not pass the check.
expect_error(ii("2020", "2010-01-01", index = IndexM, check = 1L), "characters|YYYY")
expect_error(ii("", "2010-01-01", index = IndexM, check = 1L), "characters|YYYY")
expect_error(ii("2015-01-01x", "2010-01-01", index = IndexM, check = 1L), "characters")
# arbitrary bytes in the separator positions are not a date either
expect_error(ii("2015x01x01", "2010-01-01", index = IndexM, check = 1L), "separated")
# a year outside the supported range must be rejected, not aliased into range
expect_error(ii("2999-01-01", "2010-01-01", index = IndexM, check = 1L), "Years")
expect_error(ii("1848-01-01", "2010-01-01", index = IndexM, check = 1L), "Years")
expect_true(is.nan(ii("2999-01-01", "2010-01-01", index = IndexM, check = 0L)))
expect_true(is.nan(ii("1848-01-01", "2010-01-01", index = IndexM, check = 0L)))
# and with check = 0 the same strings give NaN rather than a plausible number
expect_true(is.nan(ii("2020", "2010-01-01", index = IndexM, check = 0L)))
