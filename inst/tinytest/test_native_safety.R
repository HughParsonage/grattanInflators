# Every assertion here runs without any ABS data, and every one exercises a
# native entry point with an input that used to read or write out of bounds.

library(data.table)
library(grattanInflators)
ii <- grattanInflators:::Inflate

IndexA <- data.table(date = as.IDate(paste0(2000:2020, "-01-01")),
                     value = 1.01^(0:20))
IndexQ <- data.table(date = seq(as.IDate("2000-03-01"), by = "3 months", length.out = 84),
                     value = 1.005^(0:83))
IndexM <- data.table(date = seq(as.IDate("2000-01-01"), by = "1 month", length.out = 252),
                     value = 1.002^(0:251))

# --- `x` must have the output length ---------------------------------------
# Previously a short `x` with a longer `from` wrote past the end of `x`.
fr <- as.IDate("2005-01-01") + 0:9
expect_error(ii(fr, as.IDate("2010-01-01"), index = IndexA, x = c(1, 2, 3)),
             "length")
expect_error(ii(fr, as.IDate("2010-01-01"), index = IndexA, x = c(1, 2, 3), check = 0L),
             "length")
# and the same via the character path
expect_error(ii(as.character(fr), "2010-01-01", index = IndexA, x = c(1, 2, 3)),
             "length")

# `x` must be numeric
expect_error(ii(fr, as.IDate("2010-01-01"), index = IndexA, x = as.character(1:10)),
             "numeric")

# an integer `x` is accepted, but only via the return value
xi <- 1:10
expect_equal(ii(fr, as.IDate("2010-01-01"), index = IndexA, x = xi),
             as.double(1:10) * ii(fr, as.IDate("2010-01-01"), index = IndexA))
expect_equal(xi, 1:10)   # unchanged: coercion copies

# a length-one from/to still broadcasts over any length of x
xd <- c(1, 2, 3)
ii("2015-01-01", "2016-01-01", index = IndexA, x = xd)
expect_equal(xd, c(1.01, 2.02, 3.03))

# --- out-of-range dates give NaN, never an out-of-bounds read ---------------
# check = 0 disables reporting, but never disables the bounds check.
for (Index in list(IndexA, IndexQ, IndexM)) {
  expect_true(is.nan(ii("1949-01-01", "2010-01-01", index = Index, check = 0L)))
  expect_true(is.nan(ii("2074-01-01", "2010-01-01", index = Index, check = 0L)))
  expect_true(is.nan(ii("2010-01-01", "1949-01-01", index = Index, check = 0L)))
  expect_true(is.nan(ii("2010-01-01", "2074-01-01", index = Index, check = 0L)))
  expect_true(is.nan(ii(as.IDate("1949-01-01"), as.IDate("2010-01-01"),
                        index = Index, check = 0L)))
  expect_true(is.nan(ii(as.IDate("2074-01-01"), as.IDate("2010-01-01"),
                        index = Index, check = 0L)))
  # vectors of out-of-range dates, both scalar-`to` and vector-`to` branches
  z <- as.IDate(c("1949-01-01", "2010-01-01", "2074-01-01"))
  expect_equal(is.nan(ii(z, as.IDate("2010-01-01"), index = Index, check = 0L)),
               c(TRUE, FALSE, TRUE))
  expect_equal(is.nan(ii(z, z, index = Index, check = 0L)),
               c(TRUE, FALSE, TRUE))
  expect_equal(is.nan(ii(as.IDate("2010-01-01"), z, index = Index, check = 0L)),
               c(TRUE, FALSE, TRUE))
}

# --- zero-length inputs -----------------------------------------------------
Year <- grattanInflators:::Year
YearMonthSplit <- grattanInflators:::YearMonthSplit
expect_equal(Year(as.IDate(character(0))), integer(0))
expect_equal(YearMonthSplit(as.IDate(character(0))), list(integer(0), integer(0)))
expect_equal(YearMonthSplit(character(0)), list(integer(0), integer(0)))
expect_equal(grattanInflators:::format_idate(integer(0)), character(0))
expect_equal(fast_as_idate(character(0)), as.IDate(character(0)))
expect_equal(fast_as_idate(character(0), check = 2L), as.IDate(character(0)))
expect_equal(ii(as.IDate(character(0)), as.IDate(character(0)), index = IndexA),
             double(0))
expect_equal(ii(character(0), character(0), index = IndexA), double(0))

# --- malformed strings ------------------------------------------------------
# A month of zero used to index one before the start of the month table.
expect_true(is.na(fast_as_idate("01/00/2024", format = "%d/%m/%Y")))
expect_true(is.na(fast_as_idate("01XYZ1948", format = "%d%b%Y")))
expect_true(is.na(fast_as_idate("1948-00-01")))
expect_true(is.na(fast_as_idate("aa/01/2024", format = "%d/%m/%Y")))
expect_true(is.na(fast_as_idate("01/01/aaaa", format = "%d/%m/%Y")))
expect_true(is.na(fast_as_idate("aaJan1970", format = "%d%b%Y")))

# a systematic sweep of short and mangled strings: every one must give NA or
# an error, and none may crash
set.seed(1)
alphabet <- c(0:9, "-", "/", "A", "a", "Z", " ", "%")
junk <- vapply(1:2000,
               function(i) paste0(sample(alphabet, sample(0:12, 1), replace = TRUE),
                                  collapse = ""),
               "")
for (fmt in c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%d%b%Y")) {
  o <- fast_as_idate(junk, format = fmt, check = 0L)
  expect_true(all(is.na(o) | (o >= as.IDate("1948-01-01") & o <= as.IDate("2075-12-31"))))
}
expect_true(is.double(ii(junk, "2010-01-01", index = IndexA, check = 0L)))
expect_equal(length(ii(junk, "2010-01-01", index = IndexA, check = 0L)), length(junk))

# NA strings are NA, not an error
expect_true(is.na(fast_as_idate(NA_character_)))
expect_true(is.na(fast_as_idate(NA_character_, format = "%d/%m/%Y")))
expect_true(is.na(fast_as_idate(NA_character_, format = "%d%b%Y")))
