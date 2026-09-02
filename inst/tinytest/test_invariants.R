# The algebraic identities an inflator must satisfy, crossed over every
# supported input class and every index frequency. Synthetic indices, so this
# runs everywhere.

library(data.table)
library(grattanInflators)
ii <- grattanInflators:::Inflate

Indices <-
  list(annual = data.table(date = as.IDate(paste0(1990:2020, "-06-01")),
                           value = 1.02^(0:30)),
       quarterly = data.table(date = seq(as.IDate("1990-03-01"), by = "3 months", length.out = 124),
                              value = 1.005^(0:123)),
       monthly = data.table(date = seq(as.IDate("1990-01-01"), by = "1 month", length.out = 372),
                            value = 1.002^(0:371)))

t1 <- as.IDate(c("1995-06-01", "2001-06-01", "2010-06-01", "2015-06-01"))
t2 <- as.IDate(c("2000-06-01", "2005-06-01", "2012-06-01", "2019-06-01"))
t3 <- as.IDate(c("2003-06-01", "2018-06-01", "2016-06-01", "2020-06-01"))

for (nm in names(Indices)) {
  Index <- Indices[[nm]]
  info <- paste0("index = ", nm)

  reprs <- list(IDate = list(t1, t2, t3),
                Date = list(as.Date(t1), as.Date(t2), as.Date(t3)),
                character = list(as.character(t1), as.character(t2), as.character(t3)))

  for (rn in names(reprs)) {
    a <- reprs[[rn]][[1L]]
    b <- reprs[[rn]][[2L]]
    cc <- reprs[[rn]][[3L]]

    # Inflate(x, t, t) == x
    expect_equal(ii(a, a, index = Index), rep(1, length(t1)),
                 info = paste(info, rn, "identity"))
    # reciprocity
    expect_equal(ii(a, b, index = Index) * ii(b, a, index = Index),
                 rep(1, length(t1)),
                 info = paste(info, rn, "reciprocal"))
    # composition
    expect_equal(ii(a, b, index = Index) * ii(b, cc, index = Index),
                 ii(a, cc, index = Index),
                 info = paste(info, rn, "composition"))
    # scalar `to` agrees with a recycled vector `to`
    expect_equal(ii(a, b[1L], index = Index),
                 ii(a, rep(b[1L], length(t1)), index = Index),
                 info = paste(info, rn, "scalar to"))
    # scalar `from` agrees with a recycled vector `from`
    expect_equal(ii(a[1L], b, index = Index),
                 ii(rep(a[1L], length(t2)), b, index = Index),
                 info = paste(info, rn, "scalar from"))
    # x is multiplied by the factor
    xv <- as.double(seq_along(a))
    expect_equal(ii(a, b, index = Index, x = xv),
                 seq_along(a) * ii(a, b, index = Index),
                 info = paste(info, rn, "x"))
  }

  # the classes agree with each other
  expect_equal(ii(t1, t2, index = Index), ii(as.Date(t1), as.Date(t2), index = Index),
               info = paste(info, "IDate == Date"))
  expect_equal(ii(t1, t2, index = Index), ii(as.character(t1), as.character(t2), index = Index),
               info = paste(info, "IDate == character"))

  # NA in, NaN out (never an error, never a garbage number)
  expect_true(is.nan(ii(NA_character_, t2[1L], index = Index)), info = info)
  expect_true(is.nan(ii(as.IDate(NA), t2[1L], index = Index)), info = info)
  expect_true(is.nan(ii(t1[1L], NA_character_, index = Index)), info = info)
  expect_true(all(is.nan(ii(rep(NA_character_, 3), t2[1L], index = Index))), info = info)

  # recycling is prohibited, not silently applied
  expect_error(ii(t1, t2[1:2], index = Index), "length")

  for (chk in 0:2) {
    expect_equal(ii(t1, t2, index = Index, check = chk),
                 ii(t1, t2, index = Index),
                 info = paste(info, "check =", chk))
  }
}

# An integer year is January of that year; a financial year is fy_month of the
# appropriate calendar year (tested in detail in test_fy_semantics.R).
IndexM <- Indices$monthly
expect_equal(ii(2000L, 2010L, index = IndexM),
             ii("2000-01-01", "2010-01-01", index = IndexM))
expect_equal(ii(2000, 2010, index = IndexM),
             ii(2000L, 2010L, index = IndexM))

# A fractional year is a mistake, not a truncation
expect_error(ii(2000.5, 2010, index = IndexM), "whole number")
expect_equal(ii(2000.5, 2010, index = IndexM, check = 0L),
             ii(2000L, 2010L, index = IndexM))
