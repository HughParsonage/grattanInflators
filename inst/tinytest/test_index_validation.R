library(data.table)
library(grattanInflators)
ii <- grattanInflators:::Inflate
vi <- grattanInflators:::validate_index

good <- data.table(date = as.IDate(paste0(2000:2020, "-01-01")), value = 1.01^(0:20))
expect_equal(vi(good), as.IDate(paste0(2000:2020, "-01-01")))

# The native kernels compute a row offset from the first date and the
# frequency, so an index that is not a regular series must be rejected rather
# than silently read at the wrong offset.
expect_error(vi(as.data.frame(good)), "data.table")
expect_error(vi(data.table()), "zero rows")
expect_error(vi(data.table(a = 1, b = 2)), "columns")
expect_error(vi(good[1L]), "single observation")
expect_error(vi(data.table(date = 2000:2020, value = 1)), "IDate")
expect_error(vi(copy(good)[3L, "date" := NA]), "missing")
expect_error(vi(copy(good)[3L, "value" := NA_real_]), "non-finite")
expect_error(vi(copy(good)[3L, "value" := Inf]), "non-finite")
expect_error(vi(copy(good)[3L, "value" := 0]), "zero")
expect_error(vi(copy(good)[, "value" := as.character(value)]), "numeric")

# unsorted, duplicated and irregular date sequences
expect_error(vi(good[c(2, 1, 3:21)]), "strictly increasing")
expect_error(vi(good[c(1, 1, 2:21)]), "strictly increasing")
expect_error(vi(data.table(date = as.IDate(c("2000-01-01", "2000-01-15", "2000-01-31")),
                           value = c(1, 2, 3))),
             "same month")
expect_error(vi(good[-5L]), "regular sequence")
expect_error(vi(data.table(date = as.IDate(c("2000-01-01", "2000-03-01", "2000-08-01")),
                           value = c(1, 2, 3))),
             "annual, quarterly")
expect_error(vi(data.table(date = as.IDate(c("2000-01-01", "2000-04-01", "2000-09-01")),
                           value = c(1, 2, 3))),
             "not a regular sequence")
# 2-month steps are not a supported frequency
expect_error(vi(data.table(date = seq(as.IDate("2000-01-01"), by = "2 months", length.out = 10),
                           value = 1:10)),
             "annual, quarterly")
# annual but with a wandering month
expect_error(vi(data.table(date = as.IDate(c("2000-01-01", "2001-01-01", "2002-02-01")),
                           value = c(1, 2, 3))),
             "not a regular sequence")
# out of the supported date range
expect_error(vi(data.table(date = as.IDate(c("1900-01-01", "1901-01-01")), value = c(1, 2))),
             "supported dates")

# a zero-row index is now an error with a recognisable class, not a NULL
expect_error(ii("2015-01-01", "2016-01-01", index = data.table()), "zero rows")
e <- tryCatch(ii("2015-01-01", "2016-01-01", index = data.table()), error = function(e) e)
expect_true(inherits(e, "grattanInflators_empty_index"))

# quarterly and monthly indices are accepted
expect_equal(length(vi(data.table(date = seq(as.IDate("2000-03-01"), by = "3 months", length.out = 40),
                                  value = 1:40))),
             40L)
expect_equal(length(vi(data.table(date = seq(as.IDate("2000-01-01"), by = "1 month", length.out = 40),
                                  value = 1:40))),
             40L)
# a <Date> (not <IDate>) date column is accepted too
expect_equal(length(vi(data.table(date = seq(as.Date("2000-01-01"), by = "1 month", length.out = 40),
                                  value = 1:40))),
             40L)
