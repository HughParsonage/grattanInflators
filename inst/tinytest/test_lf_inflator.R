library(grattanInflators)

expect_equal(round(lf_inflator("2015-16", "2016-17"), 3), 1.015, tolerance = 0.001, scale = 1)
expect_equal(round(lf_inflator("2015-16", "2014-15"), 3), 0.979, tolerance = 0.001, scale = 1)
x <- c("2015-01-01", "2015-16", "2014-01-01")
expect_equal(lf_inflator(x, "2016-17"), 1 / lf_inflator("2016-17", x))
expect_equal(lf_inflator(x, x), c(1, 1, 1))
y <- c(2, 3)
lf_inflator("2015-16", "2015-16", x = y)
expect_equal(y, c(2, 3))
from_i <- as.IDate(from_c <- c("2015-01-01", "2014-02-02", NA, "2011-12-01"))
to_i <- as.IDate(to_c <- c("2015-02-02"))
expect_equal(lf_inflator(from_i, to_i), lf_inflator(from_c, to_c))

expect_equal(lf_inflator(c("2015-16", "2015-16"), from = NA_character_), c(NaN, NaN))

