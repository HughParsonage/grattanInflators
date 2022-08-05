library(tinytest)
library(grattanInflators)
library(data.table)


expect_equal(round(cpi_inflator("2015-16", "2016-17"), 3), 1.021, tolerance = 0.001, scale = 1)
expect_equal(round(cpi_inflator("2015-16", "2014-15"), 3), 0.987, tolerance = 0.001, scale = 1)

# Note sometimes CPI is negative, so we go for 3 year intervals
expect_true(min(cpi_inflator(fy::yr2fy(1949:2018), fy::yr2fy(1952:2021), adjustment = "original")) >= 1)

expect_equal(cpi_inflator("2010-06-01", "2015-06-01"),
             cpi_inflator("2010-06-01", "2014-01-02") * cpi_inflator("2014-01-02", "2015-06-01"))

xd <- as.IDate("2005-05-05") + 1:1000
yd <- as.IDate("2004-03-03") + 5 * (1:1000)
expect_equal(cpi_inflator(xd, yd), cpi_inflator(as.character(xd), yd), tolerance = 0.001, scale = 1)
expect_equal(cpi_inflator(xd, yd), cpi_inflator(xd, as.character(yd)), tolerance = 0.001, scale = 1)
expect_equal(cpi_inflator(xd, "2015-16"),
             cpi_inflator(xd, fy::yr2fy(2016L)),
             tolerance = 0.001, scale = 1)

expect_equal(cpi_inflator("2012-13", "2013-14"), 1.0295, tolerance = 0.001, scale = 1)


expect_error(cpi_inflator("1986-11-01", "1997-01-01", adjustment = "seasonal"), "earliest")
expect_error(cpi_inflator("1949-11-01", "1997-01-01", adjustment = "seasonal"), "earliest")
expect_error(cpi_inflator("1886-11-01", "1997-01-01", adjustment = "seasonal"), "Year")
expect_error(cpi_inflator(c("1986-12-01", "1986-11-01"),
                          "1997-01-01",
                          adjustment = "seasonal"),
             "from[2]",
             fixed = TRUE)

x <- as.double(1:5)
y <- x * cpi_inflator("2015-16", "2016-17")
cpi_inflator("2015-16", "2016-17", x = x)
expect_equal(x, y)

expect_equal(cpi_inflator("2015-16", c("2015-16", "2015-16")), c(1, 1))
expect_equal(cpi_inflator(2015, 2016), 1 / cpi_inflator(2016, 2015))
expect_equal(cpi_inflator(2015L, 2016), 1 / cpi_inflator(2016, 2015L))
expect_error(cpi_inflator(2099L, 2016))
expect_equal(cpi_inflator("2010-11", "2011-12", adjustment = "trimmed.mean"),
             1.02,
             scale = 1, tolerance = 0.005)
expect_equal(cpi_inflator(2015, 2016), cpi_inflator("2015-01-01", "2016-01-01"))

from_i <- as.IDate(from_c <- c("2015-01-01", "2014-02-02", NA, "2011-12-01"))
to_i <- as.IDate(to_c <- c("2015-02-02"))
expect_equal(cpi_inflator(from_i, to_i), cpi_inflator(from_c, to_c))



