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
