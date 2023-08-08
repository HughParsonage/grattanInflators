library(tinytest)
library(grattanInflators)
library(data.table)


expect_equal(round(cpi_inflator("2015-16", "2016-17"), 3), 1.021, tolerance = 0.001, scale = 1)
expect_equal(round(cpi_inflator("2015-16", "2014-15"), 3), 0.987, tolerance = 0.001, scale = 1)

# Note sometimes CPI is negative, so we go for 3 year intervals
expect_true(min(cpi_inflator(fy::yr2fy(1949:2018), fy::yr2fy(1952:2021), series = "original")) >= 1)

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


expect_error(cpi_inflator("1986-11-01", "1997-01-01", series = "seasonal"), "earliest")
expect_error(cpi_inflator("1949-11-01", "1997-01-01", series = "seasonal"), "earliest")
expect_error(cpi_inflator("1886-11-01", "1997-01-01", series = "seasonal"), "Year")
expect_error(cpi_inflator(c("1986-12-01", "1986-11-01"),
                          "1997-01-01",
                          series = "seasonal"),
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
expect_equal(cpi_inflator("2010-11", "2011-12", series = "trimmed.mean"),
             1.02,
             scale = 1, tolerance = 0.005)
expect_equal(cpi_inflator(2015, 2016), cpi_inflator("2015-01-01", "2016-01-01"))

from_i <- as.IDate(from_c <- c("2015-01-01", "2014-02-02", NA, "2011-12-01"))
to_i <- as.IDate(to_c <- c("2015-02-02"))
expect_equal(cpi_inflator(from_i, to_i), cpi_inflator(from_c, to_c))

# Approximately every supported date
all_dates <- grattanInflators:::all_dates
expect_true(min(cpi_inflator("1948-09-01", all_dates()[400:27000], series = "original")) > 0.99)

ff <- "2015-15"
expect_error(cpi_inflator(ff, "2015-16"), "ff|from")
ff <- "1999-00"
expect_true(is.double(cpi_inflator(ff, "2015-16")))
ff <- "1999-01"
expect_error(cpi_inflator(ff, "2015-16"), "ff|from")
ff <- c("1999-00", "2019-20", "2019-19")
expect_error(cpi_inflator(ff, "2015-16"), "ff|from")
ff <- c("1999-00-01")
expect_error(cpi_inflator(ff, "2015-16"), "ff|from")

expect_equal(cpi_inflator(c("2015-16", "2015-16"), NA_character_), c(NaN, NaN))
expect_equal(cpi_inflator(c("2015-16", "2015-16"), from = NA_character_), c(NaN, NaN))

flp <- c("2000-02-29", "2004-02-29", "1984-02-29", "1983-02-29")
expect_error(cpi_inflator(flp, "2000-01-01", check = 2L), "f(lp|rom).4.")

# invalid mday
fmmdd <- c("2000-02-29", "2000-11-31")
expect_error(cpi_inflator(fmmdd, "2000-01-01", check = 2L), "2000-11-31")

# invalid quartet
ffyy <- c("2015-16", NA, "2015-a7")
expect_error(cpi_inflator(ffyy, "2015-16"), "fy")

expect_equal(cpi_inflator("2019-Q1", "2020-Q1"),
             cpi_inflator("2019-01-01", "2020-01-01"))
expect_error(cpi_inflator("2019-Q1", "2020-Q5"))

if (Sys.Date() < as.Date("2025-01-01")) {
  expect_equal(cpi_inflator("2026-01-01", "2027-01-01", series = cpi_seasonal(2026, 0.05, 2027, 0.05)), 1.05)
  expect_equal(cpi_inflator("2026-01-01", "2027-01-01", series = cpi_seasonal("2025-26", 0.05, "2026-27", 0.05)), 1.05)
  expect_equal(cpi_inflator("2025-01-01", "2026-01-01", series = cpi_original(2025, 0.1)), 1.1)
}



