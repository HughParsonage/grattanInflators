

expect_equal(round(cpi_inflator("2015-16", "2016-17"), 3), 1.014, tolerance = 0.01)
expect_equal(round(cpi_inflator("2015-16", "2014-15"), 3), 0.983, tolerance = 0.01)
expect_true(min(cpi_inflator(fy::yr2fy(1949:2020), fy::yr2fy(1950:2021), adjustment = "original")) >= 1)
