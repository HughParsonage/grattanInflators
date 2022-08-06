library(grattanInflators)

expect_equal(round(wage_inflator("2015-16", "2016-17"), 3), 1.019, tolerance = 0.001, scale = 1)
expect_equal(round(wage_inflator("2015-16", "2014-15"), 3), 0.980, tolerance = 0.001, scale = 1)

