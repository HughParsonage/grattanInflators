library(grattanInflators)

expect_equal(round(lf_inflator("2015-16", "2016-17"), 3), 1.015, tolerance = 0.001, scale = 1)
expect_equal(round(lf_inflator("2015-16", "2014-15"), 3), 0.979, tolerance = 0.001, scale = 1)
