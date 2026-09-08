library(grattanInflators)
library(data.table)
setDTthreads(1)
library(tinytest)

expect_equal(round(wage_inflator("2015-16", "2016-17"), 3), 1.019, tolerance = 0.001, scale = 1)
expect_equal(round(wage_inflator("2015-16", "2014-15"), 3), 0.980, tolerance = 0.001, scale = 1)

wpi_original_dt <-  grattanInflators::wpi_original(FORECAST = TRUE)
expect_true(max(wpi_original_dt$date) <= as.IDate("2075-12-31"))
