library(data.table)
x <- CJ(y = 1970:2030, m = 1:12, d = 1:28)[, sprintf("%d-%02d-%02d", y, m, d)]
expect_equal(as.IDate(x), fast_as_idate(x, incl_day = TRUE))
