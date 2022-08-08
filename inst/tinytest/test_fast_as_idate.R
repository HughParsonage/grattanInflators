library(data.table)
x <- CJ(y = 1970:2030, m = 1:12, d = 1:28)[, sprintf("%d-%02d-%02d", y, m, d)]
expect_equal(as.IDate(x), fast_as_idate(x, incl_day = TRUE))
expect_true(TRUE)
expect_error(fast_as_idate("ABCD-02-02", check = 1L), "YYYY")
expect_error(fast_as_idate("2000-AB-02", check = 1L), "[Mm]onth")
expect_error(fast_as_idate("2000-02", check = 1L), "fy")
expect_error(fast_as_idate("1886-01-01", check = 1L), "Years")
expect_equal(fast_as_idate(c("1985-01-01",
                             "1985-00-00", "foo", "1985-aa-01", "1885-01-01",
                             "2100-01-01", "3100-01-01"),
                           check = 0L),
             c(as.IDate("1985-01-01"), NA, NA, NA, NA, NA, NA))


