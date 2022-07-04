library(tinytest)
library(data.table)
library(grattanInflators)

YMS <- grattanInflators:::YearMonthSplit
expect_equal(YMS(as.IDate("1999-12-31")), list(1999L, 12L))
expect_equal(YMS(as.IDate("2000-01-01")), list(2000L, 1L))
expect_equal(YMS(as.IDate("1948-07-01")), list(1948L, 7L))
expect_equal(YMS("2001-02", fy_month = 3L), list(2002L, 3L))

expect_error(YMS(as.IDate("1800-01-01")), "range")
expect_equal(YMS(as.IDate("2013-10-01") + (0:29)),
             list(rep(2013L, 30),
                  rep(10L, 30)))
expect_equal(YMS(as.IDate("2010-04-01")), list(2010L, 4L))
