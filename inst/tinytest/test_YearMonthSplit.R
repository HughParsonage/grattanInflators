library(tinytest)
library(data.table)
library(grattanInflators)

YMS <- grattanInflators:::YearMonthSplit
Year <- grattanInflators:::Year
expect_equal(YMS(as.IDate("1999-12-31")), list(1999L, 12L))
expect_equal(YMS(as.IDate("2000-01-01")), list(2000L, 1L))
expect_equal(YMS(as.IDate("1948-07-01")), list(1948L, 7L))
expect_equal(YMS("2001-02", fy_month = 3L), list(2002L, 3L))

expect_error(YMS(as.IDate("1800-01-01")), "1800")
expect_equal(YMS(as.IDate("2013-10-01") + (0:29)),
             list(rep(2013L, 30),
                  rep(10L, 30)))
expect_equal(YMS(as.IDate("2010-04-01")), list(2010L, 4L))

expect_equal(Year(as.IDate("1970-01-01")), 1970L)
expect_equal(Year(as.IDate("1970-01-02")), 1970L)
expect_equal(Year(as.IDate("1969-12-31")), 1969L)
expect_equal(Year(as.IDate("2000-12-31")), 2000L)
expect_equal(Year(as.IDate("1999-12-31")), 1999L)

yd <- as.IDate("2004-03-03") + 5 * (1:1000)
expect_equal(Year(yd), year(yd))
expect_equal(YMS(yd), list(year(yd), month(yd)))
yd <- as.IDate("2004-03-03") + 5 * (1:1000)
expect_equal(Year(yd), year(yd))
expect_equal(YMS(yd), list(year(yd), month(yd)))

# Year outside bounds
expect_equal(Year("1800-01-01"), 1800L)
expect_equal(Year("1948-01-02"), 1948L)






