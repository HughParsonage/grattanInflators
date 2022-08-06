format_idate <- grattanInflators:::format_idate
xc <- c("2015-01-01", NA, "2014-12-31", "2013-06-30", "2000-01-29", "2020-01-29")
expect_equal(format_idate(as.IDate(xc)), xc)
