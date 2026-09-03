format_idate <- grattanInflators:::format_idate
xc <- c("2015-01-01", NA, "2014-12-31", "2013-06-30", "2000-01-29", "2020-01-29")
expect_equal(format_idate(as.IDate(xc)), xc)

# Every supported date must round-trip. bsearch_nrst() can only return one
# less than its upper bound, so December 2075 -- the last month of the
# table, and inside the supported range -- used to be reported as November,
# and format_idate() produced impossible strings such as "2075-11-61".
library(data.table)
ad <- grattanInflators:::all_dates()
expect_equal(format_idate(ad), as.character(ad))
expect_equal(grattanInflators:::Year(ad), year(ad))
expect_equal(grattanInflators:::YearMonthSplit(ad), list(year(ad), month(ad)))
expect_equal(grattanInflators::fast_as_idate(as.character(ad)), ad)

# the two ends of the supported range explicitly
expect_equal(format_idate(as.IDate(c("1948-01-01", "2075-12-01", "2075-12-31"))),
             c("1948-01-01", "2075-12-01", "2075-12-31"))
expect_equal(grattanInflators:::YearMonthSplit(as.IDate("2075-12-31")), list(2075L, 12L))
# out of range on either side is NA, not a wrapped date
expect_equal(format_idate(as.integer(as.IDate(c("1947-12-31", "2076-01-01")))),
             c(NA_character_, NA_character_))
