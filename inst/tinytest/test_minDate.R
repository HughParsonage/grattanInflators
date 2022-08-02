
expect_equal(grattanInflators:::minDate(c("2015-01-01", "2014-12-30")), "2014-12")
expect_equal(grattanInflators:::minDate(c("1977-01-01", "2014-12-30")), "1977-01")
expect_equal(grattanInflators:::minDate(c("1977-01-01", "1977-02-27")), "1977-01")
expect_equal(grattanInflators:::minDate(c("1977-10-01", "1977-02-27")), "1977-02")

