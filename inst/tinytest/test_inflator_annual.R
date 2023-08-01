
library(data.table)
ii <-  grattanInflators:::Inflate
Index <- data.table(date = as.IDate(paste0(2000:2020, "-01-01")),
                    value = 1.01^(0:20))
expect_equal(ii("2015-16", "2016-17", index = Index), 1.01)
# expect_equal(ii("2015-16", "2021-22", index = Index), 1.01)
expect_error(ii("2015-02-29", "2015-02-28", index = Index, check = 2L), "2015-02-29")
bb <- c(NA, "2015-02-aa")
expect_error(ii(bb, "2015-02-28", index = Index, check = 2L), "bb")
# leap year
zfr <-  "2015-02-29"
expect_error(ii("2000-02-29", zfr, index = Index, check = 2L), "zfr")
expect_error(ii("2001-02-28", zfr, index = Index, check = 2L), "zfr")

zff <- 1999
expect_error(ii(zff, 2020, index = Index), "zff")
zff <- 1999L
expect_error(ii(zff, 2020, index = Index), "zff")
zff <- as.integer(zff)
expect_error(ii(zff, 2020, index = Index), "zff")
zff <- 2040
expect_error(ii(zff, 2020, index = Index), "zff")
zff <- as.integer(zff)
expect_error(ii(zff, 2020, index = Index), "zff")
zfi <- c(NA, as.IDate("1999-01-01"))
expect_error(ii(zfi, 2020, index = Index), "zfi")
zfj <- as.IDate("2077-12-30")
expect_error(ii(zfj, 2020, index = Index), "zfj")
x <- c(0, 1, 2)
expect_equal(ii("2015-01-01", "2016-01-01", index = Index, x = x), c(0, 1.01, 2.02))
expect_equal(ii(as.IDate("2015-01-01"), as.IDate("2016-01-01"), index = Index, x = x),
             c(0, 1.01, 2.02) * 1.01)



z2 <- as.IDate(c("2015-04-04", "2015-01-01", "2014-01-01", "2010-01-02"))
z1 <- as.IDate(c("2015-04-04", "2015-01-01", "2014-01-01", "2010-01-02"))
expect_equal(ii(z1, z2, index = Index), 1 / ii(z2, z1, index = Index))
expect_equal(ii(z1, z2, index = Index), 1 / ii(z2, z1, index = Index))
expect_equal(ii(z1[1], z2, index = Index), 1 / ii(z2, z1[1], index = Index))
c1 <- as.character(z1)
expect_equal(ii(c1[1], z2, index = Index), 1 / ii(c1, z1[1], index = Index))
expect_equal(ii(c1, z2, index = Index), 1 / ii(c1, z1, index = Index))

z2 <- as.Date(c("2015-04-04", "2015-01-01", "2014-01-01", "2010-01-02"))
z1 <- as.Date(c("2015-04-04", "2015-01-01", "2014-01-01", "2010-01-02"))
expect_equal(ii(z1, z2, index = Index), 1 / ii(z2, z1, index = Index))
expect_equal(ii(z1[1], z2, index = Index), 1 / ii(z2, z1[1], index = Index))
c1 <- as.character(z1)
expect_equal(ii(c1[1], z2, index = Index), 1 / ii(c1, z1[1], index = Index))
expect_equal(ii(c1, z2, index = Index), 1 / ii(c1, z1, index = Index))
expect_equal(ii(c1, NA_character_, index = Index), rep(NA_real_, length(c1)))
expect_equal(ii(c1, from = NA_character_, index = Index), rep(NA_real_, length(c1)))
expect_equal(ii(c1, as.IDate(NA), index = Index), rep(NA_real_, length(c1)))
expect_equal(ii(c1, from = as.IDate(NA), index = Index), rep(NA_real_, length(c1)))

expect_error(ii(z1, z2, index = data.table(date = as.IDate("1900-01-01"), value = 1)),
             "minDate")

# test NAs
a <- c(NA, "2015-01-01", "2021-01-01")
expect_error(ii(a, z2[1:3], index = Index), "latest allowable")

# prohibit vector recycling
expect_error(ii(z2, z1[-1], index = Index), "length")


