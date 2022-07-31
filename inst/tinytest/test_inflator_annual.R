
library(data.table)
ii <- grattanInflators:::Inflate
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
