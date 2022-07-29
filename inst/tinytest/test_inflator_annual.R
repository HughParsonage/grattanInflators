
library(data.table)
ii <- grattanInflators:::Inflate
Index <- data.table(date = as.IDate(paste0(2000:2020, "-01-01")),
                    value = 1.01^(0:20))
expect_equal(ii("2015-16", "2016-17", index = Index), 1.01)

