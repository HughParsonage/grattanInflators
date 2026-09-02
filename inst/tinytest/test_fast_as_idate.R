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

cj_dates <- CJ(y = 1970:2030, m = 1:12, d = 1:28)
cj_dates[, c("dmy", "ddmy", "dmmy") := list(sprintf("%d/%d/%d", d, m, y),
                                            sprintf("%02d/%d/%d", d, m, y),
                                            sprintf("%d/%02d/%d", d, m, y))]
as_IDate_x <- as.IDate(x)
expect_equal(as_IDate_x, fast_as_idate(cj_dates$dmy, format = "%d/%m/%Y"))
expect_equal(as_IDate_x, fast_as_idate(cj_dates$ddmy, format = "%d/%m/%Y"))
expect_equal(as_IDate_x, fast_as_idate(cj_dates$dmmy, format = "%d/%m/%Y"))

# Non-default formats honour the same checking contract as the default. In
# particular, strict checking rejects calendar days that would otherwise roll
# into the next month, and abbreviated month names must match all three letters.
expect_error(fast_as_idate("31/02/2024", format = "%d/%m/%Y", check = 2L),
             "could not be parsed")
expect_error(fast_as_idate("29/02/2023", format = "%d/%m/%Y", check = 2L),
             "could not be parsed")
expect_equal(fast_as_idate("29/02/2024", format = "%d/%m/%Y", check = 2L),
             as.IDate("2024-02-29"))
expect_error(fast_as_idate("01Fxx2024", format = "%d%b%Y", check = 1L),
             "could not be parsed")
expect_equal(fast_as_idate(c("01Feb2024", "01fEB2024"), format = "%d%b%Y", check = 1L),
             as.IDate(c("2024-02-01", "2024-02-01")))

# Chunked parsing must give identical results for repeated and mixed strings,
# including missing values, regardless of the requested thread count.
mixed_dates <- rep(c("2000-01-01", "2024-02-29", NA_character_, "2075-12-31"),
                   25000L)
expect_equal(fast_as_idate(mixed_dates, check = 2L, nThread = 2L),
             as.IDate(mixed_dates))
