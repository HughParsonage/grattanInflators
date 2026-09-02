library(tinytest)
library(grattanInflators)
guess_format <- grattanInflators:::guess_format

expect_equal(guess_format("01-01-2024"), "%d-%m-%Y")
expect_equal(guess_format("2024-01-01"), "%Y-%m-%d")
expect_equal(guess_format("01Apr2024"), "%d%b%Y")
expect_equal(guess_format("01/01/2024"), "%d/%m/%Y")
expect_equal(guess_format("2024/01/02"), "%Y/%m/%d")
expect_equal(guess_format("2024.01.02"), "%Y.%m.%d")

# Days of 30 and 31 are unambiguous but used to fall through to NULL.
expect_equal(guess_format("30-01-2024"), "%d-%m-%Y")
expect_equal(guess_format("31-01-2024"), "%d-%m-%Y")
expect_equal(guess_format("31/12/2024"), "%d/%m/%Y")

# guess_format() and fast_as_idate() must agree: whatever the guesser reports,
# the parser must actually parse that way. Previously the guesser could return
# "%d-%m-%Y", which the parser did not recognise and silently read year-first.
dates <- as.IDate(c("2024-01-02", "2024-12-31", "1999-06-30", "2024-02-29",
                    "2020-11-30", "1948-01-01"))
for (fmt in c("%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d",
             "%Y/%m/%d", "%Y.%m.%d")) {
  x <- format(dates, fmt)
  expect_equal(guess_format(x), fmt, info = fmt)
  expect_equal(fast_as_idate(x, format = guess_format(x)), dates, info = fmt)
  expect_equal(fast_as_idate(x, format = fmt, check = 1L), dates, info = fmt)
}

# an unrecognised format is an error, not a silent reinterpretation
expect_error(fast_as_idate("01-02-2024", format = "%m-%d-%Y"), "not supported")
expect_error(fast_as_idate("2024-01-01", format = "gibberish"), "not supported")
expect_error(fast_as_idate("2024-01-01", format = c("%Y-%m-%d", "%d/%m/%Y")), "single")
expect_error(fast_as_idate(2024), "character")

# NULL when nothing in the vector is even the length of a date
expect_null(guess_format("zzzz"))
expect_null(guess_format(NA_character_))
expect_null(guess_format(character(0)))
