library(tinytest)
expect_equal(grattanInflators:::guess_format("01-01-2024"), "%d-%m-%Y")
expect_equal(grattanInflators:::guess_format("2024-01-01"), "%Y-%m-%d")
expect_equal(grattanInflators:::guess_format("01Apr2024"), "%d%b%Y")
