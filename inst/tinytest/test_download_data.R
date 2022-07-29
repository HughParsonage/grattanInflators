
has_covr <- function() {
  requireNamespace("covr", quietly = TRUE) &&
    covr::in_covr()
}

if (tinytest::at_home() || has_covr()) {
  ans <- download_data()
  expect_equal(ans, 0L)
  expect_equal(when_last_updated(), Sys.Date())
}


