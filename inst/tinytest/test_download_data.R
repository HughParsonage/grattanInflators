
has_covr <- function() {
  requireNamespace("covr", quietly = TRUE) &&
    covr::in_covr()
}

if (has_covr()) {
  ans <- download_data()
  # content2series_id() includes unsupported category/adjustment combinations
  # as empty IDs, and download_data() reports those as NA. Every download that
  # was actually attempted must succeed.
  attempted <- ans[!is.na(ans)]
  expect_true(length(attempted) > 0L)
  expect_true(all(attempted == 0L))
  expect_equal(when_last_updated(), Sys.Date())
}
