
has_covr <- function() {
  requireNamespace("covr", quietly = TRUE) &&
    covr::in_covr()
}

# Every series needed by an exported inflator is part of the installed package
# and is usable even when the user data directory is empty.
series_id <- unique(content2series_id())
series_id <- series_id[nzchar(series_id)]
expect_equal(length(series_id), 12L)

bundled <- vapply(series_id,
                  grattanInflators:::bundled_series_id,
                  character(1L))
expect_true(all(nzchar(bundled)))
expect_true(all(file.exists(bundled)))

for (i in seq_along(series_id)) {
  index <- grattanInflators:::read_cached_series(bundled[i], series_id[i])
  expect_true(nrow(index) >= 2L)
}

empty_user_data <- tempfile("grattanInflators-empty-user-data-")
withr::with_envvar(c(R_USER_DATA_DIR = empty_user_data), {
  selected <- vapply(series_id,
                     grattanInflators:::available_series_id,
                     character(1L))
  expect_equal(selected, bundled)
  expect_false(grattanInflators_has_no_data())
  expect_false(dir.exists(empty_user_data))
})

# A downloaded copy remains the preferred source for updates.
updated_user_data <- tempfile("grattanInflators-updated-user-data-")
withr::with_envvar(c(R_USER_DATA_DIR = updated_user_data), {
  downloaded <- grattanInflators:::extdata_series_id(series_id[1L])
  dir.create(dirname(downloaded), recursive = TRUE)
  file.copy(bundled[1L], downloaded)
  expect_equal(grattanInflators:::available_series_id(series_id[1L]),
               downloaded)
})

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
