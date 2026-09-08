library(data.table)
library(grattanInflators)

# Keep download tests offline and isolate the cache without modifying bindings
# in the package namespace. Exercise the real download/status/marker logic.
local({
  root <- tempfile("download-review-")
  on.exit(unlink(root, recursive = TRUE))
  marker <- file.path(root, "date_last_updated.rds")
  refreshed <- character()
  download <- download_data
  environment(download) <- list2env(list(
    extdata_series_id = function(sid) file.path(root, paste0(sid, ".tsv")),
    date_last_updated.rds = function() marker,
    download.file = function(url, destfile, ...) {
      if (grepl("A2B.tsv", url, fixed = TRUE)) return(1L)
      writeLines(c("date\tvalue", "2024-05-15\t100", "2024-11-15\t110"), destfile)
      0L
    },
    RM_SERIES = function(sid) refreshed <<- c(refreshed, sid)
  ), parent = environment(download))

  expect_identical(download(character()), integer())
  expect_equal(unname(download(c("", ""))), c(NA_integer_, NA_integer_))
  expect_false(dir.exists(root))
  # Unsupported combinations returned by the public catalogue mapping.
  expect_equal(unname(download(content2series_id("awe", "trimmed-mean"))), NA_integer_)
  expect_false(file.exists(marker))

  expect_equal(unname(download(c("", "A1B"))), c(NA_integer_, 0L))
  expect_equal(readRDS(marker), Sys.Date())
  expect_equal(refreshed, "A1B")

  old_date <- as.Date("2001-01-01")
  saveRDS(old_date, marker)
  download(character())
  download("")
  expect_equal(readRDS(marker), old_date)
  expect_equal(unname(download("A2B")), 1L)
  expect_equal(readRDS(marker), old_date)
  expect_equal(unname(download(c("A1B", "A2B"))), c(0L, 1L))
  expect_equal(readRDS(marker), old_date)
})

# Availability needs only filesystem metadata, even when both candidates
# exist. Parsing and freshness comparison belong to selecting an actual index.
local({
  root <- tempfile("availability-review-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  bundled <- file.path(root, "bundled.tsv")
  downloaded <- file.path(root, "downloaded.tsv")
  has_no_data <- grattanInflators_has_no_data
  environment(has_no_data) <- list2env(list(
    content2series_id = function() c("A1B", ""),
    bundled_series_id = function(sid) bundled,
    extdata_series_id = function(sid) downloaded,
    available_series_id = function(...) stop("Must not select or parse data for an existence check")
  ), parent = environment(has_no_data))
  expect_true(has_no_data())
  file.create(bundled, downloaded)
  expect_true(has_no_data())
  writeLines("nonempty bundled file", bundled)
  expect_false(has_no_data())
  writeLines("nonempty downloaded file", downloaded)
  expect_false(has_no_data())
  unlink(bundled)
  expect_false(has_no_data())
  unlink(downloaded)
  expect_true(has_no_data())
})

# Only a zero-row data.table denotes absent data. Other classes must retain
# validate_index()'s explicit type diagnostic in every exported wrapper.
valid <- data.table(date = as.IDate(c("2024-05-15", "2024-11-15")), value = c(100, 110))
for (inflator in list(awe_inflator, awote_inflator, wage_inflator, lf_inflator, cpi_inflator)) {
  for (invalid in list(as.data.frame(valid), as.list(valid), NULL, matrix(1, 2, 2))) {
    expect_error(inflator("2024-05-15", "2024-11-15", series = invalid), "must be a data.table")
  }
  result <- NULL
  expect_message(result <- inflator("2024-05-15", "2024-11-15", series = data.table()), "zero rows")
  expect_null(result)
}

# The generic native entry point must reject unknown frequencies before it
# initialises its result or writes to caller-owned x, just like C_Inflate2.
for (freq in c(0L, 3L, 6L, NA_integer_)) {
  x <- c(7, 9)
  expect_error(.Call("C_Inflate", c(2024L, 2024L), 2024L,
                     c(100, 110), as.IDate("2024-01-01"), freq, 3L, x,
                     4L, 4L, 1L, PACKAGE = "grattanInflators"),
               "frequency.*supported")
  expect_equal(x, c(7, 9))
  expect_error(.Call("C_Inflate", 2024L, 2024L,
                     c(100, 110), as.IDate("2024-01-01"), freq, 3L, NULL,
                     4L, 4L, 1L, PACKAGE = "grattanInflators"),
               "frequency.*supported")
}
