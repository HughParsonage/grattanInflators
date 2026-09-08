if (requireNamespace("tinytest", quietly = TRUE)) {
  library(data.table)
  setDTthreads(1)
  # The suite is NOT skipped when the ABS data is unavailable: the parser,
  # native-safety, custom-series and synthetic-index tests must run on every
  # platform (and under the sanitizers) regardless of any download. The files
  # that genuinely need the ABS data skip themselves with exit_file().
  tinytest::test_package("grattanInflators")
}
