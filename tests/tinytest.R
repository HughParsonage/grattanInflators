
if (requireNamespace("tinytest", quietly = TRUE)) {
  library(data.table)
  setDTthreads(1)
  tinytest::test_package("grattanInflators")
}

