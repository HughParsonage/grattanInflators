
if (requireNamespace("tinytest", quietly = TRUE)) {
  library(data.table)
  setDTthreads(1)
  if (isFALSE(grattanInflators::grattanInflators_has_no_data())) {
    tinytest::test_package("grattanInflators")
  }
}

