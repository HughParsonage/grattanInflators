if (requireNamespace("tinytest", quietly = TRUE)) {
  library(data.table)
  setDTthreads(1)
  # The bundled ABS snapshot lets the entire suite run without internet access.
  tinytest::test_package("grattanInflators")
}
