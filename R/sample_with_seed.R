
# nocov start
sample_with_seed <- function(x, seed = NULL, n = length(x)) {
  if (requireNamespace("withr", quietly = TRUE)) {
    y <-
      withr::with_seed(seed, {
        hutils::samp(x, size = n, loud = FALSE)
      })
  } else {
    set.seed(seed)
    y <- hutils::samp(x, size = n, loud = FALSE)
    set.seed(NULL)
  }
  return(y)
}
# nocov end
