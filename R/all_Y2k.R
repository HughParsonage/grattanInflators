# equivalent to all(startsWith(x, 2)

all_Y2k <- function(x) {
  .Call("C_all_Y2k", x, PACKAGE = packageName())
}

anyPrior <- function(x, y) {
  .Call("C_anyPrior", x, y, PACKAGE = packageName())
}

minDate <- function(x) {
  .Call("C_minDate", x, PACKAGE = packageName())
}

