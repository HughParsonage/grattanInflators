#'


fast_as_idate <- function(x, incl_day = FALSE, check = 0L, nThread = 1L) {
  o <- .Call("C_fastIDate", x, incl_day, check, nThread, PACKAGE = packageName())
  class(o) <- c("IDate", "Date")
  o
}
