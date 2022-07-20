#' @noRd
#' @description A function to unit test the YearMonth struct directly

YearMonthSplit <- function(x, nThread = 1L, fy_month = 6L) {
  x_class <- supported_classes(class(x))
  .check_input(x, as.IDate("1948-01-01"), nThread)
  .Call("C_YearMonthSplit", x, x_class, fy_month, nThread, PACKAGE = packageName())
}

Year <- function(x, nThread = 1L) {
  ans <- .Call("C_Year", x, nThread, PACKAGE = packageName())
  if (is.integer(ans)) {
    return(ans)
  }
  year(x) # nocov
}


fast_as_idate <- function(x, nThread = 1L) {
  o <- .Call("C_fastIDate", x, nThread, PACKAGE = packageName())
  class(o) <- c("IDate", "Date")
  o
}


