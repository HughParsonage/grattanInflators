#' @noRd
#' @description A function to unit test the YearMonth struct directly

YearMonthSplit <- function(x, nThread = 1L, fy_month = 6L) {
  x_class <- supported_classes(class(x))
  .check_input(x, as.IDate("1948-01-01"), as.IDate("2075-12-31"), nThread = nThread)
  .Call("C_YearMonthSplit", x, x_class, fy_month, nThread, PACKAGE = packageName())
}

Year <- function(x, nThread = 1L) {
  ans <- .Call("C_Year", x, nThread, PACKAGE = packageName())
  if (is.integer(ans)) {
    return(ans)
  }
  year(x) # nocov
}





