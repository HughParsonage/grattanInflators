#' Faster conversion to IDate for common dates
#' @param x The character vector to convert, in \code{YYYY-mm-dd} form only.
#' @param incl_day Whether or not the day is necessary to convert. Set to \code{FALSE}
#' when the day component does not matter (or is constantly -01); the day component
#' in the output will be -01.
#' @param check \code{integer: 0, 1, or 2} Level of check to perform. 0 for no
#' checks.
#' @param nThread Number of threads to use.
#'
#' @examples
#' # For ABS data, we only need to care (and check)
#' # the year and month
#' fast_as_idate("2015-12-13", incl_day = FALSE)
#'
#' @return
#' A vector of class \code{IDate}, \code{Date} the same length as \code{x}.
#'
#' @export

fast_as_idate <- function(x, incl_day = TRUE, check = 0L, nThread = 1L) {
  .check_input(x, as.IDate("1948-01-01"), as.IDate("2075-12-31"), check = check, nThread = nThread)
  o <- .Call("C_fastIDate", x, incl_day, nThread, PACKAGE = packageName())
  class(o) <- c("IDate", "Date")
  o
}

format_idate <- function(x) {
  .Call("C_format_idate", x, PACKAGE = packageName())
}


all_dates <- function() {
  ans <- .Call("C_all_dates", NULL, PACKAGE = packageName())
  class(ans) <- c("IDate", "Date")
  ans
}
