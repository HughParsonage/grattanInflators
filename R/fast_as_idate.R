#' Faster conversion to IDate for common dates
#' @param x The character vector to convert, in \code{YYYY-mm-dd} form only.
#' @param incl_day Whether or not the day is necessary to convert. Set to \code{FALSE}
#' when the day component does not matter (or is constantly -01); the day component
#' in the output will be -01.
#' @param check \code{integer: 0, 1, or 2} Level of check to perform. 0 for no
#' checks.
#' @param nThread Number of threads to use.
#' @param format The expected format of the input.
#'
#' @examples
#' # For ABS data, we only need to care (and check)
#' # the year and month
#' fast_as_idate("2015-12-13", incl_day = FALSE)
#'
#' @details
#' A 10M vector of dates was observed to be parsed in 0.1s whereas
#' \code{as.IDate} took 9.0s, and \code{lubridate::ymd}, 1.6s.
#' Note that false dates (such as Feb 30)
#' will be naively parsed without warning or error (unless `check` is
#' changed from its default argument).
#'
#' @return
#' A vector of class \code{IDate}, \code{Date} the same length as \code{x}.
#'
#' @export

fast_as_idate <- function(x, incl_day = TRUE, check = 0L, nThread = 1L, format = "%Y-%m-%d") {
  .check_input(x, as.IDate("1948-01-01"), as.IDate("2075-12-31"), check = check, nThread = nThread,
               # "character"
               xclass = 5L)
  o <- .Call("C_fastIDate", x, incl_day, format, nThread, PACKAGE = packageName())
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

guess_format <- function(x) {
  .Call("C_guess_date_format", x, PACKAGE = packageName())
}
