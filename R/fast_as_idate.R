#' Faster conversion to IDate for common dates
#' @param x The character vector to convert, in \code{YYYY-mm-dd} form only.
#' @param incl_day Whether or not the day is necessary to convert. Set to \code{FALSE}
#' when the day component does not matter (or is constantly -01); the day component
#' in the output will be -01.
#' @param check \code{integer: 0, 1, or 2} Level of check to perform. 0 for no
#' checks; 1 errors on any element that cannot be parsed; 2 additionally
#' rejects impossible days of the month (such as 30 February).
#' @param nThread Number of threads to use.
#' @param format The expected format of the input: one of \code{"\%Y-\%m-\%d"},
#' \code{"\%d/\%m/\%Y"}, \code{"\%d-\%m-\%Y"} or \code{"\%d\%b\%Y"}. An
#' unrecognised format is an error rather than being reinterpreted as another.
#' \code{guess_format()} returns a format that this function accepts.
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
  if (!is.character(x)) {
    stop("`x` was of class <", toString(class(x)), "> but must be a character vector.")
  }
  # .check_input validates the year-first grammar only, so it must not be
  # applied to a day-first or month-name format.
  if (check >= 1L && identical(format, "%Y-%m-%d")) {
    .check_input(x, MIN_DATE, MAX_DATE, check = check, nThread = nThread,
                 # "character"
                 xclass = 5L)
  }
  o <- .Call("C_fastIDate", x, incl_day, check, format, nThread,
             PACKAGE = packageName())
  if (check >= 1L) {
    # every format gets the same contract: an unparseable element is an error
    bad <- which(is.na(o) & !is.na(x))
    if (length(bad)) {
      stop("`x[", bad[1L], "] = ", x[bad[1L]],
           "` could not be parsed as a date in format \"", format, "\".")
    }
  }
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
