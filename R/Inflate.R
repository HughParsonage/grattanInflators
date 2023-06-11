#' Generic inflator
#' @param from,to Times for which the inflator is desired. If \code{NULL}, a date
#' range close to the previous year is used.
#' @param index A table of at least two columns, named \code{date} and \code{value}.
#' \code{date} is the
#' column of times to which \code{from}, \code{to} will be
#' matched. \code{value} is the values that determine the inflation factor.
#' @param x (Advanced) A vector that will be inflated in-place. If \code{NULL},
#' the default, the return vector is simply the inflation factor for `from`.
#'
#' @param fy_month An integer 1-12, the month to be used for
#' years and financial years in \code{from} or \code{to}. For
#' financial years, the month is the month of the financial year,
#' so for example \code{fy_month = 9} and "2015-16" means Sep-2015,
#' while \code{fy_month = 6} means Jun-2016.
#'
#' @param check \code{integer(1)} If \code{0L}, no checks are performed, and
#' clearly invalid inputs result in \code{NA} in the output. If \code{check = 1L}
#' an error is performed for bad input; \code{check = 2L} is more thorough.
#'
#' @return
#' If `x` is `NULL`, the default, a numeric vector matching the lengths of `from`
#' and `to` equal to the ratio between the corresponding values in the column
#' \code{value}.
#'
#' If `x` is numeric, those values are multiplied by the inflators, in-place.
#' @param nThread Number of threads to use.
#'
#'
#' @export

Inflate <- function(from, to,
                    index,
                    x = NULL,
                    fy_month = 3L,
                    check = 2L,
                    nThread = getOption("grattanInflators.nThread", 1L)) {
  if (is.null(from)) {
    from <- as.IDate(Sys.Date() - 365L - 180L)
  }
  if (is.null(to)) {
    to <- as.IDate(Sys.Date() - 180L)
  }

  prohibit_vector_recycling(from, to)

  index_dates <- as.IDate(.subset2(index, "date"))
  minDate <- index_dates[1L]
  maxDate <- index_dates[length(index_dates)]
  class_from <- supported_classes(class(from))
  class_to <-   supported_classes(class(to))

  if (is.na(minDate) || !inherits(minDate, "Date") || minDate < "1948-01-01") {
    stop("`minDate = ", minDate, "` but must be a date no earlier than 1948-01-01")
  }
  if (is.double(x) && length(from) == 1L && length(to) == 1L) {
    r <- Inflate(from, to, index, fy_month = fy_month, check = check, nThread = 1L)
    .Call("C_multiply", x, r, nThread, PACKAGE = packageName())
    return(x)
  }

  from <- .check_input(from,
                       minDate = minDate, maxDate = maxDate,
                       check = check, nThread = nThread)
  to <- .check_input(to,
                     minDate = minDate, maxDate = maxDate,
                     check = check, nThread = nThread)

  if (inherits(from, "IDate") && inherits(to, "IDate") && length(from) >= length(to)) {
    if (is.null(x)) {
      x <- rep(1, max(length(from), length(to)))
    }
    return(.Call("C_Inflate2",
                 x,
                 from, to, .subset2(index, "value"),
                 minDate, date2freq(index_dates), nThread,
                 PACKAGE = packageName()))
  }

  .Call("C_Inflate",
        from,
        to,
        .subset2(index, "value"),
        minDate,
        date2freq(index_dates),
        fy_month,
        x,
        class_from,
        class_to,
        nThread,
        PACKAGE = packageName())
}

