#' CPI inflator
#'
#' @param from,to Times for which the inflator is desired.
#' @param adjustment Which CPI series to use.
#' @param fy_month An integer 1-12, the month to be used for
#' years and financial years in \code{from} or \code{to}. Since the CPI is a
#' quarterly series, specifying a year is ambiguous. For
#' financial years, the month is the month of the financial year,
#' so for example \code{fy_month = 9} and "2015-16" means Sep-2015,
#' while \code{fy_month = 6} means Jun-2016.
#'
#'
#' @param x (Advanced) A vector that will be inflated in-place. If \code{NULL},
#' the default, the return vector is simply the inflation factor for `from`.
#'
#' @param check \code{integer(1)} If \code{0L}, no checks are performed, and
#' clearly invalid inputs result in \code{NA} in the output. If \code{check = 1L}
#' an error is performed for bad input; \code{check = 2L} is more thorough.
#'
#' @param nThread Number of threads to use.
#'
#' @examples
#' cpi_inflator("2015-16", "2016-17")
#' cpi_inflator("2015-01-01", "2016-01-01")
#'
#' @return
#' If `x` is `NULL`, the default, a numeric vector matching the lengths of `from`
#' and `to` equal to the inflators by which nominal prices dated `from` must be
#' multiplied so that they are in `to` real terms.
#'
#' If `x` is numeric, it is taken to be prices dated `from` and the value returned
#' is `x` in `to` real terms.
#'
#' @export

cpi_inflator <- function(from, to,
                         adjustment = c("seasonal", "original", "trimmed.mean"),
                         fy_month = 3L,
                         x = NULL,
                         check = 1L,
                         nThread = getOption("grattanInflators.nThread", 1L)) {
  adjustment <- match.arg(adjustment)
  Index <- GET_SERIES(cpi2series_id(adjustment))
  Inflate(from, to, Index, fy_month = fy_month, x = x,
          check = check,
          nThread = nThread)
}

cpi2series_id <- function(adjustment) {
  switch(adjustment,
         original = "A2325846C",
         seasonal = "A3604506F",
         trimmed.mean = "A3604509L")

}

date2freq <- function(date) {
  d_months <- (month(date[2]) - month(date[1])) %% 12L
  if (d_months == 3L) {
    return(4L)
  }
  if (d_months == 1L) {
    return(12L)
  }

  if (d_months == 0L) {
    return(1L)
  }
  # nocov start
  warning("Unable to determine frequency from dates:\n\t",
          toString(head(date, 3)))
  return(4L)
  # nocov end
}

Inflate <- function(from, to,
                    index,
                    x = NULL,
                    fy_month = 3L,
                    check = 2L,
                    nThread = getOption("grattanInflators.nThread", 1L)) {
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

  from <- .check_input(from,
                       minDate = minDate, maxDate = maxDate,
                       check = check, nThread = nThread)
  to <- .check_input(to,
                     minDate = minDate, maxDate = maxDate,
                     check = check, nThread = nThread)


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




supported_classes <- function(x) {
  match(x, c("fy", "Date", "IDate", "integer", "character"), nomatch = 0L)
}

ensure_date <- function(x) {
  if (inherits(x, "IDate")) {
    return(x)
  }
  if (inherits(x, "fy")) {
    return(fy::fy2yr(x))
  }
  if (inherits(x, "Date")) {
    return(as.IDate(x))
  }
  if (is.double(x)) {
    x <- as.integer(x)
  }
  x
}

# nocov start
cpi_inflator2 <- function(from, to) {
  from_i <- as.integer(as.IDate(from)) - as.integer(as.IDate("1948-09-01"))
  to_i <- as.integer(as.IDate(to)) - as.integer(as.IDate("1948-09-01"))
  from_i <- from_i %/% 91L + 1L
  to_i <- to_i %/% 91L + 1L
  CPI_I <- GET_SERIES(cpi2series_id("original"))
  from_value <- CPI_I$value[from_i]
  to_value <- CPI_I$value[to_i]
  to_value / from_value
}
# nocov end




