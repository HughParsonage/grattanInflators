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
    from <- as.IDate(Sys.Date() - 365L - 180L) # nocov
  }
  if (is.null(to)) {
    to <- as.IDate(Sys.Date() - 180L) # nocov
  }

  from_vname <- varname(from, "from")
  to_vname <- varname(to, "to")

  prohibit_vector_recycling(from, to)
  from_class <- supported_classes(class(from))
  to_class <- supported_classes(class(to))
  from <- ensure_date(from)
  to <- ensure_date(to)

  # nocov start
  if (!is.data.table(index) || !nrow(index)) {
    message("Index had zero rows, possibly due to a faulty download, so returning NULL.")
    return(NULL)
  }
  # nocov end

  index_dates <- as.IDate(.subset2(index, "date"))
  minDate <- index_dates[1L]
  maxDate <- index_dates[length(index_dates)]
  if (is.na(minDate) || !inherits(minDate, "IDate") || minDate < MIN_DATE || minDate > MAX_DATE) {
    stop("index[1] = ", as.character(minDate), " but the only supported dates are between 1948 and 2075")
  }
  if (is.na(maxDate) || !inherits(maxDate, "IDate") || maxDate < MIN_DATE || maxDate > MAX_DATE) {
    stop("index[1] = ", as.character(maxDate), " but the only supported dates are between 1948 and 2075")
  }

  from_beyond <- .check_input(from,
                              minDate = minDate, maxDate = maxDate,
                              check = check, nThread = nThread, fy_month = fy_month, var = from_vname,
                              xclass = from_class)
  to_beyond <- .check_input(to,
                            minDate = minDate, maxDate = maxDate,
                            check = check, nThread = nThread, fy_month = fy_month, var = to_vname,
                            xclass = to_class)
  if (check < 2L) {
    if (from_beyond || to_beyond) {
      if (check == 1L) {
        signalCondition(simpleWarning(paste0("`from` or `to` had dates beyond the last date in the series (", as.character(maxDate), "), so projected values will be used.")))
      } else {
        message("`from` or `to` had dates beyond the last date in the series (", as.character(maxDate), "), so projected values will be used.")
      }
      index <- .prolong_ets(index)
      index_dates <- as.IDate(.subset2(index, "date"))
      maxDate <- index_dates[length(index_dates)]
    }
  }

  class_from <- supported_classes(class(from))
  class_to <-   supported_classes(class(to))

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
# nocov start
.prolong_Index <- function(index, until) {
  stopifnot(inherits(until, "IDate"))
  index_dates <- .subset2(index, "date")
  index_values <- .subset2(index, "value")
  freq <- date2freq(index_dates)
  if (freq == 1L) {
    r <- last(index_values) / index_values[length(index_values) - 1]
    yrs <- year(until) - year(last(index_dates))
    new_value <- last(index_values) * r^(seq_len(yrs + 1))
    new_dates <- seq(last(index_dates), by = "1 year", length.out = yrs + 2)
    return(rbind(index, data.table(date = new_dates, value = new_value))[date <= MAX_DATE])
  }
  if (freq == 4) {
    r <- last(index_values) / index_values[length(index_values) - 4]
    yrs <- 4 * (year(until) - year(last(index_dates)))
    new_value <- last(index_values) * r^((seq_len(yrs + 4)) / 4)
    new_dates <- seq(last(index_dates), by = "3 months", length.out = yrs + 5)[-1]
    return(rbind(index, data.table(date = new_dates, value = new_value))[date <= MAX_DATE])
  }
  if (freq == 12) {
    r <- last(index_values) / index_values[length(index_values) - 12]
    yrs <- 4 * (year(until) - year(last(index_dates)))
    new_value <- last(index_values) * r^((seq_len(yrs + 4)) / 4)
    new_dates <- seq(last(index_dates), by = "3 months", length.out = yrs + 5)[-1]
    return(rbind(index, data.table(date = new_dates, value = new_value))[date <= MAX_DATE])
  }

}
# nocov end
.prolong_ets <- function(index, level = "mean") {
  if (!requireNamespace("fable", quietly = TRUE)) {
    message(".prolong_ets requires the fable package, so using simple average rate.")
    return(.prolong_Index(index, as.IDate("2075-12-01")))
  }
  o <- setDTthreads(1)
  tsind <- fable::as_tsibble(copy(index)[, "ind" := .I], index = "ind", regular = TRUE)
  setDTthreads(o)
  value <- NULL
  mab <- fabletools::model(tsind, value = fable::ETS(log(value)))
  fab <- fabletools::forecast(mab, h = 700L) # 700 is more than the number required to 2075 for monthly data now
  if (requireNamespace("distributional", quietly = TRUE) &&
      is.numeric(level) && length(level) == 1 && !is.na(level) && between(level, 0, 100)) {
    .level <-
      if (use_lower <- (level < 50)) {
        100 - level
      } else {
        level
      }

    hilo_ <- distributional::hilo(.subset2(fab, "value"), .level)
    new_value <- .subset2(hilo_, if (use_lower) "lower" else "upper")
  } else {
    if (!identical(level, "mean")) {
      warning("level was neither a single value between 0 and 100 nor 'mean', so will be ignored. Using level = 'mean'.")
    }
    new_value <- fab[[".mean"]]
  }
  index_dates <- .subset2(index, "date")
  new_dates <-
    switch(as.character(date2freq(index_dates)),
           "1" = seq(last(index_dates), by = "1 year", length.out = length(new_value) + 1)[-1],
           "4" = seq(last(index_dates), by = "3 months", length.out = length(new_value) + 1)[-1],
           "12" = seq(last(index_dates), by = "1 month", length.out = length(new_value) + 1)[-1])
  rbind(index, data.table(date = new_dates, value = new_value)[date <= last(all_dates())])
}

.prolong_annual_r <- function(index, r) {
  index_dates <- .subset2(index, "date")
  new_dates <-
    switch(as.character(date2freq(index_dates)),
           "1" = seq(last(index_dates), by = "1 year", to = last(all_dates()))[-1],
           "4" = seq(last(index_dates), by = "3 months", to = last(all_dates()))[-1],
           "12" = seq(last(index_dates), by = "1 month", to = last(all_dates()))[-1])

  pow <- seq_along(new_dates) / date2freq(index_dates)

  ans <-
    rbind(index,
          data.table(date = new_dates,
                     value = last(.subset2(index, "value")) * (1 + r) ^ pow))
  ans[date %between% c(MIN_DATE, MAX_DATE)]
}

MIN_DATE <- as.IDate("1948-01-01")
MAX_DATE <- as.IDate("2075-12-31")





