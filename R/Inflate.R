#' Generic inflator
#' @param from,to Times for which the inflator is desired. If \code{NULL}, a date
#' range close to the previous year is used.
#' @param index A table of at least two columns, named \code{date} and \code{value}.
#' \code{date} is the
#' column of times to which \code{from}, \code{to} will be
#' matched. \code{value} is the values that determine the inflation factor.
#'
#' The dates must be a strictly increasing, regular annual, quarterly, or
#' monthly sequence, and the values finite and nonzero: the underlying
#' implementation locates an observation by arithmetic on the first date, not
#' by a lookup, so an irregular or unsorted table would silently give the wrong
#' answer. \code{Inflate} validates this before use.
#'
#' @param x (Advanced) A double vector that will be inflated in-place. If
#' \code{NULL}, the default, the return vector is simply the inflation factor
#' for `from`.
#'
#' Because \code{x} is modified in place, any other name bound to the same
#' object is modified too. An integer \code{x} is coerced to double, which
#' necessarily copies, so in that case the result must be taken from the return
#' value rather than from \code{x}.
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
#' Note that \code{check} governs how loudly invalid \emph{dates} are reported.
#' It never governs whether the index is bounds-checked: an out-of-range date
#' always yields \code{NaN} rather than reading beyond the index.
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
  fy_month <- validate_fy_month(fy_month)
  if (is.null(from)) {
    from <- as.IDate(Sys.Date() - 365L - 180L) # nocov
  }
  if (is.null(to)) {
    to <- as.IDate(Sys.Date() - 180L) # nocov
  }

  from_vname <- varname(from, "from")
  to_vname <- varname(to, "to")

  prohibit_vector_recycling(from, to)
  from_was_fy <- inherits(from, "fy")
  to_was_fy <- inherits(to, "fy")
  from_class <- supported_classes(class(from))
  to_class <- supported_classes(class(to))
  from <- ensure_date(from, fy_month = fy_month, var = from_vname, check = check)
  to <- ensure_date(to, fy_month = fy_month, var = to_vname, check = check)
  # ensure_date() may change the storage representation. Keep CLASS_FY for its
  # compact ending-year representation; otherwise report the converted type.
  from_class <- converted_class(from, from_class)
  to_class <- converted_class(to, to_class)

  # The native kernels locate an observation by arithmetic on the index's first
  # date and its frequency, so the index must genuinely be a regular series.
  # This is validated unconditionally: it costs O(nrow(index)) on a table of a
  # few hundred rows, and every result depends on it.
  index_dates <- validate_index(index)
  # Native calculation vectors are doubles. Accept ordinary integer-valued
  # custom indices without modifying a caller-owned data.table by reference.
  index_value <- as.double(.subset2(index, "value"))
  minDate <- index_dates[1L]
  maxDate <- index_dates[length(index_dates)]

  x <- validate_x(x, from, to)

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
      until <- max(.requested_until(from, from_class, fy_month),
                   .requested_until(to, to_class, fy_month))
      index <- .prolong_ets(index, until = until)
      index_dates <- validate_index(index)
      index_value <- as.double(.subset2(index, "value"))
      maxDate <- index_dates[length(index_dates)]
    }
  }

  if (is.double(x) && length(from) == 1L && length(to) == 1L) {
    # Compute the scalar factor directly. Recursing through Inflate() repeats
    # all index and input validation, which is noticeable when the actual work
    # is a single factor followed by an in-place vector multiplication.
    r <- .Call("C_Inflate",
               from, to, index_value, minDate,
               date2freq(index_dates), fy_month, NULL,
               from_class, to_class, 1L,
               PACKAGE = packageName())
    .Call("C_multiply", x, r, nThread, PACKAGE = packageName())
    return(x)
  }



  # Financial years retain their compact ending-year representation and use
  # the generic kernel. Date/IDate inputs use the specialised native path.
  if (!from_was_fy && !to_was_fy &&
      inherits(from, "IDate") && inherits(to, "IDate") &&
      length(from) >= length(to)) {
    if (is.null(x)) {
      x <- rep(1, length(from))
    }
    return(.Call("C_Inflate2",
                 x,
                 from, to, index_value,
                 minDate, date2freq(index_dates), nThread,
                 PACKAGE = packageName()))
  }

  .Call("C_Inflate",
        from,
        to,
        index_value,
        minDate,
        date2freq(index_dates),
        fy_month,
        x,
        from_class,
        to_class,
        nThread,
        PACKAGE = packageName())


}

validate_fy_month <- function(fy_month) {
  if (length(fy_month) != 1L || !is.numeric(fy_month) || is.na(fy_month) ||
      !is.finite(fy_month) || fy_month != trunc(fy_month) ||
      fy_month < 1L || fy_month > 12L) {
    stop("`fy_month` must be one integer from 1 to 12.")
  }
  as.integer(fy_month)
}

# Return the latest requested month without allocating vectors proportional to
# the input length. Character input is scanned and parsed in native code.
.requested_until <- function(x, xclass, fy_month) {
  if (!length(x)) {
    return(MIN_DATE)
  }
  if (inherits(x, "IDate")) {
    latest <- suppressWarnings(max(x, na.rm = TRUE))
    return(if (is.finite(latest)) as.IDate(latest) else MIN_DATE)
  }
  if (is.integer(x)) {
    yr <- suppressWarnings(max(x, na.rm = TRUE))
    if (!is.finite(yr)) {
      return(MIN_DATE)
    }
    mo <- 1L
    if (xclass == CLASS_FY) {
      yr <- yr - (fy_month >= 7L)
      mo <- fy_month
    }
    return(as.IDate(sprintf("%04d-%02d-01", yr, mo)))
  }
  if (is.character(x)) {
    ym <- .Call("C_maxYearMonth", x, fy_month, PACKAGE = packageName())
    if (is.na(ym[1L])) {
      return(MIN_DATE)
    }
    return(as.IDate(sprintf("%04d-%02d-01", ym[1L], ym[2L])))
  }
  MAX_DATE # nocov
}

# An index that is empty because the mirrored ABS data could not be downloaded
# is an environment problem, not a user error. The exported inflators degrade
# to a message and NULL, exactly as before, so that a machine with no internet
# access (a CRAN check machine, say) does not fail. `Inflate()` itself, which
# takes an explicit index, still errors: there an empty table is a mistake.
no_series_data <- function(index) {
  if (!is.data.table(index) || !nrow(index)) {
    message("Index had zero rows, possibly due to a faulty or absent download, ",
            "so returning NULL.")
    return(TRUE)
  }
  FALSE
}

# Validates the structural assumptions that the native kernels make of `index`
# and returns its dates as an IDate vector.
validate_index <- function(index, var = "index") {
  bad <- function(msg, cls = "grattanInflators_bad_index") {
    stop(errorCondition(paste0("`", var, "`", msg), class = cls))
  }
  if (!is.data.table(index)) {
    bad(paste0(" was of class <", toString(class(index)),
               "> but must be a data.table with columns `date` and `value`."))
  }
  if (!nrow(index)) {
    stop(errorCondition(
      paste0("`", var, "` had zero rows. If it came from the mirrored ABS data, ",
             "the download may have failed; see `download_data()`."),
      class = "grattanInflators_empty_index"))
  }
  if (!hasName(index, "date") || !hasName(index, "value")) {
    bad(paste0(" had columns ", toString(names(index)),
               " but must have columns `date` and `value`."))
  }

  dates <- .subset2(index, "date")
  if (!inherits(dates, "Date") && !inherits(dates, "IDate")) {
    bad(paste0("$date was of class <", toString(class(dates)),
               "> but must be <Date> or <IDate>."))
  }
  dates <- as.IDate(dates)
  if (anyNA(dates)) {
    bad("$date contained missing values.")
  }
  if (min(dates) < MIN_DATE || max(dates) > MAX_DATE) {
    bad(paste0("$date ranges from ", as.character(min(dates)), " to ",
               as.character(max(dates)),
               " but the only supported dates are between ",
               as.character(MIN_DATE), " and ", as.character(MAX_DATE), "."))
  }

  value <- .subset2(index, "value")
  if (!is.numeric(value)) {
    bad(paste0("$value was of type ", typeof(value), " but must be numeric."))
  }
  if (!all(is.finite(value))) {
    bad("$value contained missing or non-finite values.")
  }
  if (any(value == 0)) {
    bad("$value contained zeroes, which cannot be used as a denominator.")
  }

  if (nrow(index) < 2L) {
    bad(paste0("$date had a single observation, but at least two are required ",
               "to determine the frequency of the series."))
  }
  if (any(diff(as.integer(dates)) <= 0L)) {
    i <- which(diff(as.integer(dates)) <= 0L)[1L]
    bad(paste0("$date was not strictly increasing: ", as.character(dates[i]),
               " is followed by ", as.character(dates[i + 1L]), "."))
  }

  # Regularly spaced by whole months: the kernels reduce a date to (year,
  # month) and subtract, so only the year and month of each observation
  # matters.
  d_ym <- diff(12L * year(dates) + month(dates))
  step <- d_ym[1L]
  if (step == 0L) {
    bad(paste0("$date has two observations in the same month (",
               as.character(dates[1L]), ", ", as.character(dates[2L]),
               "), so the frequency of the series cannot be determined."))
  }
  if (!step %in% c(1L, 3L, 12L)) {
    bad(paste0("$date steps by ", step, " month(s); only annual, quarterly ",
               "and monthly series are supported."))
  }
  if (any(d_ym != step)) {
    i <- which(d_ym != step)[1L]
    bad(paste0("$date is not a regular sequence: the step from ",
               as.character(dates[i]), " to ", as.character(dates[i + 1L]),
               " is ", d_ym[i], " month(s), but the series steps by ", step, "."))
  }
  if (step == 12L && length(unique(month(dates))) != 1L) {
    bad("$date is annual but does not use a constant month.")
  }

  dates
}

# `x` is written to in place by the native code, so its type and length must be
# right before it is passed down.
validate_x <- function(x, from, to) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x)) {
    stop("`x` was of class <", toString(class(x)),
         "> but must be a numeric vector (or NULL).")
  }
  if (!is.double(x)) {
    # cannot be modified in place; the caller must use the return value
    x <- as.double(x)
  }
  if (length(from) == 1L && length(to) == 1L) {
    # x is a vector of values all dated `from`; any length is meaningful
    return(x)
  }
  n <- max(length(from), length(to))
  if (length(x) != n) {
    stop("`length(x) = ", length(x), "` but `", n,
         "` values are required (the length of `from`/`to`). ",
         "`x` is written to in place, so it must have exactly the output length.")
  }
  x
}

# Extends `index` to at least `until` by compounding the last observed
# year-on-year rate. Used when fable is unavailable.
.seq_clamped_months <- function(start, n, step) {
  if (n <= 0L) {
    return(as.IDate(character()))
  }
  .add_months(start, step * seq_len(n))
}

.prolong_Index <- function(index, until) {
  stopifnot(inherits(until, "IDate"))
  index_dates <- .subset2(index, "date")
  index_values <- .subset2(index, "value")
  n <- length(index_values)
  freq <- date2freq(index_dates)

  if (n <= freq) {
    stop("`index` has ", n, " observations, too few to determine a ",
         "year-on-year rate for a series of frequency ", freq, ".")
  }
  # the year-on-year rate implied by the last twelve months of the series
  r <- last(index_values) / index_values[n - freq]

  # whole years to `until`, rounded up, times the number of periods per year
  yrs <- max(year(until) - year(last(index_dates)) + 1L, 1L)
  n_new <- freq * yrs

  new_dates <- .seq_clamped_months(last(index_dates), n_new, 12L %/% freq)
  new_value <- last(index_values) * r ^ (seq_len(n_new) / freq)
  rbind(index, data.table(date = new_dates, value = new_value))[date <= MAX_DATE]
}

.forecast_horizon <- function(index_dates, until) {
  freq <- date2freq(index_dates)
  period_months <- 12L %/% freq
  last_ym <- 12L * year(last(index_dates)) + month(last(index_dates))
  until_ym <- 12L * year(until) + month(until)
  max(ceiling((until_ym - last_ym) / period_months), 0L)
}

.prolong_ets <- function(index, until = MAX_DATE, level = "mean") {
  if (!inherits(until, "IDate") || length(until) != 1L || is.na(until) ||
      until < MIN_DATE || until > MAX_DATE) {
    stop("`until` must be one IDate between MIN_DATE and MAX_DATE.")
  }
  index_dates <- as.IDate(.subset2(index, "date"))
  freq <- date2freq(index_dates)
  period_months <- 12L %/% freq
  until_ym <- 12L * year(until) + month(until)
  h <- .forecast_horizon(index_dates, until)
  if (h == 0L) {
    return(index)
  }
  if (!requireNamespace("fable", quietly = TRUE)) {
    message(".prolong_ets requires the fable package, so using simple average rate.")
    return(.prolong_Index(index, until))
  }
  o <- setDTthreads(1)
  # restore the thread count even if modelling or forecasting errors
  on.exit(setDTthreads(o), add = TRUE)
  tsind <- fable::as_tsibble(copy(index)[, "ind" := .I], index = "ind", regular = TRUE)
  value <- NULL
  mab <- fabletools::model(tsind, value = fable::ETS(log(value)))
  fab <- fabletools::forecast(mab, h = h)
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
  new_dates <- .seq_clamped_months(last(index_dates), length(new_value), period_months)
  ans <- rbind(index, data.table(date = new_dates, value = new_value)[date <= MAX_DATE])
  ans_dates <- .subset2(ans, "date")
  covered_through <- 12L * year(last(ans_dates)) + month(last(ans_dates)) + period_months - 1L
  if (covered_through < until_ym) {
    stop("Forecast did not reach the requested endpoint.") # nocov
  }
  ans
}

.prolong_annual_r <- function(index, r) {
  r <- .rate2rate(r)
  index_dates <- .subset2(index, "date")
  freq <- date2freq(index_dates)
  n_new <- max(.forecast_horizon(index_dates, MAX_DATE), 0L)
  new_dates <- .seq_clamped_months(last(index_dates), n_new, 12L %/% freq)

  pow <- seq_along(new_dates) / freq

  ans <-
    rbind(index,
          data.table(date = new_dates,
                     value = last(.subset2(index, "value")) * (1 + r) ^ pow))
  ans[date %between% c(MIN_DATE, MAX_DATE)]
}

MIN_DATE <- as.IDate("1948-01-01")
MAX_DATE <- as.IDate("2075-12-31")
