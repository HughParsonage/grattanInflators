#' CPI inflator
#'
#' @param from,to Times for which the inflator is desired. If \code{NULL}, a date
#' range close to the previous year is used.
#' @param series Which CPI series to use.
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
#' @param ... Set of date-rate pairs for custom CPI series in the future.
#' @param FORECAST Whether the series should be extended via an ETS forecast.
#' @param LEVEL If `FORECAST = TRUE` what prediction interval should be used.
#' (`LEVEL = 20` means the lower end of an 80\% prediction interval.) If `LEVEL = "mean"`
#' (the default), the central estimate is used.
#'
#' @examples
#' cpi_inflator(1995, 2019)  # Inflation from 1995 to 2019
#' cpi_inflator("2015-16", "2016-17")
#' cpi_inflator("2015-01-01", "2016-01-01")
#'
#' if (Sys.Date() < as.Date("2029-01-01")) {
#'   cpi_inflator("2030-01-01", "2031-01-01",
#'                series = cpi_original(2030, 0.1))
#'   cpi_inflator("2030-01-01", "2031-01-01",
#'                series = cpi_original(0.1))
#'   cpi_inflator("2030-01-01", "2032-01-01",
#'                series = cpi_original(2030, 0.1, 2031, 0.1, 2032, 0))
#' }
#'
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

cpi_inflator <- function(from = NULL, to = NULL,
                         series = c("seasonal", "original", "trimmed.mean",
                                    "monthly-original", "monthly-seasonal", "monthly-excl-volatile"),
                         fy_month = 3L,
                         x = NULL,
                         check = 1L,
                         nThread = getOption("grattanInflators.nThread", 1L)) {
  if (missing(series) || is.character(series)) {
    series <- match.arg(series)
    Index <- GET_SERIES(cpi2series_id(series))
  } else {
    Index <- copy(series)
  }

  sys_call <- deparse(sys.call())
  ans <- NULL
  withCallingHandlers({
    ans <-
      Inflate(from, to, Index,
              fy_month = fy_month, x = x,
              check = check,
              nThread = nThread)
  },
  error = function(e) {
    stop(sys_call, ": ", e$message, call. = FALSE)
  },
  warning = function(e) {
    warning(sys_call, ": ", e$message, call. = FALSE)
  },
  message = function(e) {
    # Uncommenting this line results in the message being duplicated
    # message(sys_call, ": ", e$message)
  })
  ans
}

cpi2series_id <- function(series, use_monthly) {
  switch(series,
         original = "A2325846C",
         seasonal = "A3604506F",
         trimmed.mean = "A3604509L",
         "monthly-original" = "A128478317T",
         "monthly-seasonal" = "A128481587A",
         "monthly-excl-volatile" = "A128473239F")

  # A128473239F // Monthly -- all groups CPI excluding volatile
  # A128478317T // Monthly -- Index Numbers ;  All groups CPI
  # A128481587A // Monthly -- Index Numbers ;  All groups CPI, seasonally adjusted

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

cpi_custom <- function(series, ..., FORECAST = FALSE, LEVEL = "mean") {
  Index <- GET_SERIES(cpi2series_id(series))
  if (!is.data.table(Index) || !nrow(Index)) {
    # handle elsewhere
    return(Index) # nocov
  }
  if (missing(..1)) {
    if (isTRUE(FORECAST)) {
      return(.prolong_ets(Index, level = LEVEL))
    }
    return(Index)
  }
  if (...length() %% 2L) {
    if (...length() == 1L) {
      return(.prolong_annual_r(Index, ...))
    }
    return(r2index(Index, ...))
    # NewIndex <-
  } else {
    return(dr2index(Index, ...))
  }
}

#' @rdname cpi_inflator
#' @export
cpi_seasonal <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  cpi_custom("seasonal", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname cpi_inflator
#' @export
cpi_original <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  cpi_custom("original", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname cpi_inflator
#' @export
cpi_trimmed_mean <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  cpi_custom("trimmed.mean", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname cpi_inflator
#' @export
cpi_monthly_original <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  cpi_custom("monthly-original", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname cpi_inflator
#' @export
cpi_monthly_seasonal <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  cpi_custom("monthly-seasonal", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname cpi_inflator
#' @export
cpi_monthly_excl_volatile <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  cpi_custom("monthly-excl-volatile", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

cpi_seasonal_fy <- function(...) {
  Index <- GET_SERIES_FY(cpi2series_id("seasonal"))
  if (missing(..1)) {
    return(Index)
  }

  if (...length() %% 2L) {
    if (...length() == 1L) {
      return(.prolong_annual_r(Index, ...))
    }
    return(r2index(Index, ...))
    # NewIndex <-
  } else {
    return(dr2index(Index, ...))
  }
}
