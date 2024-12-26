#' Wage inflator
#' @description Uses the Wage Price Index
#'
#' @param from,to Times for which the inflator is desired. If \code{NULL}, a date
#' range close to the previous year is used.
#' @param check \code{integer(1)} If \code{0L}, no checks are performed, and
#' clearly invalid inputs result in \code{NA} in the output. If \code{check = 1L}
#' an error is performed for bad input; \code{check = 2L} is more thorough.
#'
#' @param series A call to `wpi_original()`, `wpi_seasonal()`, or `wpi_trend()`,
#' defining which wage price index series to use.
#' @param fy_month The month to be used in `series` for financial years.
#' @param ... Set of date-rate pairs for custom WPI series in the future.
#' @param FORECAST Whether the series should be extended via an ETS forecast.
#' @param LEVEL If `FORECAST = TRUE` what prediction interval should be used.
#' (`LEVEL = 20` means the lower end of an 80\% prediction interval.) If `LEVEL = "mean"`
#' (the default), the central estimate is used.
#'
#' @param x (Advanced) A vector that will be inflated in-place. If \code{NULL},
#' the default, the return vector is simply the inflation factor for `from`.
#'
#' @param nThread Number of threads to use.
#'
#'
#' @return
#' If `x` is `NULL`, the default, a numeric vector matching the lengths of `from`
#' and `to` equal to the inflators by which nominal wages dated `from` must be
#' multiplied so that they are in `to` real terms.
#'
#' If `x` is numeric, it is taken to be wages dated `from` and the value returned
#' is `x` in `to` real terms.
#'
#' @export
wage_inflator <- function(from = NULL, to = NULL,
                          check = 1L,
                          series = wpi_original(),
                          fy_month = 3L,
                          x = NULL,
                          nThread = getOption("grattanInflators.nThread", 1L)) {
  sys_call <- deparse(sys.call())
  ans <- NULL
  withCallingHandlers({
    ans <-
      Inflate(from, to, series,
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

wpi2series_id <- function(adjustment) {
  # nocov start
  switch(adjustment,
         "original" = "A2603609J",
         "seasonal" = "A2713849C",
         "trend" = "A2713851R",
         "A2713851R")
  # nocov end
}

wpi_custom <- function(series, ..., FORECAST = FALSE, LEVEL = "mean") {
  Index <- GET_SERIES(wpi2series_id(series))
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

#' @rdname wage_inflator
#' @export
wpi_original <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  wpi_custom("original", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}


#' @rdname wage_inflator
#' @export
wpi_seasonal <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  wpi_custom("seasonal", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}


#' @rdname wage_inflator
#' @export
wpi_trend <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  wpi_custom("trend", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}
