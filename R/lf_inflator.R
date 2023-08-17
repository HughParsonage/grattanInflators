#' Labour force inflator
#' @description Uses the Labour Force Index to provide equivalent sizes of the
#' labour force over different times by multiplying by the simple ratio of the
#' sizes on those dates.
#'
#' @param from,to Times for which the inflator is desired. If \code{NULL}, a date
#' range close to the previous year is used.
#' @param check \code{integer(1)} If \code{0L}, no checks are performed, and
#' clearly invalid inputs result in \code{NA} in the output. If \code{check = 1L}
#' an error is performed for bad input; \code{check = 2L} is more thorough.
#' @param series A call to `lfi_original()`, `lfi_seasonal()`, or `lfi_trend()`.
#' @param ... Set of date-rate pairs for custom labour force series in the future.
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
#' @examples
#' # The relative size of the labour force in FY 2016-17
#' # compared to FY 2015-16
#' lf_inflator("2015-16", "2016-17")
#'
#' @return
#' If `x` is `NULL`, the default, a numeric vector matching the lengths of `from`
#' and `to` equal to the relative size of the labour force of `from` and `to`.
#'
#' If `x` is numeric, it is taken to be the sizes of the labour force on dates `from`
#' and the value returned is the equivalent size of `x` on dates `to` (by simple
#' multiplication).
#'
#' @export
lf_inflator <- function(from = NULL, to = NULL,
                        check = 1L,
                        series = lfi_original(),
                        x = NULL,
                        nThread = getOption("grattanInflators.nThread", 1L)) {
  Index <- GET_SERIES(lfi2series_id("original"))
  Inflate(from, to, index = Index,
          check = check,
          x = x,
          nThread = nThread)
}

lfi2series_id <- function(adjustment) {
  # nocov start
  switch(adjustment,
         "original" = "A84423085A",
         "seasonal" = "A84423043C",
         "trend" = "A84423127L",
         "A84423127L")
  # nocov end
}

lfi_custom <- function(series, ..., FORECAST = FALSE, LEVEL = "mean") {
  Index <- GET_SERIES(lfi2series_id(series))
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

#' @rdname lf_inflator
#' @export
lfi_original <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  lfi_custom("original", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}


#' @rdname lf_inflator
#' @export
lfi_seasonal <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  lfi_custom("seasonal", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}


#' @rdname lf_inflator
#' @export
lfi_trend <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  lfi_custom("trend", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}
