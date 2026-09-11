#' Average weekly earnings inflators and data
#' @md
#'
#' @description `awe_inflator()` uses average weekly total earnings for all
#' employees (AWE); `awote_inflator()` uses average weekly ordinary time earnings
#' for full-time adult employees (AWOTE). Both cover persons, Australia, private
#' and public sectors combined, in dollars per week, excluding salary sacrifice.
#' Changes reflect workforce composition as well as earnings changes; these
#' are earnings-level measures, unlike the Wage Price Index in [wage_inflator()].
#'
#' @details The data functions return the ABS half-yearly series for May and
#' November. The bundled May 2026 release contains original observations from
#' November 1994, and seasonal and trend observations from May 2012. Earlier
#' discontinued quarterly series are not spliced into these series.
#'
#' Inflators use the ratio of the values in the periods containing `to` and
#' `from`. May represents May to October; November represents November to April.
#' Dates are matched by month, with exact endpoint checks controlled by `check`.
#' Integer years represent January, so use an explicit May or November date
#' when that observation is intended. Financial years use `fy_month`, as in
#' [Inflate()]. Values between observations are not interpolated.
#'
#' Custom growth rates are annual rates compounded at half-yearly intervals.
#' Forecasts also retain the half-yearly frequency. Data can be refreshed with
#' [download_data()] and used directly as the `index` argument to [Inflate()].
#'
#' @inheritParams wage_inflator
#' @param series A call to `awe_original()`, `awe_seasonal()`, or `awe_trend()`
#' for `awe_inflator()`, or the corresponding `awote_*()` function for
#' `awote_inflator()`. A custom index accepted by [Inflate()] may also be used.
#' @param ... Date-rate pairs or a final annual growth rate for a custom series.
#' @return The inflators return a numeric vector of earnings ratios, or `x`
#' multiplied by those ratios. The six data functions return a `data.table`
#' with `date` (`IDate`) and `value` (dollars per week) columns.
#' @source Australian Bureau of Statistics, Average Weekly Earnings, Australia,
#' May 2026, Tables 1 (trend), 2 (seasonally adjusted), and 3 (original).
#' \url{https://www.abs.gov.au/statistics/labour/earnings-and-working-conditions/average-weekly-earnings-australia/may-2026}
#' @examples
#' awe_original()
#' awote_seasonal()
#' awe_inflator("2024-05-15", "2025-05-15")
#' awote_inflator("2024-05-15", "2025-05-15", series = awote_seasonal())
#' awote_inflator("2030-05-15", "2031-05-15", series = awote_original("3%"))
#' @export
awe_inflator <- function(from = NULL, to = NULL,
                         check = 1L,
                         series = awe_original(),
                         fy_month = 3L,
                         x = NULL,
                         nThread = getOption("grattanInflators.nThread", 1L)) {
  if (no_series_data(series)) {
    return(NULL) # nocov
  }
  Inflate(from, to, series, fy_month = fy_month, x = x,
          check = check, nThread = nThread)
}

#' @rdname awe_inflator
#' @export
awote_inflator <- function(from = NULL, to = NULL,
                           check = 1L,
                           series = awote_original(),
                           fy_month = 3L,
                           x = NULL,
                           nThread = getOption("grattanInflators.nThread", 1L)) {
  if (no_series_data(series)) {
    return(NULL) # nocov
  }
  Inflate(from, to, series, fy_month = fy_month, x = x,
          check = check, nThread = nThread)
}

earnings_custom <- function(measure, adjustment, ..., FORECAST = FALSE, LEVEL = "mean") {
  Index <- GET_SERIES(name2series_id(paste0("aus-", measure, "-", adjustment)))
  if (!is.data.table(Index) || !nrow(Index)) {
    return(Index) # nocov
  }
  if (missing(..1)) {
    if (isTRUE(FORECAST)) {
      return(.prolong_ets(Index, level = LEVEL))
    }
    return(Index)
  }
  .custom_series(Index, ...)
}

#' @rdname awe_inflator
#' @export
awe_original <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  earnings_custom("awe", "original", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname awe_inflator
#' @export
awe_seasonal <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  earnings_custom("awe", "seasonal", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname awe_inflator
#' @export
awe_trend <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  earnings_custom("awe", "trend", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname awe_inflator
#' @export
awote_original <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  earnings_custom("awote", "original", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname awe_inflator
#' @export
awote_seasonal <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  earnings_custom("awote", "seasonal", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}

#' @rdname awe_inflator
#' @export
awote_trend <- function(..., FORECAST = FALSE, LEVEL = "mean") {
  earnings_custom("awote", "trend", ..., FORECAST = FORECAST, LEVEL = LEVEL)
}
