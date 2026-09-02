#' Custom series
#' @name custom-series
#' @description Used when the true series is not appropriate, as when a forecast
#' is desired and the series is required beyond the original series.
#' @param index An index (i.e. a data.table with columns \code{date} and \code{value},
#' where \code{date} is an arithmetic sequence of monthly, quarterly, or annual dates),
#' and \code{value} is the indexed value for that date.
#' @param d1 A single date or value representing a date.
#' @param r1 The desired rate of increase for the index from the last date in \code{index}
#' to the end of \code{d1}. For example, \code{d1 = 2025} and \code{r1 = 0} applied
#' to a monthly \code{index} would keep \code{value} constant until \code{2025-12-01}.
#'
#' Rates are annual and may be given as a number (\code{0.05}) or as a
#' percentage string (\code{"5%"}, \code{"-2.5%"}).
#' @param ... A set of date-rate pairs.
#'
#' @return
#' \code{index} with dates extended until the last supported date. The final rate
#' supplied is the rate for all dates after the final date.
#'
#' @export

dr2index <- function(index, d1, r1, ...) {
  if (missing(d1)) {
    return(index)
  }
  if (missing(r1)) {
    stop("r1 is missing but d1 is provided.")
  }
  dots <- list(d1, r1, ...)
  index <- .dr_extend_pairs(index, dots)
  # the final rate carries beyond the final date supplied
  .prolong_annual_r(index, dots[[length(dots)]])
}

# Applies each (date, rate) pair in `dots` in turn.
.dr_extend_pairs <- function(index, dots) {
  if (length(dots) %% 2L) {
    stop("`...` must consist of date-rate pairs, but ", length(dots),
         " arguments were supplied.")
  }
  for (i in seq_len(length(dots) %/% 2L)) {
    index <- .dr_extend(index, dots[[2L * i - 1L]], dots[[2L * i]])
  }
  index
}

# Extends `index` at the annual rate `r` until (and including) the last period
# on or before `d1`.
.dr_extend <- function(index, d1, r) {
  if (fy::is_fy(d1)) {
    d_1 <- fy::fy2date(d1)
    d_0 <- fy::fy2date(fy::prev_fy(d1))
  } else if (is.numeric(d1) && !inherits(d1, "Date")) {
    if (length(d1) != 1L || !is.finite(d1) || d1 != trunc(d1)) {
      stop("Numeric `d1` must be a single, finite, whole-valued year.")
    }
    d1 <- as.integer(d1)
    d_1 <- as.IDate(sprintf("%d-12-31", d1))
    d_0 <- as.IDate(sprintf("%d-12-31", d1 - 1L))
  } else {
    d_1 <- ensure_date(d1)

    dates <- .subset2(index, "date")
    d_0 <- last(dates[dates < d_1])
  }

  # For retrospectives
  index <- index[.subset2(index, "date") <= d_0]
  if (!nrow(index)) {
    stop("`index` had no observations on or before ", as.character(d_0),
         ", so it cannot be extended to ", as.character(d_1), ".")
  }
  freq <- date2freq(.subset2(index, "date"))
  r <- .rate2rate(r)
  per_period <- (1 + r) ^ (1 / freq)

  # Collect the new rows first: appending one row at a time with rbind() is
  # quadratic, and made it easy to advance the date twice per iteration.
  new_dates <- list()
  cur <- last(.subset2(index, "date"))
  repeat {
    nd <- .next_date(cur, freq)
    if (nd > d_1) {
      break
    }
    new_dates[[length(new_dates) + 1L]] <- nd
    cur <- nd
  }
  if (!length(new_dates)) {
    return(index)
  }
  new_dates <- do.call(c, new_dates)
  rbind(index,
        data.table(date = new_dates,
                   value = last(.subset2(index, "value")) *
                     per_period ^ seq_along(new_dates)))
}

.days_in_month <- function(y, m) {
  nd <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[m]
  leap <- (y %% 4L == 0L & y %% 100L != 0L) | y %% 400L == 0L
  nd + as.integer(m == 2L & leap)
}

# Adds `n` whole months to `d`, clamping the day of the month so that e.g.
# 31 January plus one month is 28/29 February rather than an invalid date.
.add_months <- function(d, n) {
  y <- year(d)
  m <- month(d)
  dm <- mday(d)
  tot <- 12L * y + (m - 1L) + as.integer(n)
  y2 <- tot %/% 12L
  m2 <- tot %% 12L + 1L
  as.IDate(sprintf("%04d-%02d-%02d", y2, m2, pmin(dm, .days_in_month(y2, m2))))
}

# The next date in a series of the given frequency. `freq` defaults to the
# frequency implied by `dates` when a vector is supplied.
.next_date <- function(dates, freq = date2freq(dates)) {
  .add_months(last(dates), 12L %/% freq)
}

# Extends `index` by the date-rate pairs in `...`, then carries `r` (if given)
# to the end of the supported range.
r2index <- function(index, ..., r) {
  if (...length()) {
    index <- .dr_extend_pairs(index, list(...))
  }
  if (missing(r)) {
    return(index)
  }
  .prolong_annual_r(index, r)
}

# `...` is a set of date-rate pairs, optionally followed by a single trailing
# rate that carries to the end of the supported range.
.custom_series <- function(index, ...) {
  if (!...length()) {
    return(index)
  }
  dots <- list(...)
  if (length(dots) %% 2L == 0L) {
    return(dr2index(index, ...))
  }
  if (length(dots) == 1L) {
    return(.prolong_annual_r(index, dots[[1L]]))
  }
  do.call(r2index,
          c(list(index), dots[-length(dots)], list(r = dots[[length(dots)]])))
}


# A rate is either a finite number (0.05) or a string matching a strict
# numeric grammar, optionally suffixed with '%'. Deleting characters that do
# not match [0-9.] would drop a minus sign or an exponent, which changes the
# value rather than rejecting it.
.rate2rate <- function(r) {
  if (length(r) != 1L) {
    stop("r was length-", length(r), ", but must be length-one.")
  }
  if (!is.character(r) && !is.numeric(r)) {
    stop("r was type ", toString(class(r)), ", but must be character or numeric.")
  }

  if (is.character(r)) {
    if (is.na(r)) {
      stop("`r` was NA, but must be a finite rate.")
    }
    rc <- trimws(r)
    if (!grepl("^[+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:[eE][+-]?[0-9]+)?%?$", rc)) {
      stop("`r = ", r, "` is not a valid rate. Supply a number (e.g. 0.05) or ",
           "a percentage (e.g. \"5%\").")
    }
    if (endsWith(rc, "%")) {
      r <- as.numeric(sub("%$", "", rc)) / 100
    } else {
      r <- as.numeric(rc)
    }
  }
  if (is.na(r) || !is.finite(r)) {
    stop("`r` was ", r, ", but must be a finite rate.")
  }
  if (r <= -1) {
    stop("`r = ", r, "`, but a rate of -100% or less cannot be compounded.")
  }
  if (abs(r) >= 0.5) {
    message("abs(r) >= 0.5, this is an unlikely level of growth; r = 0.05 means 5% growth")
  }
  r
}
