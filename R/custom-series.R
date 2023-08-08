#' Custom series
#' @name custom-series
#' @description Used when the true series is not appropriate, as when a forecast
#' is desired and the series is required beyond the original series.
#' @param ... A set of date-rate pairs. If an even number of arguments are passed
#' to `...` the odd positions are dates, the even positions are rates. Then
#' inflators are fixed at the rates for dates less than the corresponding dates.
#'

NULL


.next_date <- function(dates) {
  freq <- date2freq(dates)
  out <- last(dates)
  if (freq == 1L) {
    out <- as.IDate(sprintf("%d-%d-%d", year(out) + 1L, month(out), mday(out)))
    return(out)
  }
  if (freq == 4L) {
    out <- as.IDate(sprintf("%d-%d-%d", year(out) + (month(out) > 9), (month(out) %% 12L) + 3L, mday(out)))
    return(out)
  }
  as.IDate(sprintf("%d-%d-%d", year(out) + (month(out) == 12L), (month(out) %% 12L) + 1L, mday(out)))
}

r2index <- function(index, ..., r) {
  index <- dr2index(index, ...)
  if (missing(r)) {
    return(index)
  }
  r <- .rate2rate(r)
  freq <- date2freq(.subset2(index, "date"))
  while((last_date <- last(.subset2(index, "date"))) < as.IDate("2075-12-01")) {
    dates <- .subset2(index, "date")
    next_date <- .next_date(c(dates, next_date))

    index <- rbind(index,
                   data.table(date = next_date,
                              value = last(.subset2(index, "value")) * ((1 + r) ^ (1 / freq))))
  }
  index
}

dr2index <- function(index, d1, r1, ...) {
  dates <- .subset2(index, "date")
  value <- .subset2(index, "value")
  freq <- date2freq(dates)

  next_date <- .next_date(dates)

  if (fy::is_fy(d1)) {
    d_1 <- fy::fy2date(d1)
  } else if (is.numeric(d1)) {
    d_1 <- as.IDate(sprintf("%d-12-31", d1))
  } else {
    d_1 <- ensure_date(d1)
  }

  r1 <- .rate2rate(r1)

  while (last(.subset2(index, "date")) <= d_1) {
    dates <- .subset2(index, "date")
    next_date <- .next_date(c(dates, next_date))
    index <- rbind(index,
                   data.table(date = next_date,
                              value = last(.subset2(index, "value")) * ((1 + r1) ^ (1 / freq))))

  }

  if (missing(..1)) {
    return(.prolong_annual_r(index, r1))
  }
  dr2index(index, ...)
}

.rate2rate <- function(r) {
  if (length(r) != 1L) {
    stop("r was length-", length(r), ", but must be length-one.")
  }
  if (!is.character(r) && !is.numeric(r)) {
    stop("r was type ", toString(class(r)), ", but must be character or numeric.")
  }

  if (is.character(r)) {
    if (!grepl("[0-9]", r)) {
      stop("`r = ", r, "` contained no digits.")
    }
    if (endsWith(r, "%")) {
      r <- as.numeric(gsub("[^0-9.]", "", r)) / 100
    } else {
      r <- as.numeric(gsub("[^0-9.]", "", r))
    }
  }
  if (abs(r) >= 0.5) {
    message("abs(r) >= 0.5, this is an unlikely level of growth; r = 0.05 means 5% growth")
  }
  r
}





