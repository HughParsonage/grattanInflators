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
#' @param .from_constant_form,.to_constant_form Are \code{from}, \code{to}
#' each of constant form? That is, are they all consistently dates,
#' financial years or are they (possibly) a mixture? If \code{TRUE},
#' mixtures of formats will raise an error. This is useful
#' to distinguish financial years from year-month formats (e.g. 2008-09).
#'
#' @export

cpi_inflator <- function(from, to,
                         adjustment = c("seasonal", "original", "trimmed.mean"),
                         fy_month = 3L,
                         .from_constant_form = FALSE,
                         .to_constant_form = FALSE,
                         x = NULL) {
  adjustment <- match.arg(adjustment)
  Index <- GET_SERIES(cpi2series_id(adjustment))
  Inflate(from, to, Index, fy_month = fy_month,
          .from_constant_form = .from_constant_form,
          .to_constant_form = .to_constant_form)
}

cpi2series_id <- function(adjustment) {
  switch(adjustment,
         original = "A2325846C",
         seasonal = "A3604506F",
         trimmed.mean = "A3604509L")

}

date2freq <- function(date) {
  d_months <- month(date[2]) - month(date[1])
  if (d_months == 3L) {
    return(4L)
  }
  if (d_months == 1) {
    return(12L)
  }

  if (d_months == 12L) {
    return(1L)
  }
  warning("Unable to determine frequency from dates:\n\t",
          toString(head(date, 3)))
  return(4L)
}

Inflate <- function(from, to,
                    index,
                    x = NULL,
                    fy_month = 3L,
                    .from_constant_form = FALSE,
                    .to_constant_form = FALSE,
                    nThread = getOption("grattanInflators.nThread", 1L)) {
  index_dates <- as.IDate(.subset2(index, "date"))
  index_min_date <- index_dates[1L]
  .Call("C_Inflate",
        ensure_date(from),
        ensure_date(to),
        .subset2(index, "value"),
        index_min_date,
        date2freq(index_dates),
        fy_month,
        x,
        supported_classes(class(from)),
        supported_classes(class(to)),
        isTRUE(.from_constant_form),
        isTRUE(.to_constant_form),
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
  x
}




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


