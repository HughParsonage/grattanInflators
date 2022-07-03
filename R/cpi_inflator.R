#' CPI inflator
#'
#' @param from,to Times for which the inflator is desired.
#' @export

cpi_inflator <- function(from, to,
                         adjustment = c("seasonal", "original", "trimmed.mean"),
                         fy_month = 1L,
                         .from_constant_form = FALSE,
                         .to_constant_form = FALSE) {
  adjustment <- match.arg(adjustment)
  Index <- GET_SERIES(cpi2series_id(adjustment))
  Inflate(from, to, Index)
}

cpi2series_id <- function(adjustment) {
  switch(adjustment,
         original = "A2325846C",
         seasonal = "A3604506F",
         trimmed.mean = "A3604509L")

}

Inflate <- function(from, to,
                    index,
                    nThread = getOption("grattanInflators.nThread", 1L)) {
  index_min_date <- min(as.integer(as.IDate(.subset2(index, "date"))), na.rm = TRUE)
  .Call("C_Inflate",
        ensure_date(from),
        ensure_date(to),
        .subset2(index, "value"),
        index_min_date,
        4L,
        NULL,
        supported_classes(class(from)),
        supported_classes(class(to)),
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
  CPI_I <- fread(extdata_cpi_tsv(), sep = "\t")
  from_value <- CPI_I$value[from_i]
  to_value <- CPI_I$value[to_i]
  to_value / from_value
}


