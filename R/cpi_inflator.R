#' CPI inflator
#'
#' @param from,to Times for which the inflator is desired.
#' @export

cpi_inflator <- function(from, to) {
  update_extdata2c()
  .Call("C_cpi_inflator",
        from,
        to,
        PACKAGE = packageName())
}

extdata_cpi_tsv <- function() {
  system.file("extdata", "cpi.tsv", package = packageName())
}

download_save_cpi_data <- function() {
  # https://github.com/HughParsonage/ABS-Catalogue/raw/master/data/series_id/A232/A2323355X.tsv
  CPI_Data <- fread("https://github.com/HughParsonage/ABS-Catalogue/raw/master/data/series_id/A232/A2325846C.tsv")
  if (dir.exists("inst/extdata")) {
    fwrite(CPI_Data, "inst/extdata/cpi.tsv", sep = "\t")
  } else if (file.exists(extdata_cpi_tsv())) {
    fwrite(CPI_Data, extdata_cpi_tsv(), sep = "\t")
  }
}

update_extdata2c <- function(verbose = FALSE, force = FALSE) {
  if (isFALSE(force) && .Call("C_cpi_prepared", PACKAGE = packageName())) {
    return(invisible(NULL))
  }
  stopifnot(file.exists(extdata_cpi_tsv()))
  CPI_Data <- fread(extdata_cpi_tsv(), sep = "\t")
  invisible(.Call("C_update_cpi", .subset2(CPI_Data, "value"), verbose, PACKAGE = packageName()))
}

supported_classes <- function(x) {
  match(x, c("fy", "Date", "IDate", "integer", "character"), nomatch = 0L)
}

ensure_date <- function(x) {
  if (inherits(x, "IDate")) {
    return(x)
  }
  if (inherits(x, "fy")) {
    return(as.IDate(fy::fy2date(x)))
  }
  if (inherits(x, "Date")) {
    return(as.IDate(x))
  }
  .Call("C_ensure_date", x, PACKAGE = packageName())

}


print_IDATE <- function(x) {
  .Call("C_print_IDATE", x, PACKAGE = packageName())
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

