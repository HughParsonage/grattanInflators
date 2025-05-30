#' ABS Connections
#' @description The package uses the catalogue mirrored at \url{https://github.com/HughParsonage/ABS-Catalogue}.
#' These functions expose the guts of the package's method to connect to this mirror.
#'
#' Each inflator, plus the 'adjustment', is associated with an ABS Series ID.
#'
#' @name abs-conn
#'
#' @param broad_cat,adjustment Definitions to identify the Series ID. If any
#' are multiple, the result is of the cartesian join, \strong{not} the
#' component-wise values.
#'
#' @param series_id The Series ID desired. For \code{download_data}, if \code{NULL},
#' the default, downloads all files required.
#'
#'
#' @return
#' \describe{
#' \item{\code{content2series_id}}{A character vector, the Series ID identified
#' by `broad_cat` and `adjustment`}
#' \item{\code{download_data}}{Called for its side-effect, downloading the
#' data required. Returns an integer vector of the statuses of each download.}
#' \item{\code{when_last_updated}}{The date the downloaded data was last retrieved, or
#' the string \code{"Never"} if the file does not exist.}
#' \item{\code{grattanInflators_has_no_data}}{\code{TRUE} if no data has ever been
#' received (or package directory removed); likely due to no internet connection.}
#' }
#'
NULL

# nocov start
series_id_int <- function(series_id) {
  # convert to integer e.g. A5Z = 26 + 50
  vapply(strsplit(series_id, ""),
         function(x) {
           as.integer(sum(match(x[-1], c(1:9, LETTERS), nomatch = 0L) * 10^((length(x) - 2):0)))
         },
         0L)
}
# nocov end

#' @rdname abs-conn
#' @export
content2series_id <- function(broad_cat = c("cpi", "lfi", "wpi"),
                              adjustment = c("original", "seasonal", "trend", "trimmed-mean",
                                             "monthly-original", "monthly-seasonal", "monthly-excl-volatile")) {
  cj <- CJ(broad_cat = broad_cat,
           adjustment = adjustment,
           sorted = FALSE)
  cj[, "series_id" := name2series_id(paste0("aus-", broad_cat, "-", adjustment), FALSE)]
  .subset2(cj, "series_id")
}

name2series_id <- function(name, err_ifnotfound = TRUE) {
  if (length(name) != 1) {
    return(vapply(name, name2series_id, err_ifnotfound = err_ifnotfound, ""))
  }
  ans <-
    switch(name,
           "aus-cpi-original" = "A2325846C",
           "aus-cpi-seasonal" = "A3604506F",
           "aus-cpi-trimmed-mean" = "A3604509L",
           "aus-cpi-monthly-original" = "A128478317T",
           "aus-cpi-monthly-seasonal" = "A128481587A",
           "aus-cpi-monthly-excl-volatile" = "A128473239F",
           "aus-lfi-original" = "A84423085A",
           "aus-lfi-seasonal" = "A84423043C",
           "aus-lfi-trend" = "A84423127L",
           "aus-wpi-original" = "A2603609J",
           "aus-wpi-seasonal" = "A2713849C",
           "aus-wpi-trend" = "A2713851R")
  if (is.null(ans)) {
    if (isTRUE(err_ifnotfound)) {
      stop("`name = ", name, "`, not found.") # nocov
    }
    return("")
  }
  ans
}

extdata_series_id <- function(series_id) {
  # Was originally the extdata of the package but this is now not allowed
  # in CRAN packages

  # tools::R_user_dir
  out <-
    file.path(R_user_dir(packageName(), which = "data"),
              paste0(series_id, ".tsv"))
  if (!file.exists(out)) {
    # Cannot provide an empty file
    provide.file(out)
    file.remove(out)
  }
  out
}

fread_extdata_series_id <- function(series_id) {
  if (!file.exists(extdata_series_id(series_id)) || !file.size(extdata_series_id(series_id))) {
    res <- download_data(series_id) # nocov
    if (sum(res, na.rm = TRUE)) {
      # message("download_data did not succeed.")
      return(data.table()) # nocov
    }
  }
  ans <- fread(extdata_series_id(series_id), sep = "\t")
  stopifnot(hasName(ans, "date"))
  stopifnot(hasName(ans, "value"))
  value <- NULL
  ans[, value := as.double(value)]
  ans[complete.cases(ans)]
}

file_splitter <- function(series_id) {
  series_id <- sub("^A", "", series_id)
  # "A2529212V.tsv" -> "5/29/21/2V/A2529212V.tsv"
  paste0(paste0(data.table::tstrsplit(series_id, split = "(?<=..)", perl = TRUE),
                collapse = "/"),
         "/A",
         series_id,
         ".tsv")
}



find_hughparsonage_abs_catalogue <- function(series_id) {
  paste0("https://github.com/HughParsonage/ABS-Catalogue/raw/master/data/series_id/A",
          file_splitter(series_id))
}

#' @rdname abs-conn
#' @export
download_data <- function(series_id = NULL) {
  if (is.null(series_id)) {
    # do everything
    series_id <- content2series_id()
  }


  ans <-
    sapply(series_id, function(sid) {
      if (!nzchar(sid)) {
        return(NA_integer_)
      }
      tempf <- tempfile(fileext = ".tsv")
      sid_url <- find_hughparsonage_abs_catalogue(sid)
      status <- tryCatch(download.file(sid_url, tempf, mode = "wb", quiet = TRUE),
                         error = function(e) {
                           message("download.file failed for url \n", sid_url, "\n",
                                   "Error message ", e$m)
                           1L
                         },
                         warning = function(e) {
                           message("download.file failed for url \n", sid_url, "\n",
                                   "Warning message ", e$m)
                           2L
                         })

      # nocov start
      if (status) {
        return(status)
      }
      copy_status <- file.copy(tempf, extdata_series_id(sid), overwrite = TRUE)
      file.remove(tempf)
      if (!copy_status) {
        message("File rename did not succeed", ".\n\t",
                "downloaded file: ", tempf, "\n\t",
                "intended destfile: ", extdata_series_id(sid))
        status <- 3L
      }
      return(as.integer(status))
      # nocov end
    })
  if (!sum(ans, na.rm = TRUE)) {
    saveRDS(Sys.Date(), date_last_updated.rds())
  }
  ans
}

date_last_updated.rds <- function() {
  file.path(R_user_dir(packageName(), which = "data"),
            "date_last_updated.rds")
}

#' @rdname abs-conn
#' @export
when_last_updated <- function() {
  if (!file.exists(date_last_updated.rds())) {
    return("Never updated") # nocov
  }
  return(readRDS(date_last_updated.rds()))
}

#' @rdname abs-conn
#' @export
grattanInflators_has_no_data <- function() {
  !file.exists(date_last_updated.rds()) ||
    !length(dir(tools::R_user_dir("grattanInflators", which = "data"),
                pattern = "\\.tsv$"))
}





