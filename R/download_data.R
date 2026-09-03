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
#' the string \code{"Never"} if the file does not exist. Note that this is the
#' date of \emph{retrieval}, not the ABS release the data came from: the mirror
#' tracks the ABS, so two sessions running the same package version at
#' different times may see different index histories.}
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

# Reads a two-column date/value TSV as downloaded from the ABS-Catalogue
# mirror. Errors if the file is not a usable index.
read_series_tsv <- function(path, strict = TRUE) {
  # Preserve the source tokens until malformed values have been distinguished
  # from the blank/NA boundary scaffolding present in some ABS series.
  ans <- fread(path, sep = "\t", colClasses = "character", na.strings = NULL)
  if (!hasName(ans, "date") || !hasName(ans, "value")) {
    stop("`", path, "` had columns ", toString(names(ans)),
         " but a series file must have columns `date` and `value`.")
  }
  ans <- ans[, c("date", "value"), with = FALSE]
  date <- .subset2(ans, "date")
  if (inherits(date, "Date") || inherits(date, "IDate")) {
    date <- as.IDate(date)
  } else if (is.character(date)) {
    date <- tryCatch(
      fast_as_idate(date, check = 2L),
      error = function(e) {
        stop("Downloaded series contains missing or unparseable observations.",
             call. = FALSE)
      }
    )
  } else {
    stop("Downloaded series contains an invalid `date` column.")
  }
  raw_value <- .subset2(ans, "value")
  trimmed_value <- trimws(raw_value)
  explicit_missing <- is.na(raw_value) |
    trimmed_value == "" |
    trimmed_value == "NA"
  value <- suppressWarnings(as.double(raw_value))
  malformed <- is.na(value) & !explicit_missing
  if (any(malformed)) {
    stop("Downloaded series contains nonnumeric values.")
  }
  if (anyNA(date)) {
    stop("Downloaded series contains missing or unparseable observations.")
  }
  if (anyNA(value)) {
    if (isTRUE(strict)) {
      stop("Downloaded series contains missing or unparseable observations.")
    }
    present <- which(!is.na(value))
    if (!length(present) || anyNA(value[present[1L]:present[length(present)]])) {
      stop("Downloaded series contains missing or unparseable observations.")
    }
    keep <- present[1L]:present[length(present)]
    date <- date[keep]
    value <- value[keep]
  }
  data.table(date = date, value = value)
}

read_cached_series <- function(path, series_id) {
  out <- read_series_tsv(path, strict = FALSE)
  validate_index(out, var = series_id)
  out
}

fread_extdata_series_id <- function(series_id) {
  path <- extdata_series_id(series_id)
  if (!file.exists(path) || !file.size(path)) {
    res <- download_data(series_id) # nocov
    if (sum(res, na.rm = TRUE)) {
      # message("download_data did not succeed.")
      return(data.table()) # nocov
    }
  }
  tryCatch(
    # Older cache files can contain a rectangular calendar scaffold with
    # unavailable leading/trailing values. Preserve compatibility with those
    # files, but still reject malformed dates and any missing interior value.
    read_cached_series(path, series_id),
    error = function(e) {
      stop("The cached file for series ", series_id,
           " is not a valid index and was not cached in memory. Run ",
           "download_data(\"", series_id, "\") to refresh it.\n\t",
           conditionMessage(e), call. = FALSE)
    }
  )
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
      destfile <- extdata_series_id(sid)
      # Download into the destination directory so that the final move is a
      # same-filesystem rename, i.e. atomic: a half-written or invalid file
      # must never become the cache.
      tempf <- tempfile(pattern = paste0(sid, "-"),
                        tmpdir = dirname(destfile),
                        fileext = ".tsv.tmp")
      on.exit(unlink(tempf), add = TRUE)
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
        return(as.integer(status))
      }
      # Validate the whole file before it is allowed to replace a usable cache.
      bad <- tryCatch({
        # ABS series can include unavailable leading/trailing calendar
        # scaffold rows. Trim only those boundary gaps; an interior missing
        # observation remains an error.
        validate_index(read_series_tsv(tempf, strict = FALSE), var = sid)
        NULL
      }, error = function(e) conditionMessage(e))
      if (!is.null(bad)) {
        message("The file downloaded for series ", sid,
                " is not a valid index, so the existing data has been kept.\n\t", bad)
        return(4L)
      }
      # keep the previous version, so a bad replacement can be undone
      if (file.exists(destfile)) {
        file.copy(destfile, paste0(destfile, ".bak"), overwrite = TRUE)
      }
      if (!file.rename(tempf, destfile) &&
          !file.copy(tempf, destfile, overwrite = TRUE)) {
        message("File rename did not succeed", ".\n\t",
                "downloaded file: ", tempf, "\n\t",
                "intended destfile: ", destfile)
        return(3L)
      }
      # The disk file has changed, so any copy already loaded in this session
      # is stale and must not be used again.
      RM_SERIES(sid)
      return(0L)
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
