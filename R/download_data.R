

get_all_series_ids <- function() {
  c("aus-cpi-original" = "A2325846C",
    "aus-cpi-seasonal" = "A3604506F",
    "aus-cpi-trimmed-mean" = "A3604509L",
    "aus-lfi-original" = "A84423085A",
    "aus-lfi-seasonal" = "A84423043C",
    "aus-lfi-trend" = "A84423127L",
    "aus-wpi-original" = "A2603609J",
    "aus-wpi-seasonal" = "A2713849C",
    "aus-wpi-trend" = "A2713851R")
}



series_id_int <- function(series_id) {
  # convert to integer e.g. A5Z = 26 + 50
  vapply(strsplit(series_id, ""),
         function(x) {
           as.integer(sum(match(x[-1], c(1:9, LETTERS), nomatch = 0L) * 10^((length(x) - 2):0)))
         },
         0L)
}


content2series_id <- function(broad_cat = c("cpi", "lfi", "wpi"),
                              adjustment = c("original", "seasonal", "trend", "trimmed-mean")) {
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
           "aus-lfi-original" = "A84423085A",
           "aus-lfi-seasonal" = "A84423043C",
           "aus-lfi-trend" = "A84423127L",
           "aus-wpi-original" = "A2603609J",
           "aus-wpi-seasonal" = "A2713849C",
           "aus-wpi-trend" = "A2713851R")
  if (is.null(ans)) {
    if (isTRUE(err_ifnotfound)) {
      stop("`name = ", name, "`, not found.")
    }
    return("")
  }
  ans
}

extdata_series_id <- function(series_id) {
  # if (dir.exists("./inst/extdata")) {
  #   return(paste0("./inst/extdata/", paste0(series_id, ".tsv")))
  # }
  file.path(system.file("extdata",
                        package = packageName()),
            paste0(series_id, ".tsv"))
}

fread_extdata_series_id <- function(series_id) {
  if (!file.exists(extdata_series_id(series_id))) {
    download_data(series_id)
  }
  ans <- fread(extdata_series_id(series_id), sep = "\t")
  stopifnot(hasName(ans, "date"))
  stopifnot(hasName(ans, "value"))
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

download_data <- function(series_id = NULL) {
  if (is.null(series_id)) {
    # do everything
    series_id <- content2series_id()
  }

  sapply(series_id, function(sid) {
    if (!nzchar(sid)) {
      return(NULL)
    }
    tempf <- tempfile(fileext = ".tsv")
    sid_url <- find_hughparsonage_abs_catalogue(sid)
    status <- download.file(sid_url, tempf, mode = "wb", quiet = TRUE)
    if (status) {
      stop("status_id = ", sid, " had error status", status, ".\n",
           "URL = ", sid_url)
    }
    status <- file.rename(tempf, extdata_series_id(sid))
    if (!status) {
      stop("File rename did not succeed.\n\t",
           "downloaded file: ", tempf, "\n\t",
           "intended destfile: ", extdata_series_id(sid))
    }
  })
  saveRDS(Sys.Date(),
          file.path(system.file("extdata", package = packageName()),
                    "date_last_updated.rds"))

  invisible(0L)
}





