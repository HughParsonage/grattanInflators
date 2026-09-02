#' Settings for the package environment
#' @description Reused data are stored a package environment,
#' which is visible to users as a package option `getOption("grattanInflators.env")`
#'
#' @param x The object to retrieve.
#' @noRd

GET_SERIES <- function(series_id) {
  GET(series_id, fread_extdata_series_id(series_id))
}

GET_SERIES_FY <- function(series_id) {
  nm <- paste0("fy", series_id)
  if (EXISTS(nm)) {
    return(get0(nm, envir = ENV(), inherits = FALSE))
  }
  value <- date <- NULL
  o <- data.table::setDTthreads(1L)
  on.exit(data.table::setDTthreads(o), add = TRUE)
  # copy(): GET_SERIES returns the cached table itself, which must not gain an
  # `fy` column by reference.
  ans <- copy(GET_SERIES(series_id))
  ans <- ans[, "fy" := fy::date2fy(date)][, list(value = mean(value)), keyby = "fy"]
  assign(nm, ans, envir = ENV())
  ans
}

GET <- function(x, value) {
  if (EXISTS(x)) {
    return(get0(x, envir = ENV(), inherits = FALSE))
  }
  assign(x, value, envir = ENV())
  value
}

EXISTS <- function(x) {
  exists(x, envir = ENV(), inherits = FALSE)
}

# Drop a series from the in-memory cache, so that a refreshed file on disk is
# actually read the next time the series is used.
RM_SERIES <- function(series_id) {
  nms <- intersect(c(series_id, paste0("fy", series_id)), ls(envir = ENV()))
  if (length(nms)) {
    rm(list = nms, envir = ENV())
  }
  invisible(NULL)
}

CLEAR_ENV <- function() {
  # nocov start
  rm(list = ls(envir = ENV()), envir = ENV())
  options("grattanInflators.env" = NULL)
  # nocov end
}

ENV <- function() {
  # nocov start
  if (is.null(getOption("grattanInflators.env"))) {
    options("grattanInflators.env" = new.env(parent = emptyenv()))
  }
  # nocov end
  getOption("grattanInflators.env")
}


