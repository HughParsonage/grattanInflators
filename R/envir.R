#' Settings for the package environment
#' @description Reused data are stored a package environment,
#' which is visible to users as a package option `getOption("grattanInflators.env")`
#'
#' @param x The object to retrieve.
#' @noRd

GET_SERIES <- function(series_id) {
  GET(series_id, fread_extdata_series_id(series_id))
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


