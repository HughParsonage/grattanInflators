# nocov start
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("grattanInflators.nThread"))) {
    options("grattanInflators.nThread" = 1L)
  }
  if (is.double(nThread <- getOption("grattanInflators.nThread"))) {
    options("grattanInflators.nThread" = as.integer(nThread))
  }
  if (is.null(getOption("grattanInflators.env"))) {
    options(grattanInflators.env = new.env(parent = emptyenv()))
  }

  invisible(NULL)
}

.onUnload <- function(libpath) {
  library.dynam.unload("grattanInflators", libpath)
}
# nocov end

